# ---------------------------------------------------------------------------
# cross_validation.R
#
# Definiciones de funciones para comparar los 10 modelos jerarquicos y
# elastic net (modelo 11) por su capacidad predictiva fuera de muestra. Este archivo NO ejecuta nada al hacer
# source(). La orquestacion esta repartida en dos notebooks:
#   - notebooks/fit_models.ipynb corre la validacion cruzada y guarda las
#     metricas por fold (PATH_CV_FOLDS);
#   - notebooks/model_comparison.ipynb las carga, aplica la regla de 1 EE
#     (sin elegir un modelo) y guarda el resumen (PATH_CV_SUMMARY).
#
# Esquema:
#   - K-fold repetida (CV_FOLDS x CV_REPEATS), estratificada por CV_STRATA_VAR.
#   - Los 11 modelos usan exactamente los mismos folds (comparacion pareada).
#   - Dentro de cada fold, el preprocesamiento que depende de los datos
#     (grandes medias y nivel de referencia "most_frequent") se aprende solo
#     con el entrenamiento y se aplica al fold de prueba.
#   - Elastic net elige alpha y lambda dentro de cada fold de entrenamiento,
#     con su propia validacion cruzada interna (tune_en, elastic_net.R).
#   - Metricas por fold: RMSE, MAE y R2 fuera de muestra.
#   - Regla de CV_SE_RULE errores estandar: reporta los modelos que la
#     cumplen, sin elegir uno.
#
# Requiere haber hecho source() de R/config.R, R/data_prep.R (limpieza y
# preprocesamiento), R/moderation_pipeline.R (build_formulas) y
# R/elastic_net.R (tune_en, predict_en) antes.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
})


# ===========================================================================
# 1. Folds
# ===========================================================================

#' Asigna cada fila a uno de k folds, estratificando por `strata`.
#'
#' Dentro de cada estrato las filas se reparten en orden aleatorio y ciclico,
#' de modo que cada fold conserva la proporcion de cada estrato.
assign_stratified_folds <- function(strata, k = CV_FOLDS) {
  fold <- integer(length(strata))
  for (s in unique(strata)) {
    idx <- which(strata == s)
    fold[idx] <- sample(rep_len(seq_len(k), length(idx)))
  }
  fold
}


#' Genera las asignaciones de folds de todas las repeticiones.
#'
#' @return tibble largo con columnas repeat_id, row_id y fold.
make_cv_folds <- function(data,
                          k          = CV_FOLDS,
                          repeats    = CV_REPEATS,
                          strata_var = CV_STRATA_VAR,
                          seed       = SEED) {
  set.seed(seed)
  map_dfr(seq_len(repeats), function(r) {
    tibble(repeat_id = r,
           row_id    = seq_len(nrow(data)),
           fold      = assign_stratified_folds(data[[strata_var]], k))
  })
}


# ===========================================================================
# 2. Un fold
# ===========================================================================

#' Nombres de los modelos en el orden de las tablas: los 10 lm() y elastic net.
model_levels <- function(formulas = build_formulas(), include_en = TRUE) {
  c(names(formulas), if (include_en) EN_MODEL_NAME)
}


#' Metricas de prediccion de un vector de errores.
fold_metrics <- function(err, y_test, mean_train) {
  tibble(rmse   = sqrt(mean(err^2)),
         mae    = mean(abs(err)),
         r2_oos = 1 - sum(err^2) / sum((y_test - mean_train)^2))
}


#' Ajusta todos los modelos en el entrenamiento y los evalua en la prueba.
#'
#' El preprocesamiento se aprende solo con `train_raw`. El R2 fuera de muestra
#' usa la media del entrenamiento como prediccion de referencia: es lo que
#' predeciria M0 sin ver los datos de prueba.
#'
#' Con include_en = TRUE agrega elastic net: tune_en() elige alpha y lambda
#' con `train_raw` y n_coef cuenta sus columnas distintas de cero mas el
#' intercepto.
evaluate_fold <- function(train_raw, test_raw, formulas = build_formulas(),
                          coding = GENDER_CODING, include_en = TRUE) {
  params <- learn_preprocessing(train_raw)
  train  <- apply_preprocessing(train_raw, params, coding = coding)
  test   <- apply_preprocessing(test_raw, params, coding = coding)

  y_test     <- test[[OUTCOME_VAR]]
  mean_train <- mean(train[[OUTCOME_VAR]])

  lm_rows <- imap_dfr(formulas, function(fm, nm) {
    fit <- lm(fm, data = train)
    err <- y_test - predict(fit, newdata = test)
    bind_cols(tibble(model = nm, n_coef = length(coef(fit))),
              fold_metrics(err, y_test, mean_train))
  })

  if (!include_en) return(lm_rows)

  en  <- tune_en(train_raw, coding = coding)
  err <- y_test - predict_en(en, test_raw)
  bind_rows(lm_rows,
            bind_cols(tibble(model = EN_MODEL_NAME, n_coef = length(en$selected) + 1L),
                      fold_metrics(err, y_test, mean_train)))
}


# ===========================================================================
# 3. Validacion cruzada repetida
# ===========================================================================

#' Corre la K-fold repetida sobre la muestra limpia (sin centrar).
#'
#' @param data  salida de build_clean_sample(): limpieza fila a fila hecha,
#'              preprocesamiento dependiente de los datos sin hacer.
#' @param folds salida de make_cv_folds().
#' @param seed  fija los folds internos de elastic net; los folds externos ya
#'              vienen fijados en `folds`.
#' @return tibble con una fila por repeticion x fold x modelo.
run_repeated_cv <- function(data       = build_clean_sample(),
                            folds      = make_cv_folds(data),
                            formulas   = build_formulas(),
                            coding     = GENDER_CODING,
                            include_en = TRUE,
                            seed       = SEED) {
  set.seed(seed)
  folds %>%
    group_by(repeat_id, fold) %>%
    group_modify(function(f, key) {
      test_rows <- f$row_id
      evaluate_fold(data[-test_rows, ], data[test_rows, ],
                    formulas = formulas, coding = coding,
                    include_en = include_en) %>%
        mutate(n_test = length(test_rows), .before = 1)
    }) %>%
    ungroup() %>%
    mutate(model = factor(model, levels = model_levels(formulas, include_en)))
}


# ===========================================================================
# 4. Resumen y seleccion
# ===========================================================================

#' Error estandar de una metrica de validacion cruzada repetida.
#'
#' En cada repeticion se calcula el EE de K-fold, sd(folds) / sqrt(K), y se
#' promedia entre repeticiones. Promediar las repeticiones reduce la varianza
#' de la estimacion, pero los folds de distintas repeticiones comparten
#' observaciones: dividir por sqrt(K x repeticiones) subestimaria el EE.
cv_se <- function(x, repeat_id) {
  tibble(x = x, r = repeat_id) %>%
    group_by(r) %>%
    summarise(se = sd(x) / sqrt(n()), .groups = "drop") %>%
    pull(se) %>%
    mean()
}


#' Diferencia pareada de una metrica entre cada modelo y uno de referencia.
#'
#' Se calcula fold a fold sobre las mismas particiones (modelo - referencia)
#' y se resume con su media y su EE (cv_se).
paired_delta <- function(cv_results, reference, metric = "rmse") {
  cv_results %>%
    select(repeat_id, fold, model, value = all_of(metric)) %>%
    left_join(cv_results %>%
                filter(model == reference) %>%
                select(repeat_id, fold, ref = all_of(metric)),
              by = c("repeat_id", "fold")) %>%
    group_by(model) %>%
    summarise(delta    = mean(value - ref),
              delta_se = cv_se(value - ref, repeat_id),
              .groups = "drop")
}


#' Resume las metricas por modelo y aplica la regla de `se_rule` EE.
#'
#' Ademas del RMSE medio, reporta la diferencia pareada con el mejor modelo
#' (mismos folds) y su EE, que es mas precisa que comparar dos medias sueltas.
#'
#' La regla: el mejor modelo es el de menor RMSE medio; el umbral es su RMSE
#' + se_rule x su EE. Se marcan los modelos cuyo RMSE medio no supera el
#' umbral; no se elige uno.
#'
#' n_coef es el promedio entre folds: fijo en los lm(), variable en elastic
#' net porque sus columnas se eligen en cada fold.
summarise_cv <- function(cv_results, se_rule = CV_SE_RULE) {
  resumen <- cv_results %>%
    group_by(model) %>%
    summarise(
      n_coef    = mean(n_coef),
      rmse_mean = mean(rmse),
      rmse_se   = cv_se(rmse, repeat_id),
      mae_mean  = mean(mae),
      mae_se    = cv_se(mae, repeat_id),
      r2_mean   = mean(r2_oos),
      r2_se     = cv_se(r2_oos, repeat_id),
      .groups = "drop"
    )

  best      <- resumen %>% slice_min(rmse_mean, n = 1, with_ties = FALSE)
  threshold <- best$rmse_mean + se_rule * best$rmse_se

  pareado <- paired_delta(cv_results, reference = best$model, metric = "rmse") %>%
    rename(delta_rmse = delta, delta_rmse_se = delta_se)

  resumen %>%
    left_join(pareado, by = "model") %>%
    mutate(
      is_best        = model == best$model,
      within_se_rule = rmse_mean <= threshold
    ) %>%
    structure(threshold = threshold, se_rule = se_rule,
              best = as.character(best$model))
}


# ===========================================================================
# 5. Guardado
# ===========================================================================

#' Escribe las metricas por fold en outputs/tables/.
save_cv_results <- function(cv_results, path = PATH_CV_FOLDS) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write_csv(cv_results, path)
  invisible(path)
}


#' Relee las metricas por fold guardadas por save_cv_results().
#'
#' Restaura el orden de los modelos segun model_levels().
load_cv_results <- function(path = PATH_CV_FOLDS) {
  if (!file.exists(path)) {
    stop(sprintf("No existe '%s'. Ejecuta antes notebooks/fit_models.ipynb.", path),
         call. = FALSE)
  }
  read_csv(path, show_col_types = FALSE) %>%
    mutate(model = factor(model, levels = model_levels()))
}


#' Escribe el resumen por modelo en outputs/tables/.
save_cv_summary <- function(cv_summary, path = PATH_CV_SUMMARY) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write_csv(cv_summary, path)
  invisible(path)
}
