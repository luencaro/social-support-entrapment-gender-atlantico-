# ---------------------------------------------------------------------------
# elastic_net.R
#
# Elastic net como modelo 11, junto a los 10 modelos lineales jerarquicos.
# Este archivo NO ejecuta nada al hacer source().
#
# Un "ajuste" de elastic net es un procedimiento completo, tune_en():
#   1. Aprende el preprocesamiento con los datos recibidos y arma el diseno de
#      EN_FULL_MODEL. Todas las columnas se penalizan por igual.
#   2. Para cada alpha de EN_ALPHAS, ajusta el camino de lambdas.
#   3. Elige alpha y lambda con una validacion cruzada interna de
#      EN_INNER_FOLDS folds sobre esos mismos datos (preprocesamiento por fold)
#      y la regla de CV_SE_RULE EE: entre las combinaciones con RMSE dentro
#      del umbral, la que deja menos columnas distintas de cero.
#
# El mismo procedimiento se usa con los 530 casos (modelo final, en
# fit_models.ipynb) y dentro de cada fold de entrenamiento de la validacion
# cruzada externa (cross_validation.R), de modo que alpha y lambda nunca ven
# el fold de prueba.
#
# Requiere haber hecho source() de R/config.R, R/data_prep.R,
# R/moderation_pipeline.R y R/cross_validation.R antes.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(glmnet)
})


# ===========================================================================
# 1. Diseno
# ===========================================================================

#' Matriz de diseno del modelo completo.
#'
#' @param data muestra preprocesada (con columnas *_c y Gender_code).
#' @return lista con x (sin intercepto) e y.
build_en_design <- function(data,
                            formulas = build_formulas(),
                            full     = EN_FULL_MODEL) {
  list(
    x = model.matrix(formulas[[full]], data = data)[, -1, drop = FALSE],
    y = data[[OUTCOME_VAR]]
  )
}


# ===========================================================================
# 2. Caminos de lambda
# ===========================================================================

#' Ajusta el camino completo de lambdas para cada alpha.
#'
#' @param lambdas lista opcional nombrada por alpha con secuencias de lambda
#'   fijas (las del ajuste exterior, para que los folds internos evaluen los
#'   mismos valores).
#' @return lista nombrada por alpha con objetos glmnet.
fit_en_paths <- function(design, alphas = EN_ALPHAS, nlambda = EN_NLAMBDA,
                         lambdas = NULL) {
  map(set_names(alphas, as.character(alphas)), function(a) {
    if (is.null(lambdas)) {
      glmnet(design$x, design$y, alpha = a, nlambda = nlambda)
    } else {
      glmnet(design$x, design$y, alpha = a, lambda = lambdas[[as.character(a)]])
    }
  })
}


#' Columnas distintas de cero en cada punto de cada camino.
#'
#' @return tibble con alpha, lambda_idx, lambda, n_selected y selected
#'   (columnas separadas por " + ", vacio si ninguna).
en_path_selection <- function(paths, design) {
  cols <- colnames(design$x)

  imap_dfr(paths, function(fit, a) {
    nz <- as.matrix(fit$beta)[cols, , drop = FALSE] != 0
    tibble(
      alpha      = as.numeric(a),
      lambda_idx = seq_along(fit$lambda),
      lambda     = fit$lambda,
      n_selected = colSums(nz),
      selected   = apply(nz, 2, function(z) paste(cols[z], collapse = " + "))
    )
  })
}


# ===========================================================================
# 3. Eleccion de alpha y lambda
# ===========================================================================

#' RMSE de cada (alpha, lambda) en una validacion cruzada interna.
#'
#' Los folds se estratifican por CV_STRATA_VAR y el preprocesamiento se
#' aprende en cada fold interno de entrenamiento.
#'
#' @param data_raw datos limpios (sin centrar) con los que se ajusta.
#' @param paths    caminos ajustados con data_raw; sus lambdas se reutilizan.
en_inner_cv <- function(data_raw, paths,
                        k      = EN_INNER_FOLDS,
                        coding = GENDER_CODING) {
  lambdas <- map(paths, "lambda")
  fold    <- assign_stratified_folds(data_raw[[CV_STRATA_VAR]], k)

  map_dfr(seq_len(k), function(f) {
    train_raw <- data_raw[fold != f, ]
    test_raw  <- data_raw[fold == f, ]
    params <- learn_preprocessing(train_raw)
    train  <- build_en_design(apply_preprocessing(train_raw, params, coding))
    test   <- build_en_design(apply_preprocessing(test_raw,  params, coding))

    if (!identical(colnames(train$x), colnames(test$x))) {
      stop("Las columnas del diseno difieren entre folds internos.", call. = FALSE)
    }

    imap_dfr(fit_en_paths(train, lambdas = lambdas), function(fit, a) {
      pred <- predict(fit, newx = test$x)
      tibble(fold       = f,
             alpha      = as.numeric(a),
             lambda_idx = seq_len(ncol(pred)),
             rmse       = sqrt(colMeans((test$y - pred)^2)))
    })
  })
}


#' Aplica la regla de 1 EE a la validacion cruzada interna.
#'
#' Mejor: la combinacion con menor RMSE medio. Umbral: su RMSE + se_rule x
#' EE (sd entre folds / sqrt(k)). Elegida: entre las combinaciones dentro del
#' umbral, la que deja menos columnas distintas de cero; entre empatadas, la
#' de menor RMSE.
#'
#' @return lista con la fila elegida (choice), el resumen por combinacion y
#'   el umbral.
select_en <- function(inner_cv, selection, se_rule = CV_SE_RULE) {
  resumen <- inner_cv %>%
    group_by(alpha, lambda_idx) %>%
    summarise(rmse_mean = mean(rmse),
              rmse_se   = sd(rmse) / sqrt(n()),
              .groups = "drop") %>%
    inner_join(selection, by = c("alpha", "lambda_idx"))

  best      <- resumen %>% slice_min(rmse_mean, n = 1, with_ties = FALSE)
  threshold <- best$rmse_mean + se_rule * best$rmse_se

  choice <- resumen %>%
    filter(rmse_mean <= threshold) %>%
    arrange(n_selected, rmse_mean) %>%
    slice(1)

  list(choice = choice, summary = resumen, threshold = threshold)
}


# ===========================================================================
# 4. El modelo
# ===========================================================================

#' Ajusta elastic net eligiendo alpha y lambda dentro de `data_raw`.
#'
#' @param data_raw datos limpios (salida de build_clean_sample() o un fold de
#'   entrenamiento de ella).
#' @return objeto de clase "en_model": el glmnet del alpha elegido, alpha,
#'   lambda, columnas seleccionadas, parametros de preprocesamiento y el
#'   resumen de la validacion cruzada interna.
tune_en <- function(data_raw, coding = GENDER_CODING) {
  params <- learn_preprocessing(data_raw)
  design <- build_en_design(apply_preprocessing(data_raw, params, coding))
  paths  <- fit_en_paths(design)
  sel    <- select_en(en_inner_cv(data_raw, paths, coding = coding),
                      en_path_selection(paths, design))

  structure(
    list(
      fit        = paths[[as.character(sel$choice$alpha)]],
      alpha      = sel$choice$alpha,
      lambda     = sel$choice$lambda,
      selected   = if (sel$choice$selected == "") character(0)
                   else str_split_1(sel$choice$selected, " \\+ "),
      columns    = colnames(design$x),
      params     = params,
      coding     = coding,
      n          = nrow(data_raw),
      inner_cv   = sel$summary,
      threshold  = sel$threshold
    ),
    class = "en_model"
  )
}


#' Predice con un en_model sobre datos limpios (sin centrar).
#'
#' Aplica el preprocesamiento aprendido al ajustar, no uno nuevo.
predict_en <- function(model, newdata_raw) {
  design <- build_en_design(apply_preprocessing(newdata_raw, model$params, model$coding))
  if (!identical(colnames(design$x), model$columns)) {
    stop("Las columnas de los datos nuevos no coinciden con las del modelo.", call. = FALSE)
  }
  as.numeric(predict(model$fit, newx = design$x, s = model$lambda))
}


#' Coeficientes del modelo en el lambda elegido.
en_coefficients <- function(model) {
  b <- as.matrix(coef(model$fit, s = model$lambda))[, 1]
  tibble(term = names(b), estimate = unname(b))
}


#' R2 dentro de muestra: 1 - SSE / SST con las predicciones sobre data_raw.
en_r_squared <- function(model, data_raw) {
  y <- data_raw[[OUTCOME_VAR]]
  1 - sum((y - predict_en(model, data_raw))^2) / sum((y - mean(y))^2)
}


save_en_model <- function(model, path = PATH_EN_MODEL) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(model, path)
  invisible(path)
}


load_en_model <- function(path = PATH_EN_MODEL) {
  if (!file.exists(path)) {
    stop(sprintf("No existe '%s'. Ejecuta antes notebooks/fit_models.ipynb.", path),
         call. = FALSE)
  }
  readRDS(path)
}


# ===========================================================================
# 5. Ubicacion respecto a los modelos jerarquicos
# ===========================================================================

#' Resume una seleccion por termino de la formula en vez de por columna.
#'
#' Un factor como Municipality ocupa varias columnas (una dummy por nivel no
#' referencia); elastic net puede quedarse con todas, con algunas o con
#' ninguna.
#'
#' @return tibble con term, n_columns, n_selected y status
#'   ("completo", "parcial" o "fuera").
en_term_summary <- function(selected, data,
                            formulas = build_formulas(),
                            full     = EN_FULL_MODEL) {
  mm     <- model.matrix(formulas[[full]], data = data)
  labels <- attr(terms(formulas[[full]]), "term.labels")

  tibble(term   = labels[attr(mm, "assign")[-1]],
         column = colnames(mm)[-1]) %>%
    mutate(term = factor(term, levels = labels)) %>%
    group_by(term) %>%
    summarise(n_columns  = n(),
              n_selected = sum(column %in% selected),
              .groups = "drop") %>%
    mutate(status = case_when(n_selected == 0         ~ "fuera",
                              n_selected == n_columns ~ "completo",
                              TRUE                    ~ "parcial"))
}


#' Compara un conjunto de columnas con el de cada modelo jerarquico.
#'
#' @param selected vector de columnas distintas de cero.
#' @return tibble con una fila por modelo: las columnas que le faltan a la
#'   seleccion, las que sobran, si coincide exactamente (matches) y si la
#'   seleccion es un subconjunto de sus columnas (contains).
match_hierarchical <- function(selected, data, formulas = build_formulas()) {
  imap_dfr(formulas, function(fm, nm) {
    cols_m <- colnames(model.matrix(fm, data = data))[-1]
    tibble(
      model    = nm,
      n_cols   = length(cols_m),
      missing  = paste(setdiff(cols_m, selected), collapse = " + "),
      extra    = paste(setdiff(selected, cols_m), collapse = " + "),
      matches  = setequal(cols_m, selected),
      contains = all(selected %in% cols_m)
    )
  })
}


#' Ubica una seleccion respecto a los modelos jerarquicos.
#'
#'   - "igual": coincide exactamente con un modelo.
#'   - "subconjunto": no coincide con ninguno, pero todas sus columnas estan
#'     en al menos un modelo; se reporta el mas pequeno que la contiene.
#'   - "ninguno": ningun modelo contiene todas sus columnas.
#'
#' @param match salida de match_hierarchical().
#' @return tibble de una fila con relation, model, contained_in y left_out.
locate_hierarchical <- function(match) {
  igual <- match %>% filter(matches)
  if (nrow(igual) > 0) {
    return(tibble(relation = "igual", model = igual$model[1],
                  contained_in = paste(match$model[match$contains], collapse = ", "),
                  left_out = ""))
  }

  contienen <- match %>% filter(contains) %>% arrange(n_cols)
  if (nrow(contienen) == 0) {
    return(tibble(relation = "ninguno", model = NA_character_,
                  contained_in = "", left_out = ""))
  }

  tibble(relation     = "subconjunto",
         model        = contienen$model[1],
         contained_in = paste(contienen$model, collapse = ", "),
         left_out     = contienen$missing[1])
}
