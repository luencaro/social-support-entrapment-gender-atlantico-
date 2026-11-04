# ---------------------------------------------------------------------------
# imputed_pipeline.R
#
# Pipeline de fit_models.ipynb + model_comparison.ipynb sobre las bases de la
# imputacion multiple (notebooks/imputacion.ipynb). Este archivo NO ejecuta
# nada al hacer source(): la orquestacion vive en
# notebooks/imputed_pipeline.ipynb.
#
# Cada paso del pipeline se corre por separado en cada una de las m bases y
# los resultados se combinan:
#   - coeficientes y EE de los lm(): reglas de Rubin;
#   - R2, logLik, AIC, BIC, delta R2, f2, delta AIC / BIC: promedio entre
#     bases;
#   - RMSE y R2 de la validacion cruzada: promedio entre bases, con un EE que
#     suma la varianza dentro de cada base y la varianza entre bases (Rubin);
#   - supuestos y diagnostico: en cuantas bases se cumple cada supuesto y
#     promedio de las medidas de diagnostico.
#
# Requiere haber hecho source() de R/config.R, R/data_prep.R,
# R/moderation_pipeline.R, R/model_comparison.R, R/cross_validation.R y
# R/elastic_net.R antes.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
})


# ===========================================================================
# 1. Bases imputadas
# ===========================================================================

#' Carga la lista de bases guardada por notebooks/imputacion.ipynb.
#'
#' Cada base tiene el formato de build_clean_sample(): factores con los
#' niveles de FACTOR_LEVELS, SSE colapsado y apoyo sin centrar.
load_imputed_datasets <- function(path = PATH_IMPUTED_DATA) {
  if (!file.exists(path)) {
    stop(sprintf("No existe '%s'. Ejecuta antes notebooks/imputacion.ipynb.", path),
         call. = FALSE)
  }
  bases <- readRDS(path)

  n <- unique(map_int(bases, nrow))
  if (length(n) != 1L) stop("Las bases imputadas no tienen el mismo N.", call. = FALSE)
  if (!all(map_lgl(bases, ~ all(complete.cases(.x[MODEL_VARS]))))) {
    stop("Alguna base imputada tiene faltantes en MODEL_VARS.", call. = FALSE)
  }
  bases
}


#' Filas con al menos un valor imputado.
#'
#' Un valor observado es igual en todas las bases; uno imputado cambia de
#' una base a otra.
imputed_rows <- function(bases, vars = MODEL_VARS) {
  varia <- map(vars, function(v) {
    m <- map(bases, ~ as.character(.x[[v]]))
    reduce(m[-1], function(acc, x) acc | x != m[[1]], .init = rep(FALSE, length(m[[1]])))
  })
  reduce(varia, `|`)
}


#' Parametros de preprocesamiento comunes a todas las bases.
#'
#' Niveles y referencias se aprenden de la primera base (son los mismos en
#' todas); las grandes medias son el promedio de las medias de las m bases.
#' Con un mismo centrado, los coeficientes significan lo mismo en todas las
#' bases y pueden combinarse con las reglas de Rubin.
learn_mi_preprocessing <- function(bases) {
  params <- learn_preprocessing(bases[[1]])

  refs <- map(bases, ~ learn_preprocessing(.x)$reference_levels)
  if (!all(map_lgl(refs, identical, refs[[1]]))) {
    stop("Los niveles de referencia difieren entre bases imputadas.", call. = FALSE)
  }

  params$grand_means <- map_dfr(bases, ~ map_dbl(set_names(SUPPORT_VARS), function(v) mean(.x[[v]]))) %>%
    colMeans()
  params
}


#' Aplica el mismo preprocesamiento a cada base.
prepare_mi <- function(bases, params, coding = GENDER_CODING) {
  map(bases, apply_preprocessing, params = params, coding = coding)
}


# ===========================================================================
# 2. Modelos lineales
# ===========================================================================

#' Ajusta las formulas en cada base: lista (una por base) de listas de lm().
fit_mi_models <- function(analytic_bases, formulas = build_formulas()) {
  map(analytic_bases, ~ fit_models(formulas = formulas, data = .x))
}


#' Reglas de Rubin para un vector de estimaciones y sus EE.
#'
#' Q = media de las estimaciones; U = varianza media dentro de las bases;
#' B = varianza entre bases; T = U + (1 + 1/m) B. lambda es la proporcion de
#' la varianza total que se debe a los faltantes.
rubin <- function(estimate, se) {
  m <- length(estimate)
  U <- mean(se^2)
  B <- var(estimate)
  T <- U + (1 + 1 / m) * B
  tibble(estimate = mean(estimate), std_error = sqrt(T),
         se_within = sqrt(U), lambda = (1 + 1 / m) * B / T)
}


#' Coeficientes combinados de cada modelo, sin estadistico ni p-valor.
pool_mi_coefficients <- function(mi_models) {
  imap_dfr(mi_models, ~ tidy_coefficients(.x) %>% mutate(imp = .y)) %>%
    group_by(model, term) %>%
    summarise(rubin(estimate, std_error), .groups = "drop") %>%
    mutate(model = factor(model, levels = names(mi_models[[1]])),
           term  = fct_inorder(term)) %>%
    arrange(model, term) %>%
    mutate(across(c(model, term), as.character))
}


#' Indices de ajuste promediados entre bases.
#'
#' delta AIC / BIC y los pesos de Akaike se recalculan sobre los promedios.
mi_fit_indices <- function(mi_models) {
  imap_dfr(mi_models, ~ fit_indices(.x) %>% mutate(imp = .y)) %>%
    group_by(model) %>%
    summarise(n             = first(n),
              k             = first(k),
              r_squared     = mean(r_squared),
              adj_r_squared = mean(adj_r_squared),
              log_lik       = mean(log_lik),
              aic_sd        = sd(aic),
              aic           = mean(aic),
              bic           = mean(bic),
              .groups = "drop") %>%
    mutate(model = factor(model, levels = names(mi_models[[1]]))) %>%
    arrange(model) %>%
    mutate(model = as.character(model),
           delta_aic = aic - min(aic),
           delta_bic = bic - min(bic),
           akaike_weight = exp(-delta_aic / 2) / sum(exp(-delta_aic / 2))) %>%
    relocate(aic_sd, .after = aic)
}


#' Comparaciones anidadas promediadas entre bases.
mi_compare_nested <- function(mi_models, comparisons = build_comparisons()) {
  promedios <- imap_dfr(mi_models, ~ compare_nested(.x, comparisons) %>% mutate(imp = .y)) %>%
    group_by(reduced, full, block) %>%
    summarise(df_added     = first(df_added),
              delta_r2     = mean(delta_r2),
              cohen_f2     = mean(cohen_f2),
              # Bases en que AIC favorece al modelo completo (delta < 0).
              n_aic_favors = sum(delta_aic < 0),
              delta_aic_sd = sd(delta_aic),
              delta_aic    = mean(delta_aic),
              delta_bic    = mean(delta_bic),
              .groups = "drop")

  # left_join desde `comparisons` para conservar el orden de la secuencia.
  comparisons %>%
    left_join(promedios, by = c("reduced", "full", "block")) %>%
    relocate(delta_aic_sd, .after = delta_aic) %>%
    relocate(n_aic_favors, .after = delta_bic)
}


# ===========================================================================
# 3. Elastic net
# ===========================================================================

#' Ajusta elastic net en cada base (alpha y lambda elegidos en cada una).
fit_mi_en <- function(bases, coding = GENDER_CODING, seed = SEED) {
  map(bases, function(b) {
    set.seed(seed)
    tune_en(b, coding = coding)
  })
}


#' Resumen de elastic net entre bases: por columna, en cuantas bases queda
#' distinta de cero y su coeficiente medio.
mi_en_summary <- function(en_models) {
  imap_dfr(en_models, ~ en_coefficients(.x) %>% mutate(imp = .y)) %>%
    filter(term != "(Intercept)") %>%
    group_by(term) %>%
    summarise(n_selected    = sum(estimate != 0),
              mean_estimate = mean(estimate),
              .groups = "drop") %>%
    mutate(term = factor(term, levels = en_models[[1]]$columns)) %>%
    arrange(term) %>%
    mutate(term = as.character(term))
}


# ===========================================================================
# 4. Validacion cruzada
# ===========================================================================

#' Validacion cruzada repetida en cada base, en paralelo.
#'
#' Las m bases tienen las mismas filas en el mismo orden y el mismo genero,
#' asi que make_cv_folds() con la misma semilla da las mismas particiones en
#' todas: la comparacion entre modelos es pareada dentro y entre bases.
run_mi_cv <- function(bases, cores = max(1L, parallel::detectCores() - 2L),
                      formulas = build_formulas()) {
  res <- parallel::mclapply(seq_along(bases), function(i) {
    folds <- make_cv_folds(bases[[i]])
    run_repeated_cv(bases[[i]], folds, formulas = formulas) %>% mutate(imp = i, .before = 1)
  }, mc.cores = cores, mc.preschedule = FALSE)

  fallidas <- map_lgl(res, inherits, "try-error")
  if (any(fallidas)) {
    stop(sprintf("La validacion cruzada fallo en las bases: %s\n%s",
                 paste(which(fallidas), collapse = ", "), res[[which(fallidas)[1]]]),
         call. = FALSE)
  }
  bind_rows(res) %>%
    mutate(model = factor(model, levels = model_levels(formulas)))
}


#' Combina una metrica de CV entre bases.
#'
#' La media de cada base con su EE (cv_se) se combina con las reglas de
#' Rubin: EE total = sqrt(media EE^2 + (1 + 1/m) var(medias)).
pool_cv_metric <- function(mean_by_imp, se_by_imp) {
  r <- rubin(mean_by_imp, se_by_imp)
  tibble(mean = r$estimate, se = r$std_error)
}


#' Resumen de la validacion cruzada combinada entre bases, con la regla de
#' `se_rule` EE sobre el RMSE combinado.
mi_summarise_cv <- function(cv_all, se_rule = CV_SE_RULE) {
  por_base <- cv_all %>%
    group_by(imp, model) %>%
    # Los EE van antes que las medias: summarise() evalua en orden y, si no,
    # cv_se() recibiria la media ya resumida en lugar de los folds.
    summarise(n_coef  = mean(n_coef),
              rmse_se = cv_se(rmse, repeat_id),
              r2_se   = cv_se(r2_oos, repeat_id),
              rmse    = mean(rmse),
              r2      = mean(r2_oos),
              .groups = "drop")

  resumen <- por_base %>%
    group_by(model) %>%
    summarise(n_coef = mean(n_coef),
              pool_cv_metric(rmse, rmse_se) %>% rename(rmse_mean = mean, rmse_se = se),
              pool_cv_metric(r2, r2_se)     %>% rename(r2_mean = mean, r2_se = se),
              .groups = "drop")

  best      <- resumen %>% slice_min(rmse_mean, n = 1, with_ties = FALSE)
  threshold <- best$rmse_mean + se_rule * best$rmse_se

  pareado <- cv_all %>%
    group_by(imp) %>%
    group_modify(~ paired_delta(.x, reference = best$model, metric = "rmse")) %>%
    group_by(model) %>%
    summarise(pool_cv_metric(delta, delta_se) %>%
                rename(delta_rmse = mean, delta_rmse_se = se),
              .groups = "drop")

  resumen %>%
    left_join(pareado, by = "model") %>%
    mutate(is_best = model == best$model,
           within_se_rule = rmse_mean <= threshold) %>%
    structure(threshold = threshold, se_rule = se_rule,
              best = as.character(best$model))
}


# ===========================================================================
# 5. Supuestos y diagnostico
# ===========================================================================

#' Pruebas de supuestos en cada base.
mi_assumption_tests <- function(mi_models, en_models, bases) {
  map_dfr(seq_along(mi_models), function(i) {
    assumption_tests(mi_models[[i]], en_models[[i]], bases[[i]]) %>% mutate(imp = i, .before = 1)
  }) %>%
    mutate(model = fct_inorder(model))
}


#' En cuantas bases se cumple cada supuesto, por modelo, y la mediana de su
#' p-valor (o del VIF maximo).
summarise_mi_assumptions <- function(mi_assumptions) {
  mi_assumptions %>%
    group_by(model) %>%
    summarise(m            = n(),
              normality    = sum(normality_ok),    shapiro_p = median(shapiro_p),
              homosced     = sum(homosced_ok),     bp_p      = median(bp_p),
              independence = sum(independence_ok), dw_p      = median(dw_p),
              linearity    = sum(linearity_ok),    reset_p   = median(reset_p),
              collinearity = sum(collinearity_ok), vif_max   = median(vif_max),
              all_met      = sum(all_met),
              .groups = "drop")
}


#' Diagnostico por observacion en cada base (apilado, con columna imp).
mi_model_diagnostics <- function(mi_models, en_models, bases) {
  map_dfr(seq_along(mi_models), function(i) {
    model_diagnostics(mi_models[[i]], en_models[[i]], bases[[i]]) %>% mutate(imp = i, .before = 1)
  }) %>%
    mutate(model = fct_inorder(as.character(model)))
}


#' Promedio entre bases del resumen de diagnostico de cada modelo.
summarise_mi_diagnostics <- function(mi_diag) {
  mi_diag %>%
    group_by(imp) %>%
    group_modify(~ diagnostics_summary(.x)) %>%
    group_by(model) %>%
    summarise(across(c(n, p), first),
              across(-c(imp, n, p), mean),
              .groups = "drop")
}


#' Observaciones influyentes (Cook > 4/n en al menos min_models lm()), y en
#' cuantas bases lo son.
mi_influential_observations <- function(mi_diag, analytic_bases, imputed,
                                        min_models = 8) {
  map_dfr(seq_along(analytic_bases), function(i) {
    influential_observations(filter(mi_diag, imp == i), analytic_bases[[i]],
                             min_models = min_models) %>%
      select(obs, max_cooks, max_abs_t, max_lev) %>%
      mutate(imp = i)
  }) %>%
    group_by(obs) %>%
    summarise(n_bases   = n(),
              max_cooks = max(max_cooks),
              max_abs_t = max(max_abs_t),
              max_lev   = max(max_lev),
              .groups = "drop") %>%
    mutate(imputed = imputed[obs]) %>%
    arrange(desc(n_bases), desc(max_cooks))
}


# ===========================================================================
# 6. Guardado
# ===========================================================================

save_mi_outputs <- function(tables, models = NULL) {
  paths <- c(coefficients = PATH_MI_COEFS, indices = PATH_MI_INDICES,
             nested = PATH_MI_NESTED, cv_folds = PATH_MI_CV_FOLDS,
             cv_summary = PATH_MI_CV_SUMMARY, assumptions = PATH_MI_ASSUMPTIONS,
             diagnostics = PATH_MI_DIAGNOSTICS)
  paths <- paths[names(tables)]
  walk(unique(dirname(c(paths, PATH_MI_MODELS))),
       ~ dir.create(.x, showWarnings = FALSE, recursive = TRUE))

  iwalk(tables, ~ write_csv(.x, paths[[.y]]))
  if (!is.null(models)) {
    saveRDS(models, PATH_MI_MODELS)
    paths <- c(paths, models = PATH_MI_MODELS)
  }
  invisible(unname(paths))
}
