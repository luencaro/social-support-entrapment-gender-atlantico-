# ---------------------------------------------------------------------------
# model_comparison.R
#
# Definiciones de funciones para comparar los 10 modelos lineales jerarquicos
# ajustados por moderation_pipeline.R. Este archivo NO ejecuta nada al hacer
# source(): la orquestacion vive en notebooks/model_comparison.ipynb.
#
# Alcance: cargar outputs/models/fitted_models.rds y reportar
#   - indices de ajuste por modelo: R2, R2 ajustado, logLik, AIC y BIC;
#   - comparaciones entre modelos anidados: delta R2, f2 de Cohen y
#     delta AIC / BIC (sin pruebas de hipotesis);
#   - supuestos del modelo lineal de los 10 lm() y de elastic net (lmtest y
#     car);
#   - diagnostico por observacion: residuos, leverage, distancia de Cook.
#
# Requiere haber hecho source("R/config.R") antes. Para los supuestos y el
# diagnostico de elastic net, tambien R/data_prep.R, R/moderation_pipeline.R
# y R/elastic_net.R. lmtest y car se usan con prefijo.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
})


# ===========================================================================
# 1. Modelos ajustados
# ===========================================================================

#' Carga la lista de modelos guardada por save_model_outputs().
load_fitted_models <- function(path = PATH_FITTED_MODELS) {
  if (!file.exists(path)) {
    stop(sprintf("No existe '%s'. Ejecuta antes notebooks/fit_models.ipynb.", path),
         call. = FALSE)
  }
  models <- readRDS(path)

  n_obs <- unique(map_int(models, nobs))
  if (length(n_obs) != 1L) {
    stop("Los modelos no usan el mismo N: las comparaciones no serian validas.",
         call. = FALSE)
  }
  models
}


# ===========================================================================
# 2. Secuencia de comparaciones
# ===========================================================================

#' Construye la lista de comparaciones anidadas desde config.R.
#'
#' Cada fila es un paso de la secuencia jerarquica: el modelo reducido, el
#' completo y el bloque de terminos que se agrega.
#'
#'   M0       -> M1        genero + covariables
#'   M1       -> M2_D      efecto principal de la dimension D
#'   M2_D     -> M3_D      interaccion D x genero
#'   M1       -> M2_joint  las tres dimensiones
#'   M2_joint -> M3_joint  las tres interacciones
build_comparisons <- function(support_vars = SUPPORT_VARS) {
  steps <- list(tibble(reduced = "M0", full = "M1",
                       block = "Género + covariables"))

  for (d in support_vars) {
    steps <- c(steps, list(
      tibble(reduced = "M1", full = paste0("M2_", d),
             block = sprintf("Efecto principal: %s", d)),
      tibble(reduced = paste0("M2_", d), full = paste0("M3_", d),
             block = sprintf("Interacción: %s × género", d))
    ))
  }

  steps <- c(steps, list(
    tibble(reduced = "M1", full = "M2_joint",
           block = "Efectos principales: las tres dimensiones"),
    tibble(reduced = "M2_joint", full = "M3_joint",
           block = "Interacciones: las tres dimensiones × género")
  ))

  bind_rows(steps)
}


# ===========================================================================
# 3. Indices de ajuste por modelo
# ===========================================================================

#' R2, logLik, AIC y BIC de cada modelo.
#'
#' k cuenta los parametros de la verosimilitud: coeficientes + sigma.
fit_indices <- function(models) {
  imap_dfr(models, function(fit, nm) {
    s  <- summary(fit)
    ll <- logLik(fit)
    tibble(
      model         = nm,
      n             = nobs(fit),
      k             = attr(ll, "df"),
      r_squared     = s$r.squared,
      adj_r_squared = s$adj.r.squared,
      log_lik       = as.numeric(ll),
      aic           = AIC(fit),
      bic           = BIC(fit)
    )
  }) %>%
    mutate(delta_aic = aic - min(aic),
           delta_bic = bic - min(bic),
           # Pesos de Akaike: probabilidad relativa de cada modelo en el conjunto.
           akaike_weight = exp(-delta_aic / 2) / sum(exp(-delta_aic / 2)))
}


# ===========================================================================
# 4. Comparaciones anidadas
# ===========================================================================

#' Compara un par de modelos anidados.
compare_pair <- function(reduced, full) {
  if (!all(names(coef(reduced)) %in% names(coef(full)))) {
    stop("El modelo reducido no esta anidado en el completo.", call. = FALSE)
  }

  r2_red  <- summary(reduced)$r.squared
  r2_full <- summary(full)$r.squared

  tibble(
    df_added   = length(coef(full)) - length(coef(reduced)),
    r2_reduced = r2_red,
    r2_full    = r2_full,
    delta_r2   = r2_full - r2_red,
    # f2 de Cohen del bloque: delta R2 / (1 - R2 del completo).
    cohen_f2   = (r2_full - r2_red) / (1 - r2_full),
    delta_aic  = AIC(full) - AIC(reduced),
    delta_bic  = BIC(full) - BIC(reduced)
  )
}


#' Aplica compare_pair() a toda la secuencia.
#'
#' delta_aic y delta_bic son completo - reducido: negativos favorecen al
#' modelo completo.
compare_nested <- function(models, comparisons = build_comparisons()) {
  faltan <- setdiff(c(comparisons$reduced, comparisons$full), names(models))
  if (length(faltan) > 0) {
    stop(sprintf("Faltan modelos en la lista: %s", paste(faltan, collapse = ", ")),
         call. = FALSE)
  }

  comparisons %>%
    mutate(res = map2(reduced, full,
                      ~ compare_pair(models[[.x]], models[[.y]]))) %>%
    unnest(res)
}


# ===========================================================================
# 5. Supuestos del modelo lineal
# ===========================================================================

#' VIF maximo por predictor de un lm().
#'
#' car::vif(type = "predictor") agrupa cada variable con las interacciones en
#' que aparece y da un GVIF por variable. NA si el modelo tiene menos de dos
#' terminos.
max_vif <- function(fit) {
  if (length(attr(terms(fit), "term.labels")) < 2) return(NA_real_)
  v <- suppressMessages(car::vif(fit, type = "predictor"))
  max(if (is.matrix(v) || is.data.frame(v)) v[, "GVIF"] else v)
}


#' Fila de resultados de las pruebas de supuestos.
assumption_row <- function(sw, bp, dw, reset_f, reset_p, vif_max) {
  # Los nombres de las columnas coinciden con los de los argumentos (dw,
  # reset_f...), asi que se extraen antes de construir la tabla.
  values <- list(
    shapiro_w = unname(sw$statistic),
    shapiro_p = sw$p.value,
    bp_stat   = if (is.null(bp)) NA_real_ else unname(bp$statistic),
    bp_p      = if (is.null(bp)) NA_real_ else bp$p.value,
    dw        = unname(dw$statistic),
    dw_p      = dw$p.value,
    reset_f   = reset_f,
    reset_p   = reset_p,
    vif_max   = vif_max
  )
  as_tibble(values)
}


#' Supuestos del modelo lineal de un lm().
#'
#'   - Normalidad: shapiro.test sobre los residuos.
#'   - Homocedasticidad: lmtest::bptest (Breusch-Pagan studentizado de
#'     Koenker) sobre las columnas del modelo.
#'   - Independencia: lmtest::dwtest bilateral, con p-valor exacto, en el
#'     orden de las filas.
#'   - Linealidad: lmtest::resettest con ajustado^2 y ajustado^3.
#'   - Multicolinealidad: VIF maximo por predictor, max_vif().
#'
#' En M0 (sin predictores) solo aplican normalidad e independencia.
lm_assumptions <- function(fit) {
  has_x <- ncol(model.matrix(fit)) > 1
  rs    <- if (has_x) lmtest::resettest(fit, power = 2:3, type = "fitted")

  assumption_row(
    sw      = shapiro.test(residuals(fit)),
    bp      = if (has_x) lmtest::bptest(fit),
    dw      = lmtest::dwtest(fit, alternative = "two.sided"),
    reset_f = if (has_x) unname(rs$statistic) else NA_real_,
    reset_p = if (has_x) rs$p.value else NA_real_,
    vif_max = max_vif(fit)
  )
}


#' Supuestos del modelo lineal de elastic net.
#'
#' Elastic net no es un lm(), asi que las pruebas se aplican a sus residuos
#' e = y - prediccion, con las columnas que selecciono:
#'
#'   - Normalidad: shapiro.test sobre e.
#'   - Homocedasticidad: lmtest::bptest(e ~ 1, varformula = ~ columnas), el
#'     Breusch-Pagan de Koenker sobre los residuos centrados.
#'   - Independencia: lmtest::dwtest(e ~ 1), bilateral.
#'   - Linealidad: no hay version de paquete para un modelo que no es lm():
#'     se compara con anova() la regresion auxiliar de e sobre las columnas
#'     con y sin ajustado^2 y ajustado^3 (ajustado estandarizado). En un lm()
#'     esta prueba es identica a la RESET clasica.
#'   - Multicolinealidad: max_vif() sobre esas columnas (el VIF solo depende
#'     de las columnas, no de la respuesta).
en_assumptions <- function(en_model, clean_data) {
  data   <- apply_preprocessing(clean_data, en_model$params, en_model$coding)
  fitted <- predict_en(en_model, clean_data)
  X      <- build_en_design(data)$x[, en_model$selected, drop = FALSE]

  d   <- as.data.frame(X)
  names(d) <- make.names(colnames(X), unique = TRUE)
  rhs <- names(d)
  d$e <- data[[OUTCOME_VAR]] - fitted
  d$z <- as.numeric(scale(fitted))

  bp <- NULL
  reset_f <- reset_p <- vif_max <- NA_real_
  if (length(rhs) > 0) {
    bp <- lmtest::bptest(e ~ 1, varformula = reformulate(rhs), data = d)
    m0 <- lm(reformulate(rhs, response = "e"), data = d)
    av <- anova(m0, update(m0, . ~ . + I(z^2) + I(z^3)))
    reset_f <- av$F[2]
    reset_p <- av$`Pr(>F)`[2]
    vif_max <- max_vif(m0)
  }

  assumption_row(
    sw      = shapiro.test(d$e),
    bp      = bp,
    dw      = lmtest::dwtest(e ~ 1, data = d, alternative = "two.sided"),
    reset_f = reset_f,
    reset_p = reset_p,
    vif_max = vif_max
  )
}


#' Aplica las pruebas de supuestos a los lm() y a elastic net.
#'
#' Cada prueba se marca como cumplida si p >= alpha (o VIF < vif_limit).
#' all_met indica si el modelo cumple todas las pruebas que le aplican.
#'
#' @param models     lista de lm() (salida de load_fitted_models()).
#' @param en_model   en_model (salida de load_en_model()), o NULL.
#' @param clean_data muestra limpia con la que se ajusto elastic net.
assumption_tests <- function(models, en_model = NULL, clean_data = NULL,
                             alpha   = ASSUMPTION_ALPHA,
                             vif_limit = ASSUMPTION_VIF_MAX) {
  res <- imap_dfr(models, ~ lm_assumptions(.x) %>% mutate(model = .y, .before = 1))

  if (!is.null(en_model)) {
    res <- bind_rows(res, en_assumptions(en_model, clean_data) %>%
                       mutate(model = EN_MODEL_NAME, .before = 1))
  }

  res %>%
    mutate(
      normality_ok    = shapiro_p >= alpha,
      homosced_ok     = bp_p      >= alpha,
      independence_ok = dw_p      >= alpha,
      linearity_ok    = reset_p   >= alpha,
      collinearity_ok = vif_max   <  vif_limit
    ) %>%
    rowwise() %>%
    mutate(all_met = all(c_across(ends_with("_ok")), na.rm = TRUE)) %>%
    ungroup()
}


# ===========================================================================
# 6. Diagnostico por observacion
# ===========================================================================

#' Residuos, leverage y distancia de Cook de cada observacion de un lm().
#'
#'   - std_resid:  residuo estandarizado interno, rstandard().
#'   - stud_resid: residuo studentizado externo (sin la propia observacion),
#'                 rstudent().
#'   - leverage:   diagonal de la matriz sombrero, hatvalues().
#'   - cooks_d:    distancia de Cook, cooks.distance().
lm_diagnostics <- function(fit) {
  tibble(
    obs        = seq_len(nobs(fit)),
    p          = length(coef(fit)),
    fitted     = unname(fitted(fit)),
    residual   = unname(residuals(fit)),
    std_resid  = unname(rstandard(fit)),
    stud_resid = unname(rstudent(fit)),
    leverage   = unname(hatvalues(fit)),
    cooks_d    = unname(cooks.distance(fit))
  )
}


#' Residuos de elastic net.
#'
#' Elastic net no es un lm(): sus coeficientes estan encogidos y no tiene
#' matriz sombrero, asi que leverage, residuo studentizado y distancia de
#' Cook quedan en NA. std_resid es el residuo dividido por su desviacion
#' estandar.
en_diagnostics <- function(en_model, clean_data) {
  data   <- apply_preprocessing(clean_data, en_model$params, en_model$coding)
  fitted <- as.numeric(predict_en(en_model, clean_data))
  e      <- data[[OUTCOME_VAR]] - fitted

  tibble(
    obs        = seq_along(e),
    p          = NA_integer_,
    fitted     = fitted,
    residual   = e,
    std_resid  = e / sd(e),
    stud_resid = NA_real_,
    leverage   = NA_real_,
    cooks_d    = NA_real_
  )
}


#' Diagnostico por observacion de los lm() y de elastic net.
#'
#' Marca cada observacion con los umbrales de config.R: leverage alto
#' (h > k p / n), residuo studentizado grande (|t| > resid_limit) e
#' influyente (Cook > 4 / n).
model_diagnostics <- function(models, en_model = NULL, clean_data = NULL,
                              leverage_k  = DIAG_LEVERAGE_K,
                              resid_limit = DIAG_RESID_MAX) {
  res <- imap_dfr(models, ~ lm_diagnostics(.x) %>% mutate(model = .y, .before = 1))

  if (!is.null(en_model)) {
    res <- bind_rows(res, en_diagnostics(en_model, clean_data) %>%
                       mutate(model = EN_MODEL_NAME, .before = 1))
  }

  res %>%
    group_by(model) %>%
    mutate(n             = n(),
           leverage_cut  = leverage_k * p / n,
           cooks_cut     = 4 / n,
           high_leverage = leverage > leverage_cut,
           large_resid   = abs(stud_resid) > resid_limit,
           influential   = cooks_d > cooks_cut) %>%
    ungroup() %>%
    mutate(model = factor(model, levels = unique(model)))
}


#' Resumen por modelo del diagnostico: forma de los residuos y conteo de
#' observaciones marcadas.
diagnostics_summary <- function(diag) {
  diag %>%
    group_by(model) %>%
    summarise(
      n               = first(n),
      p               = first(p),
      # Asimetria y curtosis en exceso de los residuos estandarizados
      # (0 y 0 bajo normalidad).
      skewness        = mean((std_resid - mean(std_resid))^3) / sd(std_resid)^3,
      excess_kurtosis = mean((std_resid - mean(std_resid))^4) / sd(std_resid)^4 - 3,
      leverage_cut    = first(leverage_cut),
      max_leverage    = max(leverage),
      n_high_leverage = sum(high_leverage),
      n_large_resid   = sum(large_resid),
      max_cooks       = max(cooks_d),
      n_influential   = sum(influential),
      n_lev_and_resid = sum(high_leverage & large_resid),
      .groups = "drop"
    )
}


#' Observaciones que resultan influyentes (Cook > 4 / n) en al menos
#' min_models de los lm(), con sus valores en las variables del modelo.
#'
#' @param data muestra analitica con la que se ajustaron los lm(), en el
#'             mismo orden de filas.
influential_observations <- function(diag, data, min_models = 1) {
  diag %>%
    filter(!is.na(cooks_d)) %>%
    group_by(obs) %>%
    summarise(n_models   = sum(influential),
              max_cooks  = max(cooks_d),
              max_abs_t  = max(abs(stud_resid)),
              max_lev    = max(leverage),
              .groups = "drop") %>%
    filter(n_models >= min_models) %>%
    arrange(desc(n_models), desc(max_cooks)) %>%
    left_join(data %>%
                mutate(obs = row_number()) %>%
                select(obs, all_of(c(OUTCOME_VAR, GENDER_VAR, SUPPORT_VARS))),
              by = "obs")
}


# ===========================================================================
# 7. Guardado
# ===========================================================================

#' Escribe las tablas de indices y comparaciones en outputs/tables/.
save_comparison_outputs <- function(indices_tbl, nested_tbl, assumptions_tbl = NULL,
                                    diagnostics_tbl = NULL) {
  walk(unique(dirname(c(PATH_FIT_INDICES, PATH_NESTED_TESTS, PATH_ASSUMPTIONS,
                        PATH_DIAGNOSTICS))),
       ~ dir.create(.x, showWarnings = FALSE, recursive = TRUE))

  write_csv(indices_tbl, PATH_FIT_INDICES)
  write_csv(nested_tbl, PATH_NESTED_TESTS)
  paths <- c(PATH_FIT_INDICES, PATH_NESTED_TESTS)

  if (!is.null(assumptions_tbl)) {
    write_csv(assumptions_tbl, PATH_ASSUMPTIONS)
    paths <- c(paths, PATH_ASSUMPTIONS)
  }

  if (!is.null(diagnostics_tbl)) {
    write_csv(diagnostics_tbl, PATH_DIAGNOSTICS)
    paths <- c(paths, PATH_DIAGNOSTICS)
  }

  invisible(paths)
}
