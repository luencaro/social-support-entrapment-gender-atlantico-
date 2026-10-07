# ---------------------------------------------------------------------------
# model_comparison.R
#
# Definiciones de funciones para comparar los 10 modelos lineales jerarquicos
# ajustados por moderation_pipeline.R. Este archivo NO ejecuta nada al hacer
# source(): la orquestacion vive en notebooks/model_comparison.ipynb.
#
# Alcance: cargar outputs/models/fitted_models.rds y reportar
#   - indices de ajuste por modelo: R2, R2 ajustado, logLik, AIC y BIC;
#   - comparaciones entre modelos anidados: delta R2, prueba F, f2 de Cohen,
#     razon de verosimilitud (LRT), delta AIC / BIC y prueba de Wald del
#     bloque con errores estandar robustos HC3 (sandwich + lmtest);
#   - supuestos del modelo lineal de los 10 lm() y de elastic net (lmtest y
#     performance).
#
# Requiere haber hecho source("R/config.R") antes. Para los supuestos de
# elastic net, tambien R/data_prep.R, R/moderation_pipeline.R y
# R/elastic_net.R. sandwich, lmtest y performance se usan con prefijo.
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

#' Prueba de Wald del bloque de terminos que el modelo completo agrega.
#'
#' Usa la covarianza HC3 (sandwich::vcovHC), de modo que no asume
#' homocedasticidad. lmtest::waldtest la reporta como F = W / q con gl
#' (q, gl residuales del modelo completo).
wald_block_hc3 <- function(reduced, full) {
  w <- lmtest::waldtest(reduced, full,
                        vcov = function(x) sandwich::vcovHC(x, type = "HC3"),
                        test = "F")
  tibble(f_hc3 = w$F[2], p_hc3 = w$`Pr(>F)`[2])
}


#' Compara un par de modelos anidados.
compare_pair <- function(reduced, full) {
  if (!all(names(coef(reduced)) %in% names(coef(full)))) {
    stop("El modelo reducido no esta anidado en el completo.", call. = FALSE)
  }

  r2_red  <- summary(reduced)$r.squared
  r2_full <- summary(full)$r.squared
  ll_red  <- logLik(reduced)
  ll_full <- logLik(full)

  # Prueba F clasica (asume homocedasticidad).
  av <- anova(reduced, full)

  # Razon de verosimilitud.
  lr    <- 2 * (as.numeric(ll_full) - as.numeric(ll_red))
  df_lr <- attr(ll_full, "df") - attr(ll_red, "df")

  bind_cols(
    tibble(
      df_added  = av$Df[2],
      r2_reduced = r2_red,
      r2_full   = r2_full,
      delta_r2  = r2_full - r2_red,
      # f2 de Cohen del bloque: delta R2 / (1 - R2 del completo).
      cohen_f2  = (r2_full - r2_red) / (1 - r2_full),
      f_stat    = av$F[2],
      df_resid  = av$Res.Df[2],
      p_f       = av$`Pr(>F)`[2],
      lr_stat   = lr,
      p_lr      = pchisq(lr, df_lr, lower.tail = FALSE),
      delta_aic = AIC(full) - AIC(reduced),
      delta_bic = BIC(full) - BIC(reduced)
    ),
    wald_block_hc3(reduced, full)
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

#' VIF maximo por termino de un lm().
#'
#' performance::check_collinearity da el VIF de cada termino; para un factor
#' con varias dummies es el GVIF de Fox y Monette (un solo valor por factor).
#' NA si el modelo tiene menos de dos terminos.
max_vif <- function(fit) {
  if (length(attr(terms(fit), "term.labels")) < 2) return(NA_real_)
  cc <- suppressMessages(suppressWarnings(performance::check_collinearity(fit)))
  max(cc$VIF)
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
#'   - Multicolinealidad: VIF (GVIF en factores) maximo, max_vif().
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
# 6. Guardado
# ===========================================================================

#' Escribe las tablas de indices y comparaciones en outputs/tables/.
save_comparison_outputs <- function(indices_tbl, nested_tbl, assumptions_tbl = NULL) {
  walk(unique(dirname(c(PATH_FIT_INDICES, PATH_NESTED_TESTS, PATH_ASSUMPTIONS))),
       ~ dir.create(.x, showWarnings = FALSE, recursive = TRUE))

  write_csv(indices_tbl, PATH_FIT_INDICES)
  write_csv(nested_tbl, PATH_NESTED_TESTS)
  paths <- c(PATH_FIT_INDICES, PATH_NESTED_TESTS)

  if (!is.null(assumptions_tbl)) {
    write_csv(assumptions_tbl, PATH_ASSUMPTIONS)
    paths <- c(paths, PATH_ASSUMPTIONS)
  }

  invisible(paths)
}
