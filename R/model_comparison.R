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
#     bloque con errores estandar robustos HC3.
#
# La matriz HC3 se calcula aqui directamente (vcov_hc3) para no agregar
# sandwich/lmtest al lockfile.
#
# Requiere haber hecho source("R/config.R") antes.
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

#' Matriz de covarianza HC3 de los coeficientes de un lm.
#'
#' (X'X)^-1 X' diag(e_i^2 / (1 - h_i)^2) X (X'X)^-1, equivalente a
#' sandwich::vcovHC(fit, type = "HC3").
vcov_hc3 <- function(fit) {
  X     <- model.matrix(fit)
  e     <- residuals(fit)
  h     <- hatvalues(fit)
  bread <- chol2inv(qr.R(qr(X)))
  meat  <- crossprod(X * (e / (1 - h)))
  V <- bread %*% meat %*% bread
  dimnames(V) <- list(colnames(X), colnames(X))
  V
}


#' Prueba de Wald del bloque de terminos que el modelo completo agrega.
#'
#' Usa la covarianza HC3, de modo que no asume homocedasticidad. Se reporta
#' como F = W / q con gl (q, gl residuales del modelo completo).
wald_block_hc3 <- function(reduced, full) {
  added <- setdiff(names(coef(full)), names(coef(reduced)))
  b <- coef(full)[added]
  V <- vcov_hc3(full)[added, added, drop = FALSE]
  q <- length(added)
  f <- as.numeric(t(b) %*% solve(V, b)) / q
  tibble(f_hc3 = f, p_hc3 = pf(f, q, df.residual(full), lower.tail = FALSE))
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

#' Prueba los supuestos del modelo lineal a partir de residuos y ajustados.
#'
#' Sirve igual para un lm() y para elastic net, porque solo usa los
#' residuos, los valores ajustados y las columnas del modelo (sin intercepto).
#' Las pruebas que necesitan predictores devuelven NA cuando el modelo no
#' tiene columnas (M0).
#'
#'   - Normalidad: Shapiro-Wilk sobre los residuos.
#'   - Homocedasticidad: Breusch-Pagan studentizado (Koenker), n x R2 de la
#'     regresion de e^2 sobre las columnas, chi2 con p gl.
#'   - Independencia: Durbin-Watson en el orden de las filas, con la
#'     aproximacion asintotica DW ~ N(2, 4/n).
#'   - Linealidad: RESET de Ramsey, prueba F de ajustado^2 y ajustado^3 en la
#'     regresion auxiliar de los residuos sobre las columnas. En un lm() es
#'     identica a la RESET clasica.
#'   - Multicolinealidad: VIF maximo, diagonal de la inversa de la matriz de
#'     correlaciones de las columnas.
#'
#' @param resid  residuos.
#' @param fitted valores ajustados.
#' @param X      matriz de columnas del modelo sin intercepto (puede tener 0).
#' @return tibble de una fila con estadisticos y p-valores.
check_assumptions <- function(resid, fitted, X) {
  n <- length(resid)
  p <- ncol(X)

  # Normalidad
  sw <- shapiro.test(resid)

  # Homocedasticidad (Breusch-Pagan de Koenker)
  bp_stat <- bp_p <- NA_real_
  if (p > 0) {
    aux     <- lm(resid^2 ~ X)
    bp_stat <- n * summary(aux)$r.squared
    bp_p    <- pchisq(bp_stat, df = aux$rank - 1, lower.tail = FALSE)
  }

  # Independencia (Durbin-Watson, aproximacion normal)
  dw   <- sum(diff(resid)^2) / sum(resid^2)
  dw_p <- 2 * pnorm(-abs((dw - 2) / sqrt(4 / n)))

  # Linealidad (RESET con potencias del ajustado estandarizado)
  reset_f <- reset_p <- NA_real_
  if (p > 0 && sd(fitted) > 0) {
    z  <- as.numeric(scale(fitted))
    m0 <- lm(resid ~ X)
    m1 <- lm(resid ~ X + I(z^2) + I(z^3))
    av <- anova(m0, m1)
    reset_f <- av$F[2]
    reset_p <- av$`Pr(>F)`[2]
  }

  # Multicolinealidad
  vif_max <- if (p >= 2) max(diag(solve(cor(X)))) else NA_real_

  tibble(
    shapiro_w = unname(sw$statistic), shapiro_p = sw$p.value,
    bp_stat   = bp_stat,              bp_p      = bp_p,
    dw        = dw,                   dw_p      = dw_p,
    reset_f   = reset_f,              reset_p   = reset_p,
    vif_max   = vif_max
  )
}


#' Aplica check_assumptions() a los lm() y a elastic net.
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
  res <- imap_dfr(models, function(fit, nm) {
    X <- model.matrix(fit)[, -1, drop = FALSE]
    check_assumptions(residuals(fit), fitted(fit), X) %>%
      mutate(model = nm, .before = 1)
  })

  if (!is.null(en_model)) {
    data   <- apply_preprocessing(clean_data, en_model$params, en_model$coding)
    X      <- build_en_design(data)$x[, en_model$selected, drop = FALSE]
    fitted <- predict_en(en_model, clean_data)
    res <- bind_rows(res,
                     check_assumptions(data[[OUTCOME_VAR]] - fitted, fitted, X) %>%
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
