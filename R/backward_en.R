# ---------------------------------------------------------------------------
# backward_en.R
#
# Eliminacion backward con elastic net y AIC. Este archivo NO ejecuta nada al
# hacer source(): la orquestacion vive en notebooks/backward_elastic_net.ipynb.
#
# Procedimiento (una "variable" es una columna del diseno de EN_FULL_MODEL:
# una dummy por nivel no referencia, cada dimension de apoyo centrada y cada
# interaccion):
#   1. Se parte de todas las columnas.
#   2. Se ajusta elastic net eligiendo alpha y lambda por validacion cruzada:
#      para cada alpha de EN_ALPHAS, cv.glmnet() con los mismos folds; se
#      queda la combinacion de menor error de CV (lambda.min).
#   3. Si ningun coeficiente queda exactamente en cero, se detiene.
#   4. Para cada columna con coeficiente cero se reajusta el paso 2 sin ella
#      y se calcula el AIC del elastic net resultante; sale la columna cuyo
#      retiro deja el menor AIC.
#   5. Se vuelve al paso 2 con el modelo reducido (alpha y lambda se eligen
#      de nuevo desde cero).
#
# AIC = -2 log L + 2 k, con k = coeficientes distintos de cero + intercepto.
# log L es la verosimilitud gaussiana con sigma^2 = RSS / n (o binomial si la
# respuesta es 0/1), evaluada en las predicciones del elastic net.
#
# Requiere haber hecho source() de R/config.R, R/data_prep.R,
# R/moderation_pipeline.R, R/cross_validation.R y R/elastic_net.R antes.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(glmnet)
})


# ===========================================================================
# 1. Ajuste de un paso
# ===========================================================================

#' TRUE si la respuesta es binaria (dos valores distintos).
is_binary_outcome <- function(y) length(unique(y)) == 2


#' Log-verosimilitud de las predicciones de un elastic net.
#'
#' Gaussiana con la varianza de maxima verosimilitud (RSS / n), o binomial
#' con las probabilidades predichas.
en_log_lik <- function(y, pred, binary) {
  if (binary) {
    p <- pmin(pmax(pred, 1e-12), 1 - 1e-12)
    return(sum(y * log(p) + (1 - y) * log(1 - p)))
  }
  n <- length(y)
  -n / 2 * (log(2 * pi * sum((y - pred)^2) / n) + 1)
}


#' Metricas de validacion cruzada a partir de las predicciones fuera de fold.
#'
#' Continua: RMSE y R2 (1 - SSE / SST). Binaria: AUC (por rangos) y accuracy
#' con corte 0.5.
cv_metrics <- function(y, pred_oof, binary) {
  if (binary) {
    r   <- rank(pred_oof)
    n1  <- sum(y == 1)
    n0  <- sum(y == 0)
    auc <- (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
    return(tibble(cv_auc = auc, cv_accuracy = mean((pred_oof >= 0.5) == y)))
  }
  tibble(cv_rmse = sqrt(mean((y - pred_oof)^2)),
         cv_r2   = 1 - sum((y - pred_oof)^2) / sum((y - mean(y))^2))
}


#' Ajusta elastic net con las columnas `vars`, eligiendo alpha y lambda.
#'
#' Para cada alpha corre cv.glmnet() con los mismos folds y se queda con el
#' alpha cuyo error de CV en lambda.min es el menor.
#'
#' @param x      matriz de diseno completa (sin intercepto).
#' @param y      respuesta.
#' @param vars   columnas de x que entran en el modelo.
#' @param foldid asignacion de folds, comun a todos los ajustes.
#' @return lista con alpha, lambda, coeficientes (sin intercepto), AIC,
#'   n de ceros y metricas de CV.
fit_en_step <- function(x, y, vars, foldid, alphas = EN_ALPHAS) {
  if (length(vars) < 2) {
    stop("glmnet necesita al menos dos columnas.", call. = FALSE)
  }
  binary <- is_binary_outcome(y)
  family <- if (binary) "binomial" else "gaussian"
  xs     <- x[, vars, drop = FALSE]

  cvs <- map(set_names(alphas, as.character(alphas)), function(a) {
    cv.glmnet(xs, y, family = family, alpha = a, foldid = foldid, keep = TRUE)
  })
  cv_min <- map_dbl(cvs, ~ min(.x$cvm))
  best   <- cvs[[which.min(cv_min)]]
  i_min  <- which(best$lambda == best$lambda.min)

  beta <- as.matrix(coef(best, s = "lambda.min"))[, 1]
  b    <- beta[-1]
  pred <- as.numeric(predict(best, newx = xs, s = "lambda.min", type = "response"))
  k    <- sum(b != 0) + 1

  list(
    vars    = vars,
    alpha   = as.numeric(names(cvs)[which.min(cv_min)]),
    lambda  = best$lambda.min,
    coef    = b,
    aic     = -2 * en_log_lik(y, pred, binary) + 2 * k,
    k       = k,
    n_zero  = sum(b == 0),
    metrics = cv_metrics(y, best$fit.preval[, i_min], binary)
  )
}


# ===========================================================================
# 2. Eliminacion backward
# ===========================================================================

#' Fila del registro de un paso.
step_row <- function(step, fit, removed) {
  tibble(
    step        = step,
    n_vars      = length(fit$vars),
    removed     = removed,
    alpha       = fit$alpha,
    lambda      = fit$lambda,
    aic         = fit$aic,
    k           = fit$k,
    n_zero      = fit$n_zero,
    zero_vars   = paste(names(fit$coef)[fit$coef == 0], collapse = " + "),
    vars        = list(fit$vars),
    nonzero     = list(names(fit$coef)[fit$coef != 0])
  ) %>%
    bind_cols(fit$metrics)
}


#' Eliminacion backward con elastic net y AIC.
#'
#' En cada paso, cada columna con coeficiente cero es candidata: se reajusta
#' el elastic net sin ella (alpha y lambda desde cero) y sale la que deja el
#' menor AIC, aunque ese AIC sea mayor que el actual. Ese reajuste es el
#' modelo del paso siguiente (con los mismos folds, repetirlo daria lo mismo).
#'
#' Se detiene cuando ningun coeficiente es cero o cuando quedan dos columnas
#' (glmnet no ajusta con una sola).
#'
#' @return lista con la tabla de pasos (steps), los candidatos evaluados en
#'   cada paso (candidates) y los ajustes (fits).
backward_en <- function(x, y, foldid, alphas = EN_ALPHAS, verbose = TRUE) {
  fit   <- fit_en_step(x, y, colnames(x), foldid, alphas)
  fits  <- list(fit)
  steps <- list(step_row(0L, fit, NA_character_))
  cands <- list()
  step  <- 0L

  while (fit$n_zero > 0 && length(fit$vars) > 2) {
    step <- step + 1L
    zero <- names(fit$coef)[fit$coef == 0]

    trials <- map(set_names(zero), ~ fit_en_step(x, y, setdiff(fit$vars, .x),
                                                 foldid, alphas))
    cands[[step]] <- tibble(step = step, candidate = zero,
                            aic = map_dbl(trials, "aic"))

    out <- zero[which.min(map_dbl(trials, "aic"))]
    fit <- trials[[out]]
    fits[[step + 1L]]  <- fit
    steps[[step + 1L]] <- step_row(step, fit, out)

    if (verbose) {
      cat(sprintf("Paso %2d: sale %-28s AIC = %8.2f  ceros = %d\n",
                  step, out, fit$aic, fit$n_zero))
    }
  }

  if (verbose) {
    motivo <- if (fit$n_zero == 0) "ningún coeficiente en cero"
              else "quedan dos columnas"
    cat(sprintf("Fin: %s.\n", motivo))
  }

  list(steps = bind_rows(steps), candidates = bind_rows(cands), fits = fits)
}
