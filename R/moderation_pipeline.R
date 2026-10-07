# ---------------------------------------------------------------------------
# moderation_pipeline.R
#
# Definiciones de funciones para ajustar los 10 modelos lineales jerarquicos.
# Este archivo NO ejecuta nada al hacer source(): la orquestacion vive en
# notebooks/01_fit_models.ipynb.
#
# Alcance: releer la muestra analitica, construir las formulas desde config.R,
# ajustar con lm() y guardar modelos y tablas.
#
# Fuera de alcance (pipeline posterior que carga fitted_models.rds):
# verosimilitud, AIC/BIC, tests de cambio, errores estandar HC3, VIF y
# residuales.
#
# Requiere haber hecho source("R/config.R") y source("R/data_prep.R") antes:
# load_modeling_data() reutiliza resolve_reference_level(), assert_expected_n()
# y detect_gender_coding() de la preparacion, en vez de redefinirlas.
# jsonlite se usa con prefijo: library(jsonlite) enmascararia purrr::flatten.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
})


# ===========================================================================
# 1. Muestra analitica
# ===========================================================================

#' Relee data/df_modeling_2026.csv restaurando factores y referencias.
#'
#' Util para modelar en una sesion distinta de la que corrio run_data_prep().
load_modeling_data <- function(path = PATH_MODELING_DATA) {
  if (!file.exists(path)) {
    stop(sprintf("No existe '%s'. Ejecuta antes run_data_prep().", path),
         call. = FALSE)
  }
  data <- read_csv(path, show_col_types = FALSE)

  for (v in CATEGORICAL_VARS) {
    data[[v]] <- factor(data[[v]], levels = FACTOR_LEVELS[[v]])
  }
  data <- mutate(data, across(where(is.factor), droplevels))

  for (v in names(REFERENCE_LEVELS)) {
    ref <- resolve_reference_level(data[[v]], REFERENCE_LEVELS[[v]], v)
    data[[v]] <- relevel(data[[v]], ref = ref)
  }

  assert_expected_n(data)

  # Recupera (no recalcula) la constante de centrado: X - X_c es la gran media
  # que se resto en la preparacion, y el esquema de genero se deduce de los
  # codigos presentes.
  attr(data, "grand_means") <- map_dbl(
    set_names(SUPPORT_VARS),
    ~ mean(data[[.x]] - data[[paste0(.x, CENTERED_SUFFIX)]])
  )
  attr(data, "gender_coding") <- detect_gender_coding(data[[GENDER_TERM]])
  data
}


# ===========================================================================
# 2. Formulas
# ===========================================================================

#' Construye las 10 formulas de la secuencia jerarquica desde config.R.
#'
#' Cambiar COVARIATE_VARS o SUPPORT_VARS en config.R basta para que la
#' secuencia se reconstruya: ninguna formula esta escrita a mano.
#'
#' @return lista nombrada de formulas, en orden de estimacion.
build_formulas <- function(outcome      = OUTCOME_VAR,
                           gender_term  = GENDER_TERM,
                           covariates   = COVARIATE_VARS,
                           support_vars = SUPPORT_VARS,
                           suffix       = CENTERED_SUFFIX) {

  base_terms <- c(gender_term, covariates)
  support_c  <- paste0(support_vars, suffix)

  as_formula <- function(terms) {
    rhs <- if (length(terms) == 0L) "1" else paste(terms, collapse = " + ")
    as.formula(paste(outcome, "~", rhs), env = globalenv())
  }

  interaction_terms <- function(terms) paste0(terms, ":", gender_term)

  formulas <- list(
    M0 = as_formula(character(0)),                 # solo intercepto
    M1 = as_formula(base_terms)                    # genero + covariables
  )

  # Bloques por dimension: efecto principal (M2_D) e interaccion (M3_D).
  for (i in seq_along(support_vars)) {
    dc <- support_c[i]
    formulas[[paste0("M2_", support_vars[i])]] <-
      as_formula(c(base_terms, dc))
    formulas[[paste0("M3_", support_vars[i])]] <-
      as_formula(c(base_terms, dc, interaction_terms(dc)))
  }

  # Bloque conjunto: las tres dimensiones a la vez.
  formulas[["M2_joint"]] <- as_formula(c(base_terms, support_c))
  formulas[["M3_joint"]] <- as_formula(c(base_terms, support_c,
                                         interaction_terms(support_c)))

  formulas
}


#' Devuelve la formula como texto de una sola linea.
formula_text <- function(formula) {
  paste(deparse(formula, width.cutoff = 500L), collapse = " ")
}


# ===========================================================================
# 3. Ajuste
# ===========================================================================

#' Ajusta con lm() cada formula sobre la misma matriz de modelado.
#'
#' Todas las filas estan completas, de modo que los 10 modelos usan
#' exactamente las mismas N observaciones.
fit_models <- function(formulas = build_formulas(),
                       data     = load_modeling_data()) {
  set.seed(SEED)

  faltantes <- setdiff(c(MODEL_VARS, SUPPORT_VARS_C, GENDER_TERM), names(data))
  if (length(faltantes) > 0) {
    stop(sprintf("La matriz de modelado no tiene las columnas: %s",
                 paste(faltantes, collapse = ", ")), call. = FALSE)
  }

  incompletos <- sum(!complete.cases(data[c(MODEL_VARS, SUPPORT_VARS_C, GENDER_TERM)]))
  if (incompletos > 0) {
    stop(sprintf("La matriz de modelado tiene %d filas con faltantes; se esperaba 0.",
                 incompletos), call. = FALSE)
  }

  models <- map(formulas, function(fm) {
    fit <- lm(fm, data = data)
    fit$call$formula <- fm   # para que print(fit) muestre la formula, no el objeto
    fit
  })

  coding <- attr(data, "gender_coding")
  attr(models, "gender_coding") <- if (is.null(coding)) GENDER_CODING else coding
  attr(models, "n")             <- nrow(data)
  models
}


# ===========================================================================
# 4. Tablas de salida
# ===========================================================================

#' Tabla resumen: una fila por modelo.
#'
#' Columnas: nombre, formula como texto, N, gl residuales, R2 y R2 ajustado.
#' Nada mas: verosimilitud, AIC/BIC y tests de cambio van en el pipeline
#' posterior.
summarise_models <- function(models) {
  imap_dfr(models, function(fit, nm) {
    s <- summary(fit)
    tibble(
      model         = nm,
      formula       = formula_text(formula(fit)),
      n             = length(fit$residuals),
      df_residual   = df.residual(fit),
      r_squared     = s$r.squared,
      adj_r_squared = s$adj.r.squared
    )
  })
}


#' Tabla de coeficientes de los 10 modelos con EE OLS (no robusto).
#'
#' Los errores estandar HC3 se calculan en el pipeline posterior.
tidy_coefficients <- function(models) {
  imap_dfr(models, function(fit, nm) {
    tidy(fit) %>%
      transmute(
        model     = nm,
        term      = term,
        estimate  = estimate,
        std_error = std.error,
        statistic = statistic,
        p_value   = p.value
      )
  })
}


# ===========================================================================
# 5. Guardado
# ===========================================================================

#' Manifiesto del ajuste: deja registrado como se modelo.
build_fit_manifest <- function(models, data, coding = GENDER_CODING) {
  grand_means <- attr(data, "grand_means")

  list(
    created_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    n             = nrow(data),
    n_models      = length(models),
    model_names   = names(models),
    outcome       = OUTCOME_VAR,
    gender_var    = GENDER_VAR,
    gender_term   = GENDER_TERM,
    gender_coding = coding,
    gender_codes  = as.list(GENDER_CODING_SCHEMES[[coding]]),
    covariates    = COVARIATE_VARS,
    support_vars  = SUPPORT_VARS,
    centered_vars = SUPPORT_VARS_C,
    grand_means   = if (is.null(grand_means)) NULL else as.list(grand_means),
    reference_levels = map(set_names(names(REFERENCE_LEVELS)),
                           ~ levels(data[[.x]])[1]),
    sse_collapse  = as.list(SSE_COLLAPSE_MAP),
    deletion      = "listwise (sin imputacion)",
    estimator     = "lm() / OLS",
    seed          = SEED
  )
}


#' Escribe modelos, tablas y manifiesto en outputs/.
save_model_outputs <- function(models, summary_tbl, coefficients_tbl,
                               fit_manifest = NULL) {
  walk(unique(c(dirname(PATH_FITTED_MODELS), dirname(PATH_MODELS_SUMMARY),
                dirname(PATH_COEFFICIENTS), dirname(PATH_FIT_MANIFEST))),
       ~ dir.create(.x, showWarnings = FALSE, recursive = TRUE))

  saveRDS(models, PATH_FITTED_MODELS)
  write_csv(summary_tbl, PATH_MODELS_SUMMARY)
  write_csv(coefficients_tbl, PATH_COEFFICIENTS)

  paths <- c(PATH_FITTED_MODELS, PATH_MODELS_SUMMARY, PATH_COEFFICIENTS)

  if (!is.null(fit_manifest)) {
    jsonlite::write_json(fit_manifest, PATH_FIT_MANIFEST,
                         auto_unbox = TRUE, pretty = TRUE, digits = 12)
    paths <- c(paths, PATH_FIT_MANIFEST)
  }

  invisible(paths)
}
