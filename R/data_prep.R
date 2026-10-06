# ---------------------------------------------------------------------------
# data_prep.R
#
# Preparacion de los datos: de la base cruda a la muestra analitica lista para
# modelar.
#
#   1. Recodifica a NA las etiquetas de dato faltante ("Missing" y equivalentes)
#      en Gender, SSE, Ethnicity y Municipality, y aplica droplevels().
#   2. Colapsa SSE a Low / Medium-High segun SSE_COLLAPSE_MAP.
#   3. Aplica listwise deletion pura sobre MODEL_VARS. No imputa nada.
#   4. Verifica que N coincida con EXPECTED_N; si no, se detiene con error.
#   5. Fija los niveles de referencia de Municipality, SSE y Ethnicity.
#   6. Centra Family, Friends y SigOthers restando la gran media de la muestra
#      analitica, y deja las columnas *_c en el CSV de salida: el pipeline de
#      modelado no vuelve a calcular ninguna media.
#   7. Codifica el genero segun GENDER_CODING.
#
# No elimina ni winsoriza valores extremos: las escalas son puntajes latentes
# acotados y un valor extremo es una respuesta valida.
#
# Salida:
#   data/df_modeling_2026.csv
#
# Uso:
#   Rscript R/data_prep.R     # ejecuta run_data_prep()
#   source("R/data_prep.R")   # solo define funciones, no corre nada
#
# Requiere haber hecho source("R/config.R") antes.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
})


#' Recodifica a NA las etiquetas de dato faltante de una columna categorica.
recode_missing_to_na <- function(x, missing_labels = MISSING_LABELS) {
  x_chr <- as.character(x)
  x_chr[trimws(x_chr) %in% missing_labels] <- NA_character_
  x_chr
}


#' Colapsa SSE segun el mapa declarado en config.R.
#'
#' Cualquier nivel no contemplado en el mapa se convierte en NA, de modo que un
#' cambio no anunciado en las categorias de origen aparezca como dato faltante
#' en vez de colarse silenciosamente.
collapse_sse <- function(x, map = SSE_COLLAPSE_MAP, levels = SSE_LEVELS) {
  x_chr <- recode_missing_to_na(x)
  factor(unname(map[x_chr]), levels = levels)
}


#' Resuelve un nivel de referencia declarado en config.R contra los datos.
#'
#' "most_frequent" se resuelve como el nivel mas frecuente de la muestra
#' analitica. Cualquier otro valor se toma literal y se valida que exista.
resolve_reference_level <- function(x, spec, var_name) {
  present <- levels(droplevels(factor(x)))

  if (identical(spec, "most_frequent")) {
    return(names(sort(table(x), decreasing = TRUE))[1])
  }

  if (!spec %in% present) {
    stop(sprintf(
      "Nivel de referencia '%s' declarado para %s no existe en la muestra analitica (niveles: %s).",
      spec, var_name, paste(present, collapse = ", ")
    ), call. = FALSE)
  }
  spec
}


#' Detiene la ejecucion si el N no es el reportado por el tratamiento de datos.
assert_expected_n <- function(data, expected_n = EXPECTED_N) {
  n <- nrow(data)
  if (!identical(as.integer(n), as.integer(expected_n))) {
    stop(sprintf(
      paste0("N de la muestra analitica (%d) no coincide con EXPECTED_N (%d).\n",
             "Revisa la base cruda o actualiza EXPECTED_N en R/config.R."),
      n, expected_n
    ), call. = FALSE)
  }
  invisible(n)
}


#' Traduce el factor de genero a la columna numerica usada en las formulas.
encode_gender <- function(x, coding = GENDER_CODING,
                          schemes = GENDER_CODING_SCHEMES) {
  if (!coding %in% names(schemes)) {
    stop(sprintf("GENDER_CODING '%s' desconocido. Opciones: %s.",
                 coding, paste(names(schemes), collapse = ", ")), call. = FALSE)
  }
  scheme <- schemes[[coding]]

  x_chr <- as.character(x)
  desconocidos <- setdiff(unique(x_chr[!is.na(x_chr)]), names(scheme))
  if (length(desconocidos) > 0) {
    stop(sprintf("Niveles de genero sin codigo asignado: %s",
                 paste(desconocidos, collapse = ", ")), call. = FALSE)
  }
  unname(scheme[x_chr])
}


#' Lee la base cruda y construye la muestra analitica lista para modelar.
#'
#' Pasos, en orden:
#'   1. "Missing" (y equivalentes) -> NA en las categoricas.
#'   2. SSE colapsado a Low / Medium-High.
#'   3. Listwise deletion pura sobre MODEL_VARS, sin imputacion, + droplevels().
#'   4. Verificacion de que N == EXPECTED_N.
#'   5. Niveles de referencia de Municipality, SSE y Ethnicity.
#'   6. Centrado del apoyo por la gran media de la muestra analitica.
#'   7. Codificacion de genero segun GENDER_CODING.
#'
#' No elimina ni winsoriza valores extremos: las escalas estan acotadas y un
#' valor extremo es una respuesta valida.
#'
#' @return tibble con atributos "grand_means", "gender_coding" y "n_raw".
build_analytic_sample <- function(path = PATH_RAW_DATA,
                                  coding = GENDER_CODING) {
  raw <- read_csv(path, show_col_types = FALSE)

  missing_cols <- setdiff(c(MODEL_VARS, OUTCOME_SE_VAR), names(raw))
  if (length(missing_cols) > 0) {
    stop(sprintf("La base cruda no tiene las columnas: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  # Pasos 1-2: Missing -> NA en las categoricas y colapso de SSE.
  cleaned <- raw %>%
    mutate(
      across(all_of(setdiff(CATEGORICAL_VARS, "SSE")), recode_missing_to_na),
      SSE = collapse_sse(SSE)
    ) %>%
    mutate(
      across(
        all_of(setdiff(CATEGORICAL_VARS, "SSE")),
        ~ factor(.x, levels = FACTOR_LEVELS[[cur_column()]])
      )
    )

  # Paso 3: listwise deletion pura. Sin imputacion de ningun tipo.
  analytic <- cleaned %>%
    select(all_of(c(MODEL_VARS, OUTCOME_SE_VAR))) %>%
    drop_na(all_of(MODEL_VARS)) %>%
    mutate(across(where(is.factor), droplevels))

  # Paso 4: el N tiene que ser el reportado por el tratamiento de datos.
  assert_expected_n(analytic)

  # Paso 5: niveles de referencia.
  for (v in names(REFERENCE_LEVELS)) {
    ref <- resolve_reference_level(analytic[[v]], REFERENCE_LEVELS[[v]], v)
    analytic[[v]] <- relevel(analytic[[v]], ref = ref)
  }

  # Paso 6: centrado por la gran media de la muestra analitica.
  grand_means <- map_dbl(set_names(SUPPORT_VARS), ~ mean(analytic[[.x]]))
  for (v in SUPPORT_VARS) {
    analytic[[paste0(v, CENTERED_SUFFIX)]] <- analytic[[v]] - grand_means[[v]]
  }

  # Paso 7: codificacion de genero.
  analytic[[GENDER_TERM]] <- encode_gender(analytic[[GENDER_VAR]], coding = coding)

  attr(analytic, "grand_means")   <- grand_means
  attr(analytic, "gender_coding") <- coding
  attr(analytic, "n_raw")         <- nrow(raw)
  analytic
}


#' Ejecuta la preparacion y escribe data/df_modeling_2026.csv.
#'
#' @return tibble invisible con la muestra analitica.
run_data_prep <- function(path = PATH_RAW_DATA,
                          coding = GENDER_CODING,
                          out_path = PATH_MODELING_DATA,
                          verbose = TRUE) {
  set.seed(SEED)

  analytic    <- build_analytic_sample(path, coding = coding)
  grand_means <- attr(analytic, "grand_means")

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  write_csv(analytic, out_path)

  if (verbose) {
    refs <- map_chr(set_names(names(REFERENCE_LEVELS)), ~ levels(analytic[[.x]])[1])
    cat(sprintf("Filas crudas:          %d\n", attr(analytic, "n_raw")))
    cat(sprintf("Muestra analitica:     %d (listwise sobre %s)\n",
                nrow(analytic), paste(MODEL_VARS, collapse = ", ")))
    cat(sprintf("Niveles de referencia: %s\n",
                paste(sprintf("%s = %s", names(refs), refs), collapse = " | ")))
    cat(sprintf("Grandes medias:        %s\n",
                paste(sprintf("%s = %.6f", names(grand_means), grand_means),
                      collapse = " | ")))
    cat(sprintf("Codificacion genero:   %s (%s)\n", coding,
                paste(sprintf("%s = %s",
                              names(GENDER_CODING_SCHEMES[[coding]]),
                              GENDER_CODING_SCHEMES[[coding]]),
                      collapse = ", ")))
    cat(sprintf("Escrito: %s\n", out_path))
  }

  invisible(analytic)
}


#' Deduce el esquema de codificacion a partir de los codigos presentes.
detect_gender_coding <- function(x, schemes = GENDER_CODING_SCHEMES) {
  presentes <- sort(unique(x[!is.na(x)]))
  hit <- detect(names(schemes),
                ~ isTRUE(all.equal(presentes, sort(unname(schemes[[.x]])))))
  if (is.null(hit)) {
    stop(sprintf("Los codigos de genero presentes (%s) no coinciden con ningun esquema de config.R.",
                 paste(presentes, collapse = ", ")), call. = FALSE)
  }
  hit
}


# Se ejecuta solo al invocar `Rscript R/data_prep.R`; con source() no corre nada.
if (sys.nframe() == 0L) {
  source("R/config.R")
  run_data_prep()
}
