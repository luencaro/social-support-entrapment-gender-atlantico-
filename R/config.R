# ---------------------------------------------------------------------------
# config.R
#
# Unica fuente de verdad para las decisiones parametrizables del analisis:
# nombres de variables, colapso de SSE, niveles de referencia, codificacion de
# genero y semilla.
#
# R/moderation_pipeline.R lee de aqui y no redefine ninguna de estas
# decisiones. Este archivo solo define constantes: no ejecuta nada al hacer
# source().
# ---------------------------------------------------------------------------


## --- Rutas -----------------------------------------------------------------
# Todas relativas a la raiz del proyecto.

PATH_RAW_DATA       <- Sys.getenv("DATA_PATH", unset = "data/df_moderation_2026.csv")
PATH_MODELING_DATA  <- "data/df_modeling_2026.csv"
PATH_FITTED_MODELS  <- "outputs/models/fitted_models.rds"
PATH_FIT_MANIFEST   <- "outputs/models/fit_manifest.json"
PATH_MODELS_SUMMARY <- "outputs/tables/models_summary.csv"
PATH_COEFFICIENTS   <- "outputs/tables/coefficients_ols.csv"
PATH_FIT_INDICES    <- "outputs/tables/fit_indices.csv"
PATH_NESTED_TESTS   <- "outputs/tables/nested_tests.csv"
PATH_ASSUMPTIONS    <- "outputs/tables/assumption_tests.csv"
PATH_CV_FOLDS       <- "outputs/tables/cv_fold_metrics.csv"
PATH_CV_SUMMARY     <- "outputs/tables/cv_summary.csv"
PATH_EN_MODEL       <- "outputs/models/en_model.rds"


## --- Semilla ---------------------------------------------------------------
# Misma semilla que el EDA. lm() es determinista; se fija por consistencia con
# el resto del proyecto.

SEED <- 2026L


## --- Nombres de variables --------------------------------------------------

OUTCOME_VAR    <- "Entrapment"        # puntaje latente de entrapment
OUTCOME_SE_VAR <- "Entrapment_SE"     # EE del puntaje latente (se arrastra, no se modela aqui)

# Dimensiones del MSPSS. El sufijo construye el nombre de la version centrada.
SUPPORT_VARS    <- c("Family", "Friends", "SigOthers")
CENTERED_SUFFIX <- "_c"
SUPPORT_VARS_C  <- paste0(SUPPORT_VARS, CENTERED_SUFFIX)

# Moderador: columna cruda (factor) y columna numerica usada en las formulas.
GENDER_VAR  <- "Gender"
GENDER_TERM <- "Gender_code"

# Covariables de ajuste del bloque M1 (ademas del moderador).
COVARIATE_VARS <- c("Municipality", "SSE", "Ethnicity")

# Variables categoricas que reciben limpieza de "Missing" + droplevels().
CATEGORICAL_VARS <- c(GENDER_VAR, COVARIATE_VARS)

# Variables que definen la muestra analitica (listwise deletion pura).
MODEL_VARS <- c(OUTCOME_VAR, SUPPORT_VARS, GENDER_VAR, COVARIATE_VARS)


## --- Codigos de dato faltante ----------------------------------------------
# Etiquetas que en las variables categoricas significan "sin dato" y deben
# recodificarse a NA antes de droplevels().

MISSING_LABELS <- c("", " ", "NA", "N/A", "NaN", "None", ".",
                    "Missing", "missing", "MISSING", "Sin dato", "sin dato")


## --- Colapso de SSE --------------------------------------------------------
# Los seis estratos DANE ya vienen colapsados en Low / Medium / High en la base
# cruda. Aqui se colapsan a dos niveles porque High tiene 7 casos en la muestra
# completa: una celda demasiado pequena para estimar un contraste propio.

SSE_COLLAPSE_MAP <- c(
  "Low"    = "Low",
  "Medium" = "Medium-High",
  "High"   = "Medium-High"
)
SSE_LEVELS <- c("Low", "Medium-High")


## --- Orden de niveles de los factores --------------------------------------
# Orden de presentacion. El nivel de referencia se fija aparte (ver
# REFERENCE_LEVELS) y no tiene por que ser el primero de esta lista.

FACTOR_LEVELS <- list(
  Gender       = c("Female", "Male"),
  Municipality = c("Polo Nuevo", "Santo Tomas", "Puerto Colombia", "Suan"),
  SSE          = SSE_LEVELS,
  Ethnicity    = c("Mestizo", "Caucasian", "Afrodescendant", "Other")
)


## --- Niveles de referencia -------------------------------------------------
# "most_frequent" se resuelve sobre la muestra analitica (no sobre la base
# cruda) dentro de build_analytic_sample().

REFERENCE_LEVELS <- list(
  SSE          = "Low",
  Ethnicity    = "Mestizo",
  Municipality = "most_frequent"
)


## --- Codificacion de genero ------------------------------------------------
# "dummy"  -> Female = 0,    Male = 1
#             En los modelos M3 el coeficiente de D_c es la pendiente del apoyo
#             en el nivel 0 (Female) y D_c:Gender_code es la diferencia
#             Male - Female.
# "effect" -> Female = -0.5, Male = +0.5
#             En los modelos M3 el coeficiente de D_c es el promedio de las dos
#             pendientes y D_c:Gender_code sigue siendo la diferencia
#             Male - Female.
#
# La codificacion efectivamente usada queda registrada en PATH_FIT_MANIFEST y
# en el atributo "gender_coding" de la lista de modelos guardada.

GENDER_CODING <- "dummy"

GENDER_CODING_SCHEMES <- list(
  dummy  = c(Female = 0,    Male = 1),
  effect = c(Female = -0.5, Male = 0.5)
)


## --- N esperado ------------------------------------------------------------
# N de la muestra analitica reportado por el tratamiento de datos: listwise
# deletion sobre MODEL_VARS, sin imputacion, sobre las 680 filas de la base
# cruda. Si la preparacion produce un N distinto, el pipeline se detiene con
# error en vez de seguir con una muestra inesperada.

EXPECTED_N <- 530L


## --- Tratamiento de valores extremos ---------------------------------------
# Las escalas son puntajes latentes acotados: un valor extremo es una respuesta
# valida. No se eliminan ni se winsorizan casos. Esta constante existe para
# dejar la decision explicita y verificable, no para habilitar lo contrario.

WINSORIZE_EXTREMES <- FALSE


## --- Validacion cruzada ----------------------------------------------------
# K-fold repetida, estratificada por genero para que cada fold conserve la
# proporcion mujeres / hombres. El preprocesamiento que depende de los datos
# (centrado y nivel de referencia "most_frequent") se aprende en cada fold de
# entrenamiento y se aplica al de prueba.
#
# Regla de CV_SE_RULE errores estandar: se reportan los modelos cuyo RMSE
# medio no supera min(RMSE) + CV_SE_RULE * EE del mejor. No se elige uno.

CV_FOLDS      <- 10L
CV_REPEATS    <- 20L
CV_STRATA_VAR <- GENDER_VAR
CV_SE_RULE    <- 1


## --- Elastic net -----------------------------------------------------------
# Modelo 11, junto a los 10 lm(). Se parte del diseno de EN_FULL_MODEL (todos
# los terminos candidatos) y todas sus columnas se penalizan por igual:
# genero, dummies de las covariables, dimensiones de apoyo e interacciones.
#
# alpha y lambda se eligen con una validacion cruzada interna de
# EN_INNER_FOLDS folds (estratificada por CV_STRATA_VAR) sobre los datos con
# que se ajusta: los 530 casos en el modelo final, o el fold de entrenamiento
# dentro de la validacion cruzada externa. Regla de CV_SE_RULE EE: entre las
# combinaciones (alpha, lambda) con RMSE dentro del umbral, la que deja menos
# columnas distintas de cero.
#
# alpha = 0 (ridge) se excluye porque nunca deja coeficientes en cero.

EN_MODEL_NAME   <- "ElasticNet"
EN_FULL_MODEL   <- "M3_joint"
EN_ALPHAS       <- c(0.1, 0.25, 0.5, 0.75, 1)
EN_NLAMBDA      <- 100L
EN_INNER_FOLDS  <- 10L


## --- Supuestos del modelo lineal -------------------------------------------
# Nivel de significancia de las pruebas de supuestos (Shapiro-Wilk,
# Breusch-Pagan, Durbin-Watson, RESET) y VIF maximo tolerado.

ASSUMPTION_ALPHA   <- 0.05
ASSUMPTION_VIF_MAX <- 5
