# Descripción del proyecto

**Natalia Alvarado y Luis Cabarcas** · Septiembre 2026

---

## 1. Planteamiento del problema

La conducta suicida en la adolescencia es un problema prioritario de salud pública. El modelo IMV plantea que la ideación suicida surge de una secuencia en la que la derrota social (*defeat*) lleva a la sensación de no poder escapar (*entrapment*), y que esta, sin factores que la mitiguen, puede derivar en pensamientos suicidas. El apoyo social percibido se ha estudiado sobre todo como moderador de la transición *entrapment* → ideación. Su papel protector en la formación del propio *entrapment* ha recibido poca atención, la evidencia es escasa y poco diversa, y no se ha examinado si el género condiciona el efecto. Esto justifica evaluar la relación en contextos poco representados, como el de los adolescentes de municipios semirurales del Atlántico.

---

## 2. Antecedentes

- **O'Connor (2011)** formuló el IMV [@OConnor2011], y **O'Connor y Kirtley (2018)** lo actualizaron señalando la escasez de investigación sobre moderadores y mediadores entre *entrapment* y suicidalidad [@OConnorKirtley2018].
- **Pollak et al. (2021)**, en 74 adolescentes, hallaron que el pensamiento positivo sobre el futuro, postulado como protector, exacerbó la asociación *defeat*/*entrapment*–ideación [@Pollak2021].
- **Demir y Buyukcolpan (2026)** probaron tres moderadores en dos muestras turcas: la pertenencia frustrada y el apoyo social percibido moderaron la relación *entrapment*–ideación en direcciones opuestas; la carga percibida no moderó [@DemirBuyukcolpan2026].
- **Souza et al. (2024)**, en una revisión de 100 estudios (138,365 participantes), confirmaron la ruta *defeat* → *entrapment* → ideación y señalaron la falta de estudios sobre moderadores en poblaciones poco representadas [@Souza2024].
- **Trejos-Herrera et al. (2018)** validaron el MSPSS en 766 adolescentes de Barranquilla, antecedente psicométrico directo del instrumento usado [@Trejos2018].
- **Metodología de moderación:** Yuan, Cheng y Maxwell (2014) proponen un modelo de dos niveles que maneja la heterocedasticidad [@Yuan2014]; Liu y Yuan (2021) desarrollan nuevas medidas de tamaño del efecto [@LiuYuan2021]; Liu y Wang (2024) proponen procedimientos robustos con datos faltantes [@LiuWang2024].

---

## 3. Justificación

En Colombia, la Encuesta Nacional de Salud Mental de 2015 halló una frecuencia de ideación suicida de 6,6 % y de intento de 2,5 % [@MinSalud2015], y datos recientes indican que el problema persiste en la población joven [@ElNuevoSiglo2026]. Entre 2022 y 2024 se registraron 193 suicidios en Barranquilla y el resto del Atlántico, muchos en menores de 21 años [@EmisoraAtl2024].

El marco normativo respalda la relevancia del tema: la Ley 1616 de 2013 [@Ley1616_2013], el Plan Decenal de Salud Pública 2012–2021, con meta de 4,7 suicidios por 100.000 habitantes [@PDSP2012], la Resolución 4886 de 2018 [@Res4886_2018] y la Ley 2460 de 2025 [@Ley2460_2025]. Identificar factores protectores como el apoyo social es, por tanto, un insumo para metas de política pública aún no cumplidas.

Aunque el IMV tiene respaldo empírico general [@Souza2024], el papel del apoyo social en una etapa temprana del modelo está poco probado y su efecto protector puede no ser universal. Este estudio aborda ese vacío en un contexto colombiano poco representado.

### 3.1. Objetivo general

Evaluar si el apoyo social percibido, diferenciado en familia, amigos y otras personas significativas, ejerce un efecto protector sobre el *entrapment* en adolescentes escolarizados de municipios semirurales del Atlántico, y si el género modera dicha relación.

### 3.2. Objetivos específicos

1. Caracterizar las covariables sociodemográficas e implementar un procedimiento de imputación de datos faltantes en el MSPSS, a partir del EDA.
2. Estimar el efecto principal de cada dimensión del apoyo social sobre el *entrapment* mediante regresión lineal jerárquica por bloques.
3. Evaluar si el género modera linealmente esa relación mediante términos de interacción (Dimensión × Género).
4. Incorporar la incertidumbre de medición de los constructos latentes mediante SEM o un enfoque bayesiano (MCMC).
5. Explorar dinámicas no lineales (umbral, techo o piso) y su moderación por género mediante modelos aditivos generalizados (GAM).

---

## 4. Acerca de los datos

La base original tiene 680 observaciones de estudiantes escolarizados del departamento del Atlántico, con variables demográficas, antropométricas y escalas psicométricas de salud mental, bienestar y experiencias escolares. Las escalas (salvo el cuestionario de acoso escolar, que se trata con más cautela por la heterogeneidad de sus formatos de respuesta) tienen modelos de medición establecidos y se transformaron en puntajes latentes continuos.

El archivo de trabajo (`df_moderation_2026.csv`) contiene 680 filas y 12 columnas. `Escuela` fue reemplazada por `Municipality` para evitar frecuencias muy bajas por institución.

**Tabla 1.** Variables de la base de análisis.

| Variable | Tipo | Descripción |
|---|---|---|
| `Gender` | Binaria (moderador) | Género del estudiante (Female / Male) |
| `Age` | Continua | Edad en años (13–20) |
| `Ethnicity` | Nominal (4 niveles) | Mestizo / Caucasian / Afrodescendant / Other |
| `Grade` | Ordinal (3 niveles) | Grado escolar (9 / 10 / 11) |
| `SSE` | Ordinal (3 niveles) | Nivel socioeconómico (Low / Medium / High), colapsado desde los 6 estratos DANE |
| `Education` | Ordinal (4 niveles) | Máximo nivel educativo de los padres o cuidadores |
| `Municipality` | Nominal (4 niveles) | Polo Nuevo / Santo Tomás / Puerto Colombia / Suan |
| `Family` | Continua (puntaje latente) | Apoyo social percibido de la familia (MSPSS) |
| `Friends` | Continua (puntaje latente) | Apoyo social percibido de los amigos (MSPSS) |
| `SigOthers` | Continua (puntaje latente) | Apoyo social percibido de otras personas significativas (MSPSS) |
| `Entrapment` | Continua (puntaje latente) | Puntaje latente de *entrapment* (E-SF, 4 ítems) |
| `Entrapment_SE` | Continua | Error estándar del puntaje latente de *entrapment* |

---

## Referencias
