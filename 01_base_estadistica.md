# Base estadística

Esta sección resume la matemática que se usa en el proyecto: cómo se calcula
cada estadístico, cómo se decide con un p-valor y cómo funcionan los modelos
que se ajustan en los notebooks. Las fórmulas son las que implementa el código.

**Notación.** $n$ es el tamaño de la muestra; $x_1, \dots, x_n$ son las
observaciones de una variable; $y$ es la variable respuesta (*entrapment*);
$\hat{y}$ es una predicción; $e_i = y_i - \hat{y}_i$ es un residuo.

## 1. Estadística descriptiva

### Medidas de tendencia central

- **Media:** $\displaystyle \bar{x} = \frac{1}{n}\sum_{i=1}^{n} x_i$.
- **Mediana:** el valor central de los datos ordenados. Con $x_{(1)} \le \dots \le x_{(n)}$,
  es $x_{((n+1)/2)}$ si $n$ es impar y el promedio de $x_{(n/2)}$ y
  $x_{(n/2+1)}$ si $n$ es par. A diferencia de la media, no se mueve con los
  valores extremos.

Cuando la media y la mediana difieren mucho, la distribución es asimétrica: la
media se desplaza hacia la cola larga.

### Medidas de dispersión

- **Varianza y desviación estándar (DE):**

  $$
  s^2 = \frac{1}{n-1}\sum_{i=1}^{n}(x_i - \bar{x})^2, \qquad s = \sqrt{s^2}.
  $$

  Se divide por $n-1$ y no por $n$ porque $\bar{x}$ se estimó con los mismos
  datos; así $s^2$ no subestima la varianza poblacional.
- **Cuartiles y rango intercuartílico:** $Q_1$ y $Q_3$ dejan por debajo el
  25 % y el 75 % de los datos; $\text{IQR} = Q_3 - Q_1$ cubre el 50 % central.
  En los diagramas de caja, los bigotes llegan hasta el último dato dentro de
  $[Q_1 - 1.5\,\text{IQR},\; Q_3 + 1.5\,\text{IQR}]$, y lo que queda fuera se
  dibuja como punto.
- **Desviación absoluta mediana (MAD):**

  $$
  \text{MAD} = 1.4826 \cdot \operatorname{mediana}\big(|x_i - \operatorname{mediana}(x)|\big).
  $$

  La constante 1.4826 hace que la MAD coincida con la DE cuando los datos son
  normales.

### Valores atípicos: filtro de Hampel

Un valor se marca como atípico si cae fuera de

$$
\operatorname{mediana}(x) \pm 3 \cdot \text{MAD}.
$$

### Frecuencias y datos faltantes

Para una variable categórica, la frecuencia relativa de un nivel es
$n_{\text{nivel}}/n$, y el porcentaje de faltantes de una variable es
$100 \cdot n_{\text{NA}}/n$.

## 2. Pruebas de hipótesis

### Lógica general

1. **Hipótesis nula $H_0$:** la afirmación de "no hay efecto" (dos medias
   iguales, una correlación igual a cero, un coeficiente igual a cero).
   **Hipótesis alternativa $H_1$:** lo contrario.
2. **Estadístico de prueba $T$:** un número calculado con los datos que mide
   cuánto se alejan de lo que esperaría $H_0$.
3. **Distribución bajo $H_0$:** la distribución que tendría $T$ si $H_0$
   fuera cierta (normal, $t$, $F$, $\chi^2$…).
4. **p-valor:** la probabilidad, suponiendo $H_0$ cierta, de obtener un
   estadístico al menos tan extremo como el observado. En una prueba bilateral
   con estadístico simétrico, $p = P(|T| \ge |t_{\text{obs}}| \mid H_0)$.

### Regla de decisión

Se fija de antemano un nivel de significancia $\alpha$; en el proyecto,
$\alpha = 0.05$.

- Si $p < \alpha$: **se rechaza $H_0$**. La diferencia observada sería poco
  probable si $H_0$ fuera cierta.
- Si $p \ge \alpha$: **no se rechaza $H_0$**. Esto no prueba que $H_0$ sea
  cierta; solo que los datos no alcanzan para descartarla.

Hay dos errores posibles: el **error tipo I** (rechazar $H_0$ siendo cierta),
cuya probabilidad es $\alpha$, y el **error tipo II** (no rechazarla siendo
falsa). La potencia, $1 - P(\text{tipo II})$, crece con el tamaño de muestra.

### Intervalos de confianza

Un intervalo de confianza del 95 % para un parámetro $\theta$ se construye, en
general, como

$$
\hat\theta \pm z_{0.975}\cdot \text{EE}(\hat\theta), \qquad z_{0.975} \approx 1.96,
$$

o con el cuantil de la $t$ en lugar del de la normal. Si el IC95 % excluye el
valor de $H_0$, la prueba bilateral correspondiente da $p < 0.05$.

### Comparaciones múltiples: corrección de Holm

Al hacer $m$ pruebas, la probabilidad de al menos un falso positivo crece. Holm
ordena los p-valores de menor a mayor, $p_{(1)} \le \dots \le p_{(m)}$, y los
ajusta así:

$$
p^{\text{Holm}}_{(i)} = \max_{j \le i}\; \min\!\big(1,\; (m - j + 1)\, p_{(j)}\big).
$$

Luego se aplica la misma regla, $p^{\text{Holm}} < \alpha$. 


## 3. Comparación de grupos

### Dos grupos

- **$t$ de Welch** (paramétrica). Compara dos medias sin suponer varianzas
  iguales:

  $$
  t = \frac{\bar{x}_1 - \bar{x}_2}{\sqrt{s_1^2/n_1 + s_2^2/n_2}},
  \qquad
  \nu = \frac{\left(s_1^2/n_1 + s_2^2/n_2\right)^2}
             {\dfrac{(s_1^2/n_1)^2}{n_1-1} + \dfrac{(s_2^2/n_2)^2}{n_2-1}},
  $$

  y el p-valor sale de una $t$ con $\nu$ grados de libertad.
- **Wilcoxon-Mann-Whitney** (no paramétrica). Ordena todas las observaciones
  juntas y compara los rangos. Con $R_1$ la suma de rangos del grupo 1,

  $$
  W = R_1 - \frac{n_1(n_1+1)}{2},
  $$

  que cuenta en cuántos pares (uno de cada grupo) gana el grupo 1. Para $n$
  grande se usa la aproximación normal. No supone normalidad, así que es
  robusta al efecto piso del *entrapment*.
- **$d$ de Cohen** (tamaño del efecto de la $t$):

  $$
  d = \frac{\bar{x}_1 - \bar{x}_2}{s^*},
  $$

  con $s^*$ la DE combinada
  $\sqrt{\big((n_1-1)s_1^2 + (n_2-1)s_2^2\big)/(n_1+n_2-2)}$, o el promedio
  $\sqrt{(s_1^2 + s_2^2)/2}$ cuando no se supone varianza común. Valores
  orientativos: 0.2 pequeño, 0.5 mediano, 0.8 grande.
- **Correlación rango-biserial** (tamaño del efecto de Wilcoxon):

  $$
  r_{rb} = \frac{2W}{n_1 n_2} - 1,
  $$

  entre −1 y 1: es la diferencia entre la proporción de pares en que gana el
  grupo 1 y la proporción en que gana el grupo 2.

### Tres o más grupos

- **ANOVA**
  $w_j = n_j/s_j^2$, $W = \sum_j w_j$ y media ponderada
  $\tilde{x} = \sum_j w_j \bar{x}_j / W$:

  $$
  F^* = \frac{\dfrac{1}{k-1}\sum_{j=1}^{k} w_j(\bar{x}_j - \tilde{x})^2}
             {1 + \dfrac{2(k-2)}{k^2-1}\sum_{j=1}^{k}\dfrac{(1 - w_j/W)^2}{n_j - 1}},
  $$

  que se compara con una $F$ de $k-1$ y
  $\nu_2 = (k^2-1)\big/\big(3\sum_j (1-w_j/W)^2/(n_j-1)\big)$ grados de
  libertad.
- **Kruskal-Wallis** (no paramétrica). Con $R_j$ la suma de rangos del grupo
  $j$:

  $$
  H = \frac{12}{n(n+1)}\sum_{j=1}^{k}\frac{R_j^2}{n_j} - 3(n+1) \;\sim\; \chi^2_{k-1}
  $$

  (con una corrección cuando hay empates).
- **Tamaños del efecto:**

  $$
  \eta^2 = \frac{SS_{\text{entre}}}{SS_{\text{total}}}
         = \frac{\sum_j n_j(\bar{x}_j - \bar{x})^2}{\sum_i (x_i - \bar{x})^2},
  \qquad
  \varepsilon^2 = \frac{H}{n-1}.
  $$

  $\eta^2$ es la proporción de la varianza explicada por el grupo;
  $\varepsilon^2$ es su análogo basado en rangos.

## 4. Asociación entre variables categóricas

### Prueba $\chi^2$ de independencia

Para una tabla de contingencia de $r$ filas y $c$ columnas, con frecuencias
observadas $O_{ij}$, las frecuencias esperadas bajo independencia son
$E_{ij} = (\text{total fila}_i \cdot \text{total columna}_j)/n$, y

$$
\chi^2 = \sum_{i=1}^{r}\sum_{j=1}^{c}\frac{(O_{ij} - E_{ij})^2}{E_{ij}}
\;\sim\; \chi^2_{(r-1)(c-1)}.
$$

La aproximación $\chi^2$ falla cuando alguna $E_{ij} < 5$; en ese caso el
p-valor se calcula por Monte Carlo.

### V de Cramér

$$
V = \sqrt{\frac{\chi^2}{n\,\big(\min(r, c) - 1\big)}},
$$

entre 0 (independencia) y 1 (asociación perfecta). Mide la intensidad de la
asociación, no su dirección ni qué celdas la generan.

## 5. Correlación

### Pearson

$$
r = \frac{\sum_i (x_i - \bar{x})(y_i - \bar{y})}
         {\sqrt{\sum_i (x_i - \bar{x})^2}\,\sqrt{\sum_i (y_i - \bar{y})^2}},
\qquad -1 \le r \le 1.
$$

Mide la asociación **lineal**. Para probar $H_0: \rho = 0$:

$$
t = \frac{r\sqrt{n-2}}{\sqrt{1-r^2}} \;\sim\; t_{n-2}.
$$

### Spearman

Es la correlación de Pearson aplicada a los rangos de $x$ y de $y$. Mide la
asociación **monótona** y es robusta a valores extremos. Si Pearson y Spearman
coinciden, la asociación no depende de la forma exacta de la distribución.

### Transformación $z$ de Fisher

La distribución de $r$ es asimétrica cerca de ±1. La transformación

$$
z = \operatorname{atanh}(r) = \tfrac{1}{2}\ln\frac{1+r}{1-r}
$$

es aproximadamente normal con EE $= 1/\sqrt{n-3}$. Se usa para dos cosas:

- **IC95 % de $r$:** $\tanh\!\big(z \pm 1.96/\sqrt{n-3}\big)$.
- **Comparar dos correlaciones de grupos independientes** (por ejemplo,
  mujeres frente a hombres):

  $$
  z = \frac{\operatorname{atanh}(r_1) - \operatorname{atanh}(r_2)}
           {\sqrt{\dfrac{1}{n_1-3} + \dfrac{1}{n_2-3}}} \;\sim\; N(0,1).
  $$

### Jackknife

Para ver si una correlación depende de pocos casos, se recalcula $n$ veces
excluyendo una observación cada vez (*leave-one-out*). Con
$z_{(i)} = \operatorname{atanh}(r_{(i)})$, sin la observación $i$, y
$\bar{z}_{(\cdot)}$ su promedio:

$$
\widehat{\text{sesgo}} = (n-1)\big(\bar{z}_{(\cdot)} - z\big), \qquad
z_{\text{jack}} = z - \widehat{\text{sesgo}}, \qquad
\text{EE}_{\text{jack}} = \sqrt{\frac{n-1}{n}\sum_{i=1}^{n}\big(z_{(i)} - \bar{z}_{(\cdot)}\big)^2}.
$$

El IC95 % es $\tanh\!\big(z_{\text{jack}} \pm 1.96\,\text{EE}_{\text{jack}}\big)$.
Un sesgo despreciable y un EE parecido al clásico indican una correlación
estable.

## 6. Modelos lineales

### El modelo

$$
y_i = \beta_0 + \beta_1 x_{i1} + \dots + \beta_p x_{ip} + \varepsilon_i,
\qquad \varepsilon_i \sim N(0, \sigma^2) \text{ independientes},
$$

o en forma matricial $\mathbf{y} = \mathbf{X}\boldsymbol\beta + \boldsymbol\varepsilon$,
con $\mathbf{X}$ de $n \times (p+1)$ (la primera columna es de unos).

### Estimación por mínimos cuadrados ordinarios (OLS)

Se eligen los coeficientes que minimizan la suma de cuadrados de los residuos,
$\text{SSE} = \sum_i e_i^2$:

$$
\hat{\boldsymbol\beta} = (\mathbf{X}^\top\mathbf{X})^{-1}\mathbf{X}^\top\mathbf{y},
\qquad
\hat\sigma^2 = \frac{\text{SSE}}{n - p - 1},
\qquad
\widehat{\operatorname{Var}}(\hat{\boldsymbol\beta}) = \hat\sigma^2(\mathbf{X}^\top\mathbf{X})^{-1}.
$$

El EE de cada coeficiente es la raíz del elemento correspondiente de la
diagonal de esa matriz. Para probar $H_0: \beta_j = 0$:

$$
t_j = \frac{\hat\beta_j}{\text{EE}(\hat\beta_j)} \;\sim\; t_{n-p-1}.
$$

**Interpretación:** $\hat\beta_j$ es el cambio esperado en $y$ por cada unidad
de $x_j$, manteniendo constantes las demás variables del modelo.


### Interacción y moderación

Para una dimensión de apoyo $D$ y el género $G$:

$$
y = \beta_0 + \beta_1 D^c + \beta_2 G + \beta_3\,(D^c \cdot G) + \dots + \varepsilon.
$$

La pendiente del apoyo es $\beta_1$ en las mujeres ($G = 0$) y
$\beta_1 + \beta_3$ en los hombres ($G = 1$). Por eso $\beta_3$ mide la
**moderación**: la diferencia de pendientes entre géneros. Probar
$H_0: \beta_3 = 0$ es probar que el género no modera el efecto del apoyo.

### Bondad de ajuste

$$
R^2 = 1 - \frac{\text{SSE}}{\text{SST}}, \quad \text{SST} = \sum_i (y_i - \bar{y})^2,
\qquad
R^2_{\text{aj}} = 1 - (1 - R^2)\,\frac{n-1}{n-p-1}.
$$

$R^2$ es la proporción de la varianza de $y$ explicada por el modelo y nunca
baja al agregar términos. $R^2_{\text{aj}}$ penaliza los términos adicionales.


### Verosimilitud y criterios de información

Con errores normales, la log-verosimilitud del modelo ajustado es

$$
\ell = -\frac{n}{2}\left[\ln(2\pi) + \ln\!\left(\frac{\text{SSE}}{n}\right) + 1\right],
$$

y $k = p + 2$ cuenta los parámetros (los $p+1$ coeficientes más $\sigma^2$).

- **Prueba de razón de verosimilitud (LRT):**

  $$
  \text{LR} = 2(\ell_{\text{comp}} - \ell_{\text{red}}) \;\sim\; \chi^2_q.
  $$

- **Criterios de información** (menor es mejor):

  $$
  \text{AIC} = -2\ell + 2k, \qquad \text{BIC} = -2\ell + k\ln n.
  $$

  Los dos premian el ajuste y castigan la complejidad; BIC castiga más
  ($\ln 530 \approx 6.3$ por parámetro frente a 2), así que prefiere modelos
  más pequeños. Se reportan como $\Delta_i = \text{AIC}_i - \min_j \text{AIC}_j$:
  $\Delta < 2$ indica modelos prácticamente equivalentes y $\Delta > 10$,
  evidencia fuerte a favor del mejor.
- **Pesos de Akaike:**

  $$
  w_i = \frac{\exp(-\Delta_i/2)}{\sum_j \exp(-\Delta_j/2)},
  $$

  la probabilidad relativa de que cada modelo sea el mejor del conjunto.

### Errores estándar robustos (HC3)

La varianza clásica $\hat\sigma^2(\mathbf{X}^\top\mathbf{X})^{-1}$ supone
varianza constante de los errores. La versión robusta HC3 no lo supone:

$$
\widehat{\operatorname{Var}}_{\text{HC3}}(\hat{\boldsymbol\beta})
= (\mathbf{X}^\top\mathbf{X})^{-1}\,
  \mathbf{X}^\top \operatorname{diag}\!\left(\frac{e_i^2}{(1 - h_{ii})^2}\right)\mathbf{X}\,
  (\mathbf{X}^\top\mathbf{X})^{-1},
$$

donde $h_{ii}$ es el elemento $i$ de la diagonal de
$\mathbf{H} = \mathbf{X}(\mathbf{X}^\top\mathbf{X})^{-1}\mathbf{X}^\top$ (el
apalancamiento de la observación $i$). Para probar un bloque de $q$
coeficientes $\mathbf{b}$ con esa varianza $\mathbf{V}$ se usa la prueba de
Wald:

$$
F_{\text{HC3}} = \frac{\mathbf{b}^\top \mathbf{V}^{-1}\mathbf{b}}{q}
\;\sim\; F_{q,\; n - p - 1}.
$$

## 7. Supuestos del modelo lineal

| Supuesto | Prueba | $H_0$ |
|---|---|---|
| Normalidad de los residuos | Shapiro-Wilk | Los residuos son normales |
| Homocedasticidad | Breusch-Pagan | La varianza de los residuos es constante |
| Independencia | Durbin-Watson | Los residuos consecutivos no están correlacionados |
| Linealidad | RESET de Ramsey | La forma lineal es adecuada |
| Multicolinealidad | VIF | — (criterio: VIF < 5) |


## 8. Métricas de error

Para un fold de prueba con $n_{\text{te}}$ casos:

$$
\text{RMSE} = \sqrt{\frac{1}{n_{\text{te}}}\sum_{i}(y_i - \hat{y}_i)^2},
\qquad
\text{MAE} = \frac{1}{n_{\text{te}}}\sum_{i}|y_i - \hat{y}_i|,
\qquad
R^2_{\text{fuera}} = 1 - \frac{\sum_i (y_i - \hat{y}_i)^2}{\sum_i (y_i - \bar{y}_{\text{tr}})^2},
$$

donde $\bar{y}_{\text{tr}}$ es la media del entrenamiento. $R^2_{\text{fuera}}$
mide qué proporción del error de "predecir siempre la media" elimina el modelo;
a diferencia del $R^2$ dentro de muestra, puede bajar al agregar términos que
solo ajustan ruido.

### Regla de 1 error estándar

1. El mejor modelo es el de menor RMSE medio, $\overline{\text{RMSE}}_{\min}$.
2. El umbral es $\overline{\text{RMSE}}_{\min} + 1 \cdot \text{EE}_{\min}$.
3. Los modelos con RMSE medio por debajo del umbral **no se distinguen** del
   mejor: la diferencia está dentro del ruido de la propia estimación.


