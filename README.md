# Apoyo social, entrapment y género — Atlántico

Book en [MyST](https://mystmd.org/) para el proyecto de Seminario Investigativo
*"El género en la relación entre apoyo social y entrapment en adolescentes
escolarizados de municipios del Atlántico"* (Natalia Alvarado y Luis Cabarcas).

## Contexto

El modelo Motivacional-Volitivo Integrado (IMV) estudia el apoyo social sobre
todo como un moderador que amortigua la transición del *entrapment* (la
sensación de no poder escapar) hacia la ideación suicida. Su posible papel
sobre el propio *entrapment* está poco estudiado, y no se sabe si el género
condiciona ese efecto.

Este proyecto evalúa, en 680 adolescentes escolarizados (grados 9.º a 11.º) de
municipios del Atlántico, Colombia, si el apoyo social percibido —familia,
amigos y otras personas significativas— protege frente al *entrapment*, y si
el género modera esa relación.

## Estructura

```
.devcontainer/                     Entorno de desarrollo (Dev Container)
.github/workflows/deploy.yml       Publicación del book en GitHub Pages
index.md                           Portada del book
00_descripcion_del_proyecto.md     Problema, antecedentes, justificación y objetivos
01_base_estadistica.md             Matemática que usa el proyecto
references.bib                     Bibliografía
notebooks/
  eda.ipynb                        Análisis Exploratorio de Datos
  fit_models.ipynb                 Ajuste de los 10 modelos jerárquicos, elastic net
                                   y validación cruzada
  model_comparison.ipynb           Comparación de modelos y supuestos
R/
  config.R                         Decisiones del análisis (variables, rutas, parámetros)
  data_prep.R                      Limpieza y preprocesamiento de la muestra analítica
  moderation_pipeline.R            Fórmulas y ajuste de los modelos lineales
  elastic_net.R                    Ajuste de elastic net (modelo 11)
  cross_validation.R               Validación cruzada repetida
  model_comparison.R               Índices de ajuste, pruebas anidadas y supuestos
data/                              Datos (no se versionan)
outputs/                           Modelos y tablas generados (no se versionan)
myst.yml                           Configuración del proyecto/sitio MyST
renv.lock                          Versiones exactas de los paquetes de R
```

## Entorno de desarrollo (Dev Container)

El proyecto trae un [Dev Container](https://containers.dev/)
([.devcontainer/](.devcontainer)) que arma un entorno automáticamente e
igual para diferentes sistemas operativos.

**Requisitos previos:**

1. [VS Code](https://code.visualstudio.com/) con la extensión
   [Dev Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers).
2. Un motor de contenedores corriendo: Docker Desktop en Windows, o Docker/Podman
   en Linux.

**Pasos:**

1. Abre la carpeta del proyecto en VS Code.
2. Cuando aparezca la notificación *"Reopen in Container"*, acéptala (o usa la
   paleta de comandos: `Dev Containers: Reopen in Container`).
3. La primera vez, la construcción tarda varios minutos: instala R
   ([rocker/r-ver](https://hub.docker.com/r/rocker/r-ver) 4.3.3), Node.js 20,
   y luego corre `renv::restore()` para instalar exactamente las versiones de
   paquetes de R fijadas en [renv.lock](renv.lock), registra el kernel de
   Jupyter (`ir`) y instala `mystmd`. Las siguientes veces es casi instantáneo
   (la caché de `renv` se conserva en un volumen de Docker).
4. Abre un notebook, elige el kernel **R**, y ejecuta las celdas
   normalmente.


Si se ve errores al cargar
paquetes o versiones que no se actualizan:

1. Cierra VS Code y borra la caché de renv y la librería vieja:

   ```bash
   docker volume rm social-support-entrapment-renv-cache
   ```

   Y borra la carpeta local `renv/library/` (no está versionada, se regenera
   sola).

2. Reconstruye el contenedor desde cero:
   `Dev Containers: Rebuild Container Without Cache`.

### Actualizar paquetes de R

Si agregas una librería nueva al notebook, instálala dentro del Dev Container
y luego fija la versión en el lockfile:

```r
install.packages("nombre_paquete")
renv::snapshot()
```

## Construir el book localmente

Dentro del Dev Container (ya trae Node.js y `mystmd` instalados):

```bash
myst start --keep-host --server-port 3100
```

```bash
myst build --html
```
