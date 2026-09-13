# Apoyo social, entrapment y género — Atlántico

Book en [MyST](https://mystmd.org/) para el proyecto de Seminario Investigativo
*"El género en la relación entre apoyo social y entrapment en adolescentes
escolarizados de municipios del Atlántico"* (Natalia Alvarado y Luis Cabarcas).

## Estructura

```
.devcontainer/         Definición del entorno de desarrollo (Dev Container)
data/                  Datos (df_moderation_2026.csv)
notebooks/eda.ipynb    Análisis Exploratorio de Datos (kernel de R)
index.md               Portada del book
myst.yml               Configuración del proyecto/sitio MyST
renv.lock               Versiones exactas de los paquetes de R
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

Esto es válido igual en Linux que en Windows — ambos abren el mismo
`devcontainer.json` y obtienen el mismo entorno, sin pasos manuales
distintos por sistema operativo.

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

Esto levanta un servidor local con recarga en caliente (VS Code reenvía los
puertos 3000 y 3100 automáticamente). El flag `--keep-host` es necesario
porque, sin él, `myst start` sobrescribe la variable `HOST` a `localhost`,
que en el Dev Container resuelve a IPv6 y hace que la página se quede
cargando indefinidamente; `--keep-host` respeta el `HOST=0.0.0.0` ya
definido en [devcontainer.json](.devcontainer/devcontainer.json).

`myst start` en realidad levanta **dos** servidores: el del sitio (puerto
3000) y un "content server" interno (puerto 3100) que sirve las imágenes
generadas por los notebooks (gráficas de R). Si ese segundo puerto no está
fijado y reenviado, las gráficas no cargan y solo se ve el texto alternativo
`plot without title`. Por eso se fija con `--server-port 3100` (también vía
`SERVER_PORT=3100` en [devcontainer.json](.devcontainer/devcontainer.json))
y se agrega a `forwardPorts`.

Para generar el sitio estático (lo que hace el pipeline de CI), que sirve
todo desde un solo puerto y no tiene este problema:

```bash
myst build --html
```
