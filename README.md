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

El equipo trabaja en dos sistemas operativos distintos (Linux / Bluefin y
Windows), así que en vez de instalar R, Node.js, Jupyter y `mystmd` a mano en
cada máquina, el proyecto trae un [Dev Container](https://containers.dev/)
([.devcontainer/](.devcontainer)) que arma ese entorno automáticamente e
igual para los dos sistemas operativos.

**Requisitos previos:**

1. [VS Code](https://code.visualstudio.com/) con la extensión
   [Dev Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers).
2. Un motor de contenedores corriendo: Docker Desktop en Windows, o Docker/Podman
   en Linux (en Bluefin ya viene Docker instalado).

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
4. Abre `notebooks/eda.ipynb`, elige el kernel **R**, y ejecuta las celdas
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

Commitea el `renv.lock` actualizado para que tu compañera lo reciba en su
próximo rebuild del contenedor.

## Construir el book localmente

Dentro del Dev Container (ya trae Node.js y `mystmd` instalados):

```bash
myst start
```

Esto levanta un servidor local con recarga en caliente (VS Code reenvía el
puerto 3000 automáticamente). Para generar el sitio estático (lo que hace el
pipeline de CI):

```bash
myst build --html
```
