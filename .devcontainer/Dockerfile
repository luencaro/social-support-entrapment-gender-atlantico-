FROM rocker/r-ver:4.3.3

ENV DEBIAN_FRONTEND=noninteractive

# Debe coincidir con la versión que bootstrapea renv/activate.R, para que la
# imagen no quede desalineada del lockfile.
ENV RENV_VERSION=1.2.4

# La librería del proyecto vive DENTRO del contenedor, no en el bind mount del
# workspace: renv enlaza los paquetes desde su caché con symlinks, y los
# symlinks no funcionan de forma confiable cuando la carpeta está en el sistema
# de archivos de Windows montado por Docker Desktop.
ENV RENV_PATHS_LIBRARY=/opt/renv/library

# Librerías de sistema para compilar tidyverse/gt (ragg, textshaping,
# systemfonts, xml2, curl) y pandoc para las exportaciones de MyST.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        curl pandoc python3-pip \
        libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev \
        libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev \
        libtiff5-dev libjpeg-dev libwebp-dev libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

# Jupyter desde pip con versiones fijas: los paquetes de Ubuntu 22.04 traen
# nbconvert 6.x / jupyter-core 4.x, demasiado antiguos para la ejecución de
# notebooks de MyST.
RUN pip3 install --no-cache-dir \
        "jupyter-core==5.7.2" \
        "jupyter-client==8.6.3" \
        "jupyter-server==2.14.2" \
        "nbformat==5.10.4" \
        "nbclient==0.10.2" \
        "nbconvert==7.16.6"

RUN Rscript -e "install.packages('remotes', repos = 'https://cloud.r-project.org')" \
    && Rscript -e "remotes::install_version('renv', version = Sys.getenv('RENV_VERSION'), repos = 'https://cloud.r-project.org')"

# El kernel de Jupyter arranca en notebooks/, así que no lee el .Rprofile de la
# raíz y renv no se activa. Sin esto, R cae a la librería del sistema y falla la
# primera celda. La ruta se calcula en el build para no fijar a mano la versión
# de R ni la arquitectura.
RUN R_LIB="${RENV_PATHS_LIBRARY}/R-$(Rscript -e 'cat(paste(R.version$major, substr(R.version$minor, 1, 1), sep = "."))')/$(Rscript -e 'cat(R.version$platform)')" \
    && mkdir -p "$R_LIB" \
    && echo ".libPaths(c('$R_LIB', .libPaths()))" >> "$(Rscript -e 'cat(R.home("etc"))')/Rprofile.site"
