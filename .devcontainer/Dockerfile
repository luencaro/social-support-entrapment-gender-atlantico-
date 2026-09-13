FROM rocker/r-ver:4.3.3

ENV DEBIAN_FRONTEND=noninteractive

# System libraries needed to build tidyverse/gt (ragg, textshaping, systemfonts, xml2, curl)
# plus Jupyter (for the R kernel) and pandoc (used by MyST/rmarkdown exports).
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        curl jupyter-core jupyter-client jupyter-notebook jupyter-nbconvert pandoc \
        libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev \
        libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev \
        libtiff5-dev libjpeg-dev libwebp-dev libuv1-dev \
    && rm -rf /var/lib/apt/lists/* \
    && Rscript -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
