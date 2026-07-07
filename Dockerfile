# Usar la imagen base de Rocker Shiny
FROM rocker/shiny:4.3.2

# 1. Instalar dependencias del sistema necesarias para R y paquetes específicos
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libxt6 \
    libz-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# 2. Instalar todas tus librerías de R desde el repositorio binario de Posit
RUN R -e "install.packages(c('shiny', 'readxl', 'haven', 'tools', 'rmarkdown', 'bs4Dash', 'dplyr', 'ggplot2', 'htmltools'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

# 3. Copiar tu código al directorio que espera Shiny Server
COPY . /srv/shiny-server/

# 4. Exponer el puerto
EXPOSE 3838
