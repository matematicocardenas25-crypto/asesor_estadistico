# 1. Usar la imagen base de Rocker Shiny (evita configurar puertos y CMD manualmente)
FROM rocker/shiny:4.3.2

# 2. Instalación de las dependencias del sistema (Incluyendo todas las que agregaste)
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    pandoc \
    libudunits2-dev \
    libpq-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libz-dev \
    libxt6 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# 3. Instalar los paquetes desde el repositorio de binarios (CRÍTICO para evitar el error de RAM)
RUN R -e "install.packages(c('shiny', 'bs4Dash', 'dplyr', 'ggplot2', 'htmltools', 'readxl', 'haven', 'rmarkdown', 'writexl', 'tools'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

# 4. VERIFICACIÓN: Fallar aquí si shiny no se instaló (Tu excelente idea de control de calidad)
RUN R -e "if (!requireNamespace('shiny', quietly = TRUE)) stop('ERROR FATAL: El paquete shiny no se instaló.')"

# 5. Copiar tu código al directorio nativo que espera Shiny Server
COPY . /srv/shiny-server/

# 6. Exponer el puerto estándar de la imagen rocker/shiny
EXPOSE 3838
