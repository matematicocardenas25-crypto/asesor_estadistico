# 1. Imagen base de R
FROM rocker/r-ver:4.3.2

# 2. Instalar dependencias del sistema (añadidas librerías para compilar shiny y gráficos)
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
    && rm -rf /var/lib/apt/lists/*

# 3. Instalar los paquetes de R
RUN R -e "install.packages(c('shiny', 'bs4Dash', 'dplyr', 'ggplot2', 'htmltools', 'readxl', 'haven', 'rmarkdown', 'writexl'), repos='https://cloud.r-project.org/')"

# 4. VERIFICACIÓN: Fallar aquí si shiny no se instaló
RUN R -e "if (!requireNamespace('shiny', quietly = TRUE)) stop('ERROR FATAL: El paquete shiny no se instaló.')"

# 5. Configurar directorio de la app
RUN mkdir /app
COPY . /app
WORKDIR /app

# 6. Exponer puerto
EXPOSE 8080

# 7. Comando de inicio
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '8080')))" ]
``*
