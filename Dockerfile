# 1. Usar una imagen que YA trae Shiny preinstalado (Ahorra muchísima RAM)
FROM rocker/shiny:4.3.2

# 2. Instalar dependencias del sistema necesarias
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 3. Instalar los paquetes adicionales usando binarios precompilados de Posit (Ultra rápido)
RUN R -e "install.packages(c('bs4Dash', 'dplyr', 'ggplot2', 'htmltools'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

# 4. Crear el directorio y copiar los archivos
RUN mkdir /app
COPY . /app
WORKDIR /app

# 5. Puerto predeterminado
EXPOSE 8080

# 6. Comando de ejecución
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '8080')))" ]
