# 1. Utilizar una imagen base oficial de R optimizada para producción
FROM rocker/r-ver:4.3.2

# 2. Instalar dependencias del sistema necesarias para paquetes comunes de R
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 3. Instalar los paquetes de R que utiliza la aplicación
# NOTA: Añade dentro del vector c(...) los paquetes específicos de tu Asesor Estadístico
RUN R -e "install.packages(c('shiny', 'bs4Dash', 'dplyr', 'ggplot2', 'htmltools'), repos='https://cloud.r-project.org/')"

# 4. Crear el directorio de trabajo dentro del contenedor y copiar los archivos
RUN mkdir /app
COPY . /app
WORKDIR /app

# 5. Informar el puerto predeterminado (Render interceptará esto)
EXPOSE 8080

# 6. Comando de ejecución: Lee la variable PORT del entorno o usa el 8080 por defecto
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '8080')))" ]