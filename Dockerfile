FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    pandoc \
    libudunits2-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'bs4Dash', 'dplyr', 'ggplot2', 'htmltools', 'readxl', 'haven', 'rmarkdown', 'writexl'), repos='https://cloud.r-project.org/')"

RUN mkdir /app
COPY . /app
WORKDIR /app

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '8080')))" ]
