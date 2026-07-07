#Ui
mod_analisis_ui <- function(id){
  ns <- NS(id)
  
  verbatimTextOutput(ns("output"))
}
# SERVER
mod_analisis_server <- function(id, df, vars){
  moduleServer(id, function(input, output, session){

    output$output <- renderPrint({

      req(df(), vars())

      data <- df()
      v <- vars()

      cat("===== ANÁLISIS ESTADÍSTICO =====\n\n")

      if(length(v) == 1){

        x <- data[[v[1]]]

        if(is.numeric(x)){
          x <- na.omit(x)

          cat("Variable:", v[1], "\n\n")
          cat("Media:", mean(x), "\n")
          cat("Mediana:", median(x), "\n")
          cat("SD:", sd(x), "\n")
          cat("Min:", min(x), "\n")
          cat("Max:", max(x), "\n")

          cat("\nPercentiles:\n")
          print(quantile(x))

        } else {

          cat("Frecuencias:\n")
          print(table(x))
        }

      } else if(length(v) == 2){

        x1 <- data[[v[1]]]
        x2 <- data[[v[2]]]

        if(is.numeric(x1) && is.numeric(x2)){
          cat("Correlación:\n")
          print(cor.test(x1, x2))
        } else {
          cat("Tabla cruzada:\n")
          print(table(x1, x2))
        }
      }

    })

  })
}
