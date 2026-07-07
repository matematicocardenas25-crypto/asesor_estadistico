# UI
mod_datos_agrupados_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    h2("📊 Datos agrupados"),
    
    selectInput(
      ns("variable"),
      "Variable numérica:",
      choices = NULL
    ),
    
    radioButtons(
      ns("metodo"),
      "Método de agrupación:",
      choices = c(
        "Sturges" = "sturges",
        "Rice" = "rice",
        "Scott" = "scott",
        "Freedman-Diaconis" = "fd",
        "Comparar métodos" = "comparar"
      ),
      selected = "fd"
    ),
    
    checkboxInput(
      ns("modo_estudiante"),
      "📘 Explicar procedimiento",
      TRUE
    ),
    
    actionButton(
      ns("calcular"),
      "✅ Construir tabla"
    ),
    
    br(), br(),
    
    h3("🔎 Diagnóstico"),
    tableOutput(ns("diagnostico")),
    verbatimTextOutput(ns("diagnostico_texto")),
    
    br(),
    
    h3("⚖ Comparación de métodos"),
    tableOutput(ns("comparacion")),
    
    br(),
    
    h3("🏆 Evaluación de métodos"),
    tableOutput(ns("decision")),
    
    br(),
    
    h3("✅ Recomendación"),
    
    verbatimTextOutput(
      ns("comparacion_recomendacion")
    ),
    
    verbatimTextOutput(
      ns("comparacion_recomendacion")
    ),
    
    br(),
    
    uiOutput(ns("titulo_tabla")),
    tableOutput(ns("tabla")),
    verbatimTextOutput(
      ns("resumen_tabla")
    ),
    actionButton(
      ns("analizar_agrupados"),
      "📊 Calcular histograma y medidas agrupadas"
    ),
    br(),
    h3("📈 Histograma agrupado"),
    
    plotOutput(
      ns("histograma"),
      height = "500px"
    ),
    br(),
    h3("📐 Medidas agrupadas"),
    
    tableOutput(
      ns("medidas_agrupadas")
    ),
    br(),
    h3("📖 Explicación"),
    verbatimTextOutput(ns("explicacion")),
    
    br(),
    
    h3("🎓 Conclusión"),
    verbatimTextOutput(ns("conclusion"))
    
  )
}
# SERVER
mod_datos_agrupados_server <- function(id, df_global){
  
  moduleServer(id, function(input, output, session){
    
    observe({
      
      req(df_global())
      
      vars_num <- names(
        Filter(is.numeric, df_global())
      )
      
      updateSelectInput(
        session,
        "variable",
        choices = vars_num
      )
      
    })
    
    obtener_metodo <- function(x, metodo){
      
      n <- length(x)
      
      rango <- max(x) - min(x)
      
      if(metodo == "sturges"){
        
        k <- ceiling(1 + log2(n))
        
      } else if(metodo == "rice"){
        
        k <- ceiling(2 * n^(1/3))
        
      } else if(metodo == "scott"){
        
        h <- 3.5 * sd(x) * n^(-1/3)
        
        k <- ceiling(rango / h)
        
      } else if(metodo == "fd"){
        
        h <- 2 * IQR(x) * n^(-1/3)
        
        k <- ceiling(rango / h)
        
      }
      
      list(
        k = k,
        amplitud = rango / k
      )
    }
    
    # ---------------------------------------------------
    # DIAGNOSTICO
    # ---------------------------------------------------
    output$diagnostico <- renderTable({
      
      req(input$variable)
      
      x <- na.omit(df_global()[[input$variable]])
      
      media <- mean(x)
      
      mediana <- median(x)
      
      desv <- sd(x)
      
      skew <- mean((x-media)^3)/(desv^3)
      
      curtosis <- mean((x-media)^4)/(desv^4) - 3
      
      cv <- desv/media*100
      
      data.frame(
        
        Indicador = c(
          "Observaciones",
          "Mínimo",
          "Máximo",
          "Rango",
          "Rango Intercuartílico",
          "Media",
          "Mediana",
          "Desviación estándar",
          "Asimetría",
          "Curtosis",
          "CV (%)"
        ),
        
        Valor = round(c(
          length(x),
          min(x),
          max(x),
          diff(range(x)),
          IQR(x),
          media,
          mediana,
          desv,
          skew,
          curtosis,
          cv
        ),2)
        
      )
      
    })
    output$diagnostico_texto <- renderText({
      
      req(input$variable)
      
      x <- na.omit(df_global()[[input$variable]])
      
      media <- mean(x)
      
      desv <- sd(x)
      
      skew <- mean((x - media)^3)/(desv^3)
      
      curtosis <- mean((x - media)^4)/(desv^4) - 3
      
      cv <- desv/media*100
      
      Q1 <- quantile(x, 0.25)
      
      Q3 <- quantile(x, 0.75)
      
      IQRx <- IQR(x)
      
      lim_inf <- Q1 - 1.5*IQRx
      
      lim_sup <- Q3 + 1.5*IQRx
      
      n_outliers <- sum(
        x < lim_inf | x > lim_sup
      )
      
      texto <- ""
      
      # Asimetría
      
      if(abs(skew) < 0.5){
        
        texto <- paste0(
          texto,
          "✅ Distribución aproximadamente simétrica.\n\n"
        )
        
      } else if(abs(skew) < 1){
        
        texto <- paste0(
          texto,
          "⚠ Asimetría moderada.\n\n"
        )
        
      } else {
        
        texto <- paste0(
          texto,
          "🔴 Asimetría alta.\n\n"
        )
        
      }
      
      # Curtosis
      
      if(curtosis < 0){
        
        texto <- paste0(
          texto,
          "📉 Distribución platicúrtica.\n\n"
        )
        
      } else if(curtosis > 0){
        
        texto <- paste0(
          texto,
          "📈 Distribución leptocúrtica.\n\n"
        )
        
      } else {
        
        texto <- paste0(
          texto,
          "📊 Distribución mesocúrtica.\n\n"
        )
        
      }
      
      # CV
      
      if(cv < 15){
        
        texto <- paste0(
          texto,
          "✅ Datos homogéneos.\n\n"
        )
        
      } else if(cv < 30){
        
        texto <- paste0(
          texto,
          "⚠ Dispersión moderada.\n\n"
        )
        
      } else {
        
        texto <- paste0(
          texto,
          "🔴 Alta variabilidad. Los datos son heterogéneos.\n\n"
        )
        
      }
      
      texto <- paste0(
        texto,
        "📌 Outliers detectados: ",
        n_outliers
      )
      
      texto
      
    })
    
    # ---------------------------------------------------
    # COMPARACION
    # ---------------------------------------------------
    
    output$comparacion <- renderTable({
      
      req(input$variable)
      
      x <- na.omit(df_global()[[input$variable]])
      
      st <- obtener_metodo(x,"sturges")
      ri <- obtener_metodo(x,"rice")
      sc <- obtener_metodo(x,"scott")
      fd <- obtener_metodo(x,"fd")
      
      data.frame(
        
        Metodo = c(
          "Sturges",
          "Rice",
          "Scott",
          "Freedman-Diaconis"
        ),
        
        Clases = c(
          st$k,
          ri$k,
          sc$k,
          fd$k
        ),
        
        Amplitud = round(c(
          st$amplitud,
          ri$amplitud,
          sc$amplitud,
          fd$amplitud
        ),2)
        
      )
      
    })
    
    # ---------------------------------------------------
    # EVALUACION
    # ---------------------------------------------------
    
    output$decision <- renderTable({
      
      req(input$variable)
      
      x <- na.omit(df_global()[[input$variable]])
      
      skew <- mean(
        (x - mean(x))^3
      )/(sd(x)^3)
      
      if(abs(skew) > 1){
        
        data.frame(
          
          Metodo = c(
            "Sturges",
            "Rice",
            "Scott",
            "Freedman-Diaconis"
          ),
          
          Estado = c(
            "❌ No recomendado",
            "⚠ Aceptable",
            "⚠ Aceptable",
            "✅ Recomendado"
          ),
          
          Justificacion = c(
            "Muy pocas clases para esta distribución",
            "Mayor detalle",
            "Sensible a valores extremos",
            "Robusto al sesgo y outliers"
          )
          
        )
        
      } else {
        
        data.frame(
          
          Metodo = c(
            "Sturges",
            "Rice",
            "Scott",
            "Freedman-Diaconis"
          ),
          
          Estado = c(
            "⚠ Aceptable",
            "✅ Recomendado",
            "⚠ Aceptable",
            "⚠ Aceptable"
          ),
          
          Justificacion = c(
            "Método clásico",
            "Buen equilibrio entre detalle y simplicidad",
            "Mayor cantidad de clases",
            "Más robusto"
          )
          
        )
        
      }
      
    })
    
    # ---------------------------------------------------
    # RECOMENDACION
    # ---------------------------------------------------
    output$comparacion_recomendacion <- renderText({
      
      req(input$variable)
      
      x <- na.omit(df_global()[[input$variable]])
      
      skew <- mean(
        (x-mean(x))^3
      )/(sd(x)^3)
      
      metodo_usuario <- input$metodo
      
      metodo_recomendado <- ""
      
      if(abs(skew) > 1){
        
        metodo_recomendado <- "fd"
        
      } else if(length(x) < 100){
        
        metodo_recomendado <- "sturges"
        
      } else if(length(x) > 1000){
        
        metodo_recomendado <- "scott"
        
      } else {
        
        metodo_recomendado <- "rice"
        
      }
      
      if(
        metodo_usuario != "comparar" &&
        metodo_usuario != metodo_recomendado
      ){
        
        return(
          
          paste(
            
            "⚠ ADVERTENCIA\n\n",
            
            "Método seleccionado:",
            metodo_usuario,
            
            "\n\nMétodo recomendado:",
            metodo_recomendado,
            
            "\n\nLa agrupación puede no ser la más adecuada para esta distribución."
            
          )
          
        )
        
      }
      
      ""
      
    })
    output$recomendacion <- renderText({
      
      req(input$variable)
      
      x <- na.omit(df_global()[[input$variable]])
      
      n <- length(x)
      
      media <- mean(x)
      
      skew <- mean((x-media)^3)/(sd(x)^3)
      
      if(abs(skew) > 1){
        
        return(
          paste(
            "✅ MÉTODO RECOMENDADO: FREEDMAN-DIACONIS\n\n",
            "La distribución presenta una asimetría importante.\n",
            "Este método utiliza el rango intercuartílico (IQR),\n",
            "por lo que es más robusto frente a valores extremos."
          )
        )
        
      }
      
      if(n < 100){
        
        return(
          paste(
            "✅ MÉTODO RECOMENDADO: STURGES\n\n",
            "La muestra es pequeña y la interpretación es sencilla."
          )
        )
        
      }
      
      if(n > 1000){
        
        return(
          paste(
            "✅ MÉTODO RECOMENDADO: SCOTT\n\n",
            "La muestra es grande y requiere mayor detalle."
          )
        )
        
      }
      
      paste(
        "✅ MÉTODO RECOMENDADO: RICE\n\n",
        "Balance adecuado entre simplicidad y detalle."
      )
      
    })
    
    # ---------------------------------------------------
    # TABLA DE FRECUENCIAS
    # ---------------------------------------------------
    
    tabla_freq <- eventReactive(input$calcular,{
      
      req(input$variable)
      
      x <- na.omit(df_global()[[input$variable]])
      
      metodo <- input$metodo
      
      if(metodo == "comparar"){
        metodo <- "fd"
      }
      
      info <- obtener_metodo(x, metodo)
      
      breaks <- seq(
        min(x),
        max(x),
        length.out = info$k + 1
      )
      
      clases <- cut(
        x,
        breaks,
        include.lowest = TRUE
      )
      
      fi <- as.numeric(table(clases))
      
      Fi <- cumsum(fi)
      
      hi <- fi/sum(fi)
      
      Hi <- cumsum(hi)
      
      Xi <- (
        breaks[-1] +
          breaks[-length(breaks)]
      )/2
      
      PctAcum <- round(Hi * 100, 2)
      
      data.frame(
        Clase = paste0(
          round(breaks[-length(breaks)],2),
          " - ",
          round(breaks[-1],2)
        ),
        Xi = round(Xi,2),
        fi = fi,
        Fi = Fi,
        hi = round(hi,4),
        Hi = round(Hi,4),
        Porcentaje = round(100*hi,2),
        Porcentaje_Acum = PctAcum
      )
      
    })
    output$titulo_tabla <- renderUI({
      
      req(input$variable)
      
      h3(
        paste(
          "📋 Tabla de frecuencias agrupada de:",
          input$variable
        )
      )
      
    })
    output$tabla <- renderTable({
      tabla_freq()
    })
    output$histograma <- renderPlot({
      
      req(input$analizar_agrupados)
      
      x <- na.omit(
        df_global()[[input$variable]]
      )
      
      metodo <- input$metodo
      
      if(metodo == "comparar"){
        metodo <- "fd"
      }
      
      info <- obtener_metodo(
        x,
        metodo
      )
      
      breaks <- seq(
        min(x),
        max(x),
        length.out = info$k + 1
      )
      
      hist(
        x,
        breaks = breaks,
        col = "steelblue",
        border = "white",
        main = paste(
          "Histograma agrupado de",
          input$variable
        ),
        xlab = input$variable
      )
      
    })
    output$medidas_agrupadas <- renderTable({
      
      req(input$analizar_agrupados)
      
      x <- na.omit(
        df_global()[[input$variable]]
      )
      
      metodo <- input$metodo
      
      if(metodo == "comparar"){
        metodo <- "fd"
      }
      
      info <- obtener_metodo(
        x,
        metodo
      )
      
      breaks <- seq(
        min(x),
        max(x),
        length.out = info$k + 1
      )
      
      clases <- cut(
        x,
        breaks = breaks,
        include.lowest = TRUE
      )
      
      fi <- as.numeric(
        table(clases)
      )
      
      Xi <- (
        breaks[-1] +
          breaks[-length(breaks)]
      ) / 2
      
      n <- sum(fi)
      
      # ==================================
      # MEDIA AGRUPADA
      # ==================================
      
      media <- sum(
        Xi * fi
      ) / n
      
      # ==================================
      # MEDIANA AGRUPADA
      # ==================================
      
      Fi <- cumsum(fi)
      
      clase_mediana <- which(
        Fi >= n/2
      )[1]
      
      Li_med <- breaks[
        clase_mediana
      ]
      
      Fa <- if(
        clase_mediana == 1
      ) 0 else Fi[
        clase_mediana - 1
      ]
      
      fm <- fi[
        clase_mediana
      ]
      
      A <- breaks[
        clase_mediana + 1
      ] - breaks[
        clase_mediana
      ]
      
      mediana <- Li_med +
        ((n/2 - Fa)/fm) * A
      
      # ==================================
      # MODA AGRUPADA
      # ==================================
      
      clase_modal <- which.max(fi)
      
      Li_mod <- breaks[
        clase_modal
      ]
      
      f1 <- fi[
        clase_modal
      ]
      
      f0 <- if(
        clase_modal == 1
      ) 0 else fi[
        clase_modal - 1
      ]
      
      f2 <- if(
        clase_modal == length(fi)
      ) 0 else fi[
        clase_modal + 1
      ]
      
      moda <- Li_mod +
        (
          (f1-f0) /
            ((f1-f0)+(f1-f2))
        ) * A
      
      # ==================================
      # DISPERSION
      # ==================================
      
      varianza <- sum(
        fi * (Xi - media)^2
      )/(n - 1)
      
      desviacion <- sqrt(
        varianza
      )
      
      cv <- 100 *
        desviacion /
        media
      
      # ==================================
      # TABLA FINAL
      # ==================================
      
      data.frame(
        
        Medida = c(
          
          "Media agrupada",
          
          "Mediana agrupada",
          
          "Moda agrupada",
          
          "Varianza agrupada",
          
          "Desviación estándar agrupada",
          
          "Coeficiente de variación (%)"
          
        ),
        
        Valor = round(
          
          c(
            
            media,
            
            mediana,
            
            moda,
            
            varianza,
            
            desviacion,
            
            cv
            
          ),
          
          2
          
        )
        
      )
      
    })
    
    # ---------------------------------------------------
    # EXPLICACION
    # ---------------------------------------------------
    
    output$explicacion <- renderText({
      
      if(!input$modo_estudiante){
        return("")
      }
      
      paste(
        "Xi: Marca de clase\n",
        "fi: Frecuencia absoluta\n",
        "Fi: Frecuencia acumulada\n",
        "hi: Frecuencia relativa\n",
        "Hi: Frecuencia relativa acumulada\n",
        "%: Porcentaje dentro del intervalo"
      )
      
    })
    
    # ---------------------------------------------------
    # CONCLUSION
    # ---------------------------------------------------
    output$conclusion <- renderText({
      
      req(input$variable)
      
      x <- na.omit(df_global()[[input$variable]])
      
      req(length(x) > 1)
      
      media <- mean(x)
      
      mediana <- median(x)
      
      desv <- sd(x)
      
      if(desv == 0){
        
        return(
          "No es posible generar una conclusión porque la variable no presenta variabilidad."
        )
        
      }
      
      cv <- desv/media * 100
      
      skew <- mean((x - media)^3)/(desv^3)
      
      curtosis <- mean((x - media)^4)/(desv^4) - 3
      
      Q1 <- quantile(x, 0.25)
      
      Q3 <- quantile(x, 0.75)
      
      IQRx <- IQR(x)
      
      lim_inf <- Q1 - 1.5 * IQRx
      
      lim_sup <- Q3 + 1.5 * IQRx
      
      n_outliers <- sum(
        x < lim_inf | x > lim_sup
      )
      
      texto <- paste0(
        "La variable presenta una media de ",
        round(media, 2),
        " y una mediana de ",
        round(mediana, 2),
        ".\n\n"
      )
      
      if(media > mediana){
        
        texto <- paste0(
          texto,
          "La media es superior a la mediana, indicando asimetría positiva.\n\n"
        )
        
      } else {
        
        texto <- paste0(
          texto,
          "La media es inferior a la mediana, indicando asimetría negativa.\n\n"
        )
        
      }
      
      texto <- paste0(
        texto,
        "La desviación estándar es de ",
        round(desv, 2),
        ".\n\n",
        "El coeficiente de variación es de ",
        round(cv, 2),
        "%.\n\n"
      )
      
      if(cv > 30){
        
        texto <- paste0(
          texto,
          "Los datos son heterogéneos y presentan alta dispersión.\n\n"
        )
        
      } else {
        
        texto <- paste0(
          texto,
          "Los datos presentan dispersión moderada.\n\n"
        )
        
      }
      
      if(curtosis > 0){
        
        texto <- paste0(
          texto,
          "La distribución es leptocúrtica.\n\n"
        )
        
      } else if(curtosis < 0){
        
        texto <- paste0(
          texto,
          "La distribución es platicúrtica.\n\n"
        )
        
      } else {
        
        texto <- paste0(
          texto,
          "La distribución es mesocúrtica.\n\n"
        )
        
      }
      
      texto <- paste0(
        texto,
        "Se detectaron ",
        n_outliers,
        " posibles valores extremos.\n\n"
      )
      
      if(abs(skew) > 1){
        
        texto <- paste0(
          texto,
          "Por estas características se recomienda utilizar el método Freedman-Diaconis."
        )
        
      } else {
        
        texto <- paste0(
          texto,
          "No se observaron asimetrías severas."
        )
        
      }
      
      return(texto)
      
    })
    
    # cierre moduleServer
  })
  
  # cierre función principal
}

