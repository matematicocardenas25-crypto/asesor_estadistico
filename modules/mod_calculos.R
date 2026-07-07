# =============================
# UI
# =============================
mod_calculos_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("salida")),
    br(),
    actionButton(ns("guardar"), "💾 Guardar resultado")
  )
}

# =============================
# SERVER
# =============================
mod_calculos_server <- function(id, df, vars, input_main, historial){
  moduleServer(id, function(input, output, session){

    # =============================
    # UI DINÁMICO
    # =============================
    output$salida <- renderUI({

      ns <- session$ns
      req(df(), vars())

      tagList(
        h3("🧮 Resultados Estadísticos"),
        tableOutput(ns("tabla_resultados")),
        br(),
        h3("📌 Pruebas"),
        tableOutput(ns("tabla_pruebas"))
      )
    })

    # =============================
    # TABLA RESULTADOS
    # =============================
    output$tabla_resultados <- renderTable({

      req(df(), vars())

      data <- df()
      v <- vars()
      cruce <- input_main$tipo_cruce

      if(cruce == "uni" && length(v) == 1){

        x <- data[[v[1]]]

        if(is.numeric(x)){

          x <- na.omit(x)

          data.frame(
            Estadistico = c("n", "Media", "Mediana", "Min", "Max",
                            "Varianza", "DesvStd", "IQR"),
            Valor = c(
              length(x),
              mean(x),
              median(x),
              min(x),
              max(x),
              var(x),
              sd(x),
              IQR(x)
            )
          )

        } else {

          tab <- table(x)

          data.frame(
            Categoria = names(tab),
            Frecuencia = as.numeric(tab),
            Proporcion = round(prop.table(tab), 3)
          )
        }

      } else if(cruce == "bi" && length(v) == 2){

        x1 <- data[[v[1]]]
        x2 <- data[[v[2]]]

        if(is.numeric(x1) && is.numeric(x2)){

          data.frame(
            Medida = c("Covarianza", "Correlacion"),
            Valor = c(
              cov(x1, x2, use="complete.obs"),
              cor(x1, x2, use="complete.obs")
            )
          )

        } else {

          as.data.frame(table(x1, x2))
        }
      }
    })

    # =============================
    # TABLA PRUEBAS
    # =============================
    output$tabla_pruebas <- renderTable({

      req(df(), vars())

      data <- df()
      v <- vars()
      cruce <- input_main$tipo_cruce
      enfoque <- input_main$tipo_enfoque

      if(cruce == "uni" && length(v) == 1){

        x <- data[[v[1]]]

        if(is.numeric(x) && enfoque == "inf"){

          x <- na.omit(x)
          n <- length(x)

          sk <- calcular_skewness(x)
          kt <- calcular_kurtosis(x)
          jb <- (n/6)*(sk^2 + (kt^2/4))
          pjb <- 1 - pchisq(jb, df=2)

          data.frame(
            Prueba = c("Shapiro-Wilk", "Jarque-Bera"),
            Estadistico = c(
              if(n <= 50) shapiro.test(x)$statistic else NA,
              jb
            ),
            p_valor = c(
              if(n <= 50) shapiro.test(x)$p.value else NA,
              pjb
            )
          )
        }

      } else if(cruce == "bi" && length(v) == 2){

        x1 <- data[[v[1]]]
        x2 <- data[[v[2]]]

        if(is.numeric(x1) && is.numeric(x2) && enfoque == "inf"){

          test <- cor.test(x1, x2)

          data.frame(
            Prueba = "Correlacion Pearson",
            Estadistico = test$statistic,
            p_valor = test$p.value
          )

        } else if(!is.numeric(x1) && !is.numeric(x2) && enfoque == "inf"){

          test <- chisq.test(table(x1, x2))

          data.frame(
            Prueba = "Chi-cuadrado",
            Estadistico = test$statistic,
            p_valor = test$p.value
          )
        }
      }
    })

    # =============================
    # ✅ GUARDAR (VERSIÓN SEGURA)
    # =============================
    observeEvent(input$guardar, {

      # Validación segura
      if(is.null(vars()) || is.null(df())) return()

      v <- vars()
      cruce <- input_main$tipo_cruce
      enfoque <- input_main$tipo_enfoque

      nuevo <- list(
        variables = v,
        tipo = cruce,
        enfoque = enfoque,
        fecha = Sys.time()
      )

      # Inicializar si está vacío
      if(is.null(historial$log)){
        historial$log <- list()
      }

      # ✅ FORMA SEGURA DE AGREGAR
      historial$log[[length(historial$log) + 1]] <- nuevo

      # ✅ DEBUG OPCIONAL (ver en consola)
      print("✅ Guardado en historial")

    })

  })
}
