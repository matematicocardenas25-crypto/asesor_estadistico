# =============================
# UI
# =============================
mod_exploracion_ui <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("resumen"))
}

# =============================
# SERVER
# =============================
mod_exploracion_server <- function(id, df){
  moduleServer(id, function(input, output, session){

    # =============================
    # UI PRINCIPAL (TABLAS)
    # =============================
    output$resumen <- renderUI({

      ns <- session$ns

      req(df())

      tagList(

        h3("📊 Resumen General"),
        tableOutput(ns("tabla_general")),

        h3("📦 Tipos de Variables"),
        tableOutput(ns("tabla_tipos")),

        h3("🧹 Calidad de Datos"),
        tableOutput(ns("tabla_calidad")),

        h3("📋 Detalle por Variable"),
        tableOutput(ns("tabla_detalle")),

        br(),
        h3("📌 Diagnóstico"),
        verbatimTextOutput(ns("diagnostico"))

      )
    })

    # =============================
    # TABLA GENERAL
    # =============================
    output$tabla_general <- renderTable({
      req(df())
      data <- df()

      data.frame(
        Métrica = c("Filas", "Columnas"),
        Valor = c(nrow(data), ncol(data))
      )
    })

    # =============================
    # TIPOS DE VARIABLES
    # =============================
    output$tabla_tipos <- renderTable({
      req(df())
      data <- df()

      es_cat <- function(x){
        is.factor(x) || is.character(x) || is.logical(x)
      }

      data.frame(
        Tipo = c("Numéricas", "Categóricas"),
        Cantidad = c(
          sum(sapply(data, is.numeric)),
          sum(sapply(data, es_cat))
        )
      )
    })

    # =============================
    # CALIDAD DE DATOS
    # =============================
    output$tabla_calidad <- renderTable({
      req(df())
      data <- df()

      vacios <- sum(sapply(data, function(col){
        if(is.character(col)){
          sum(trimws(col) == "", na.rm = TRUE)
        } else 0
      }))

      data.frame(
        Métrica = c("Total NA", "Celdas vacías"),
        Valor = c(sum(is.na(data)), vacios)
      )
    })

    # =============================
    # DETALLE POR VARIABLE
    # =============================
    output$tabla_detalle <- renderTable({
      req(df())
      data <- df()

      data.frame(
        Variable = names(data),
        Tipo = sapply(data, function(x) class(x)[1]),
        NA_count = sapply(data, function(x) sum(is.na(x))
        ),
        Vacios = sapply(data, function(x){
          if(is.character(x)){
            sum(trimws(x) == "", na.rm = TRUE)
          } else 0
        })
      )
    })

    # =============================
    # DIAGNÓSTICO FINAL
    # =============================
    output$diagnostico <- renderPrint({

      req(df())
      data <- df()

      na_total <- sum(is.na(data))

      vacios <- sum(sapply(data, function(col){
        if(is.character(col)){
          sum(trimws(col) == "", na.rm = TRUE)
        } else 0
      }))

      cat("=========== DIAGNÓSTICO GENERAL ===========\n\n")

      if(na_total == 0 && vacios == 0){
        cat("✅ Base de datos limpia\n")
        cat("✔ No hay valores faltantes (NA)\n")
        cat("✔ No hay celdas vacías\n")
        cat("✔ Lista para análisis estadístico\n")
      } else {
        cat("⚠️ Problemas detectados:\n\n")

        if(na_total > 0){
          cat("❌ Existen valores NA\n")
        }

        if(vacios > 0){
          cat("❌ Existen celdas vacías\n")
        }

        cat("\nRecomendación: realizar limpieza de datos\n")
      }

    })

  })
}
