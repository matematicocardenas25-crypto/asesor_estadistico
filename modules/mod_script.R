# UI
mod_script_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    h3("💻 Script reproducible en R"),
    tags$pre(style="background:#f4f4f4; padding:15px;",
             verbatimTextOutput(ns("script")))
  )
}
# SERVER
mod_script_server <- function(id, df, vars, input_main){
  moduleServer(id, function(input, output, session){

    output$script <- renderText({

      req(df(), vars())

      v <- vars()
      cruce <- input_main$tipo_cruce
      enfoque <- input_main$tipo_enfoque

      # =============================
      # BASE
      # =============================
      script <- "data <- read.csv('archivo.csv')\n\n"

      # =============================
      # UNIVARIADO
      # =============================
      if(cruce == "uni" && length(v) == 1){

        script <- paste0(script,
          "# Variable seleccionada\n",
          "x <- data[['", v[1], "']]\n\n"
        )

        script <- paste0(script,
          "# Limpieza\n",
          "x <- na.omit(x)\n\n"
        )

        script <- paste0(script,
          "# Tipo de variable\n",
          "is.numeric(x)\n\n"
        )

        script <- paste0(script,
          "# =============================\n",
          "# Análisis descriptivo\n",
          "# =============================\n",
          "mean(x)\nmedian(x)\nsd(x)\nsummary(x)\n\n"
        )

        script <- paste0(script,
          "# Gráfico\n",
          "hist(x)\nboxplot(x)\n\n"
        )

        if(enfoque == "inf"){

          script <- paste0(script,
            "# =============================\n",
            "# Análisis inferencial\n",
            "# =============================\n",
            "shapiro.test(x)\n",
            "t.test(x)\n\n"
          )
        }
      }

      # =============================
      # BIVARIADO
      # =============================
      if(cruce == "bi" && length(v) == 2){

        script <- paste0(script,
          "# Variables seleccionadas\n",
          "x <- data[['", v[1], "']]\n",
          "y <- data[['", v[2], "']]\n\n"
        )

        script <- paste0(script,
          "# =============================\n",
          "# Análisis bivariado\n",
          "# =============================\n"
        )

        script <- paste0(script,
          "# Tabla cruzada\n",
          "table(x, y)\n\n"
        )

        script <- paste0(script,
          "# Gráfico\n",
          "plot(x, y)\n\n"
        )

        if(enfoque == "inf"){

          script <- paste0(script,
            "# =============================\n",
            "# Pruebas inferenciales\n",
            "# =============================\n",
            "cor.test(x, y)\n",
            "lm(y ~ x)\n\n"
          )
        }
      }

      # =============================
      # FINAL
      # =============================
      paste0(
        "# ======================================\n",
        "# SCRIPT GENERADO AUTOMÁTICAMENTE\n",
        "# ======================================\n\n",
        script
      )

    })

  })
}
