# ==============================================================================
# PLATAFORMA ANALÍTICA AVANZADA - VERSIÓN MODULAR
# Autor: Ismael Antonio Cardenas López
# ==============================================================================

library(shiny)
library(readxl)
library(haven)
library(tools)
library(rmarkdown)

# ==============================
# CARGAR UTILIDADES
# ==============================
source("utils/estadistica.R")
source("utils/outliers.R")

# ==============================
# CARGAR MÓDULOS
# ==============================
source("modules/mod_carga_datos.R")
source("modules/mod_selector_variables.R")
source("modules/mod_exploracion.R")
source("modules/mod_graficos.R")
source("modules/mod_calculos.R")
source("modules/mod_guia.R")
source("modules/mod_info_datos.R")
source("modules/mod_script.R")
source("modules/mod_variables_derivadas.R")
source("modules/mod_reportes.R")
source("modules/mod_historial.R")
source("modules/mod_datos_agrupados.R")
source("modules/mod_normalidad.R")

# ==============================
# CONFIGURACIÓN (Sin LaTeX)
# ==============================
latex_ok <- FALSE  

options(shiny.maxRequestSize = 150 * 1024^2)

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(

  tags$head(
    tags$meta(name = "author", content = "Ismael Antonio Cardenas López"),
    
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #eef2f3 0%, #8e9eab 100%);
        font-family: 'Segoe UI';
      }
      .header-card {
        background: white;
        padding: 25px;
        border-radius: 16px;
        box-shadow: 0 10px 25px rgba(0,0,0,0.1);
        margin: 20px;
        display: flex;
        align-items: center;
        gap: 25px;
      }
      .profile-img {
        border-radius: 50%;
        width: 100px;
        height: 100px;
        border: 3px solid #34495e;
      }
      .welcome-msg {
        background: linear-gradient(135deg, #667eea, #764ba2);
        color: white;
        padding: 25px;
        border-radius: 12px;
        text-align: center;
        margin: 20px;
      }
      .card {
        background: white;
        padding: 20px;
        border-radius: 12px;
        margin-top: 10px;
      }
    "))
  ),

  div(class = "header-card",
      img(src = "foto.jpeg", class = "profile-img"),
      div(
        h3("Ismael Antonio Cardenas López"),
        p("Licenciado en Matemática - UNAN León Nicaragua"),
        span(format(Sys.Date(), "%d de %B de %Y"))
      )
  ),

  div(class = "welcome-msg",
      h2("🎓 Asesor Estadístico Inteligente"),
      p("Análisis descriptivo, inferencial y visualización interactiva de datos")
  ),

  sidebarLayout(
    
    sidebarPanel(
      h4("Carga de datos"),
      mod_carga_datos_ui("datos"),
      
      hr(),
      
      h4("Configuración"),
      radioButtons("tipo_cruce", "Tipo análisis:",
                   choices = c("Univariado"="uni","Bivariado"="bi")),
      
      radioButtons("tipo_enfoque", "Enfoque:",
                   choices = c("Descriptivo"="desc","Inferencial"="inf")),
      
      hr(),
      
      h4("Variables"),
      mod_selector_ui("vars"),
      
      hr(),
      
      mod_reportes_ui("reportes"),
      
      hr(),
      
      h4("Descargas"),
      
      selectInput("formato_descarga", "Formato de descarga:",
                  choices = c(
                    "CSV" = "csv",
                    "Excel (.xlsx)" = "xlsx",
                    "R (RDS)" = "rds",
                    "SPSS (.sav)" = "sav",
                    "Stata (.dta)" = "dta"
                  ),
                  selected = "csv"),
      
      downloadButton("descargar_datos", "📥 Descargar base completa")
    ),
    
    mainPanel(
      tabPanel("📜 Historial",
               div(class="card",
                   mod_historial_ui("historial")
               )
      ),
      
      tabsetPanel(
        
        tabPanel("📘 Guía",
                 div(class = "card",
                     mod_guia_ui("guia")
                 )
        ),
        
        tabPanel("🔍 Exploración",
                 div(class = "card",
                     mod_exploracion_ui("exploracion")
                 )
        ),
        
        tabPanel("🧮 Cálculos",
                 div(class = "card",
                     mod_calculos_ui("calc")
                 )
        ),
        
        tabPanel("📊 Datos agrupados",
                 div(class = "card",
                     mod_datos_agrupados_ui("agrupados")
                 )
        ),
        
        tabPanel("📈 Normalidad",
                 div(class = "card",
                     mod_normalidad_ui("normalidad")
                 )
        ),
        
        tabPanel("📈 Gráficos",
                 div(class = "card",
                     mod_graficos_ui("plot")
                 )
        ),
        
        tabPanel("📂 Datos",
                 div(class = "card",
                     mod_info_ui("info")
                 )
        ),
        
        tabPanel("🧮 Variables",
                 div(class = "card",
                     mod_vars_derivadas_ui("vars_derivadas")
                 )
        ),
        
        tabPanel("💻 Script",
                 div(class = "card",
                     mod_script_ui("script")
                 )
        )
      )
    )
    
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session){
  
  historial <- reactiveValues(log = list())
  df <- mod_carga_datos_server("datos")
  df_global <- reactiveVal(NULL)
  
  observeEvent(df(), {
    df_global(df())
  })
  
  vars <- mod_selector_server("vars", df_global, input)
  mod_vars_derivadas_server("vars_derivadas", df_global)
  
  mod_calculos_server("calc", df_global, vars, input, historial)
  mod_historial_server("historial", historial)
  mod_info_server("info", df_global)
  mod_guia_server("guia", df_global, vars, input)
  mod_exploracion_server("exploracion", df_global)
  mod_graficos_server("plot", df_global, vars)
  mod_script_server("script", df_global, vars, input)
  mod_reportes_server("reportes", df_global, vars)
  mod_datos_agrupados_server("agrupados", df_global)
  mod_normalidad_server("normalidad", df_global)
  
  output$descargar_datos <- downloadHandler(
    
    filename = function(){
      formato <- input$formato_descarga
      if (is.null(formato) || formato == "") {
        formato <- "csv"
      }
      paste0("datos_completos_", Sys.Date(), ".", formato)
    },
    
    content = function(file){
      
      req(df_global())
      data <- df_global()
      
      formato <- input$formato_descarga
      if (is.null(formato) || formato == "") {
        formato <- "csv"
      }
      formato <- as.character(formato)
      
      if (formato == "csv") {
        write.csv(data, file, row.names = FALSE)
      } else if (formato == "xlsx") {
        writexl::write_xlsx(data, path = file)
      } else if (formato == "rds") {
        saveRDS(data, file = file)
      } else if (formato == "sav") {
        haven::write_sav(data, path = file)
      } else if (formato == "dta") {
        haven::write_dta(data, path = file)
      } else {
        write.csv(data, file, row.names = FALSE)
      }
      
    }
  )
  
}

shinyApp(ui, server)
