# ==============================================================================
# MÓDULO REPORTES - VERSIÓN ADAPTADA PARA RENDER (SOLO HTML Y WORD)
# ==============================================================================

mod_reportes_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Generar Informe"),
    
    selectInput(ns("formato_reporte"), "Formato de salida:",
                choices = c(
                  "HTML (.html)" = "html",
                  "Word (.docx)" = "docx"
                  # Removido PDF ya que no hay LaTeX en el servidor
                ),
                selected = "html"),
    
    textInput(ns("titulo_reporte"), "Titulo del informe:", 
              value = "Informe Estadistico"),
    
    downloadButton(ns("descargar_reporte"), "Generar y descargar informe")
  )
}

mod_reportes_server <- function(id, df_global, vars) {
  moduleServer(id, function(input, output, session) {
    
    output$descargar_reporte <- downloadHandler(
      
      filename = function() {
        ext <- switch(input$formato_reporte,
                      "html" = "html",
                      "docx" = "docx",
                      "html")
        paste0("informe_", Sys.Date(), ".", ext)
      },
      
      content = function(file) {
        req(df_global())
        
        datos <- df_global()
        if (is.null(datos) || nrow(datos) == 0) {
          # Retornamos un archivo de texto simple indicando que no hay datos
          writeLines("No hay datos cargados para generar el informe.", file)
          return()
        }
        
        # 1. Usar un directorio temporal seguro y aislado
        temp_dir <- tempdir()
        tempReport <- file.path(temp_dir, "reporte.Rmd")
        
        # Crear el Rmd
        crear_rmd_archivo(
          archivo = tempReport,
          titulo = input$titulo_reporte
        )
        
        # Detectar tipo de variables
        vars_numericas <- names(datos)[sapply(datos, is.numeric)]
        vars_categoricas <- names(datos)[sapply(datos, function(x) is.factor(x) || is.character(x))]
        
        output_format <- switch(input$formato_reporte,
                                "html" = rmarkdown::html_document(toc = TRUE, toc_float = TRUE, theme = "flatly"),
                                "docx" = rmarkdown::word_document(toc = TRUE),
                                rmarkdown::html_document(toc = TRUE))
        
        params_list <- list(
          titulo = input$titulo_reporte,
          datos = datos,
          vars_numericas = vars_numericas,
          vars_categoricas = vars_categoricas,
          fecha = format(Sys.time(), "%d de %B de %Y, %H:%M")
        )
        
        # 2. Renderizar sin usar 'file' directamente
        tryCatch({
          # render() genera el archivo con su extensión correcta en temp_dir
          out_file <- rmarkdown::render(
            input = tempReport,
            output_format = output_format,
            params = params_list,
            envir = new.env(parent = globalenv()),
            clean = TRUE
          )
          
          # 3. Copiar el archivo ya terminado al flujo de descarga de Shiny
          file.copy(out_file, file)
          
        }, error = function(e) {
          # Si RMarkdown falla, el usuario descarga un txt con el error en lugar de que se caiga la app
          msg <- conditionMessage(e)
          writeLines(paste("Error analítico al compilar el informe:\n", msg), file)
        })
      }
    )
  })
}
                                         
