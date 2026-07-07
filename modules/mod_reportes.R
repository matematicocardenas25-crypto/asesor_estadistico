# ==============================================================================
# MÓDULO REPORTES - VERSIÓN CON DEBUGGING Y RUTA FIJA
# ==============================================================================

mod_reportes_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Generar Informe"),
    
    selectInput(ns("formato_reporte"), "Formato de salida:",
                choices = c(
                  "HTML (.html)" = "html",
                  "Word (.docx)" = "docx",
                  "PDF (.pdf)" = "pdf"
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
                      "pdf"  = "pdf",
                      "html")
        paste0("informe_", Sys.Date(), ".", ext)
      },
      
      content = function(file) {
        req(df_global())
        
        datos <- df_global()
        if (is.null(datos) || nrow(datos) == 0) {
          showNotification("No hay datos cargados.", type = "error")
          return(NULL)
        }
        
        # USAR DIRECTORIO DE LA APP (no tempdir que puede fallar en Windows)
        app_dir <- getwd()
        tempReport <- file.path(app_dir, "informe_temp.Rmd")
        
        # Crear el Rmd
        crear_rmd_archivo(
          archivo = tempReport,
          titulo = input$titulo_reporte,
          formato = input$formato_reporte
        )
        
        # VERIFICAR QUE SE CREÓ
        if (!file.exists(tempReport)) {
          showNotification("ERROR: No se pudo crear el archivo Rmd.", type = "error")
          return(NULL)
        }
        
        # Leer y mostrar las primeras lineas para debug (solo en consola R)
        rmd_content <- readLines(tempReport, n = 20)
        cat("=== RMD CREADO EN:", tempReport, "===\n")
        cat(paste(rmd_content, collapse = "\n"), "\n")
        cat("=== FIN PRIMERAS 20 LINEAS ===\n")
        cat("=== TAMANO DEL ARCHIVO:", file.size(tempReport), "bytes ===\n")
        
        # Detectar tipo de variables
        vars_numericas <- names(datos)[sapply(datos, is.numeric)]
        vars_categoricas <- names(datos)[sapply(datos, function(x) is.factor(x) || is.character(x))]
        
        # Renderizar segun formato
        output_format <- switch(input$formato_reporte,
                                "html" = rmarkdown::html_document(toc = TRUE, toc_float = TRUE),
                                "docx" = rmarkdown::word_document(toc = TRUE),
                                "pdf"  = rmarkdown::pdf_document(toc = TRUE),
                                rmarkdown::html_document(toc = TRUE))
        
        params_list <- list(
          titulo = input$titulo_reporte,
          datos = datos,
          vars_numericas = vars_numericas,
          vars_categoricas = vars_categoricas,
          fecha = format(Sys.time(), "%d de %B de %Y, %H:%M")
        )
        
        # Renderizar con tryCatch detallado
        tryCatch({
          
          cat("=== INICIANDO RENDER ===\n")
          cat("Input:", tempReport, "\n")
          cat("Output:", file, "\n")
          cat("Formato:", input$formato_reporte, "\n")
          
          rmarkdown::render(
            input = tempReport,
            output_file = file,
            output_format = output_format,
            params = params_list,
            envir = new.env(parent = globalenv()),
            clean = TRUE
          )
          
          cat("=== RENDER COMPLETADO ===\n")
          cat("=== ARCHIVO GENERADO:", file, "===\n")
          cat("=== TAMANO:", file.size(file), "bytes ===\n")
          
          showNotification("Informe generado exitosamente!", type = "message", duration = 5)
          
        }, error = function(e) {
          msg <- conditionMessage(e)
          cat("=== ERROR EN RENDER ===\n")
          cat(msg, "\n")
          cat("=== FIN ERROR ===\n")
          
          showNotification(paste("Error:", msg), type = "error", duration = 10)
          
          # Crear archivo de error para no dejar vacio
          writeLines(paste("Error al generar informe:\n", msg), file)
        })
        
        # Limpiar archivo temporal Rmd (opcional, comenta si quieres revisarlo)
        # unlink(tempReport)
      }
    )
  })
}

# ==============================================================================
# FUNCION: Crea el archivo Rmd linea por linea
# ==============================================================================

crear_rmd_archivo <- function(archivo, titulo, formato) {
  
  fig_width <- if (formato == "pdf") 7 else 10
  fig_height <- if (formato == "pdf") 5 else 6
  
  lineas <- c(
    "---",
    paste0('title: "', titulo, '"'),
    'author: "Ismael Antonio Cardenas Lopez"',
    'date: "`r params$fecha`"',
    "output: ",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    theme: flatly",
    "    highlight: tango",
    paste0("    fig_width: ", fig_width),
    paste0("    fig_height: ", fig_height),
    "params:",
    '  titulo: "Informe Estadistico"',
    "  datos: NULL",
    "  vars_numericas: NULL",
    "  vars_categoricas: NULL",
    "  fecha: NULL",
    "---",
    "",
    "```{r setup, include=FALSE}",
    'knitr::opts_chunk$set(',
    '  echo = FALSE,',
    '  message = FALSE,',
    '  warning = FALSE,',
    '  fig.align = "center",',
    '  out.width = "100%"',
    ")",
    "library(knitr)",
    "library(ggplot2)",
    "library(dplyr)",
    "```",
    "",
    "# 1. Informacion General de los Datos",
    "",
    "```{r}",
    "n <- nrow(params$datos)",
    "p <- ncol(params$datos)",
    "```",
    "",
    "- **Numero de observaciones:** `r n`",
    "- **Numero de variables:** `r p`",
    "- **Fecha de generacion:** `r params$fecha`",
    "",
    "---",
    "",
    "# 2. Estructura de la Base de Datos",
    "",
    "```{r}",
    'str(params$datos) %>% capture.output() %>% paste(collapse = "\\n") %>% cat()',
    "```",
    "",
    "---",
    "",
    "# 3. Resumen Estadistico General",
    "",
    "```{r}",
    "if (length(params$vars_numericas) > 0) {",
    "  resumen <- params$datos[, params$vars_numericas, drop = FALSE] %>%",
    "    summarise(across(everything(), list(",
    "      Min = ~min(., na.rm = TRUE),",
    "      Q1 = ~quantile(., 0.25, na.rm = TRUE),",
    "      Mediana = ~median(., na.rm = TRUE),",
    "      Media = ~mean(., na.rm = TRUE),",
    "      Q3 = ~quantile(., 0.75, na.rm = TRUE),",
    "      Max = ~max(., na.rm = TRUE),",
    "      DE = ~sd(., na.rm = TRUE),",
    "      CV = ~sd(., na.rm = TRUE) / abs(mean(., na.rm = TRUE)) * 100",
    '    ), .names = "{.col}_{.fn}"))',
    "  ",
    "  print(resumen)",
    "}",
    "```",
    "",
    "---",
    "",
    '# 4. Analisis por Variable Numerica',
    "",
    '```{r, results="asis"}',
    "if (length(params$vars_numericas) > 0) {",
    '  for (var in params$vars_numericas) {',
    '    cat("## Variable:", var, "\\n\\n")',
    "    ",
    "    x <- params$datos[[var]]",
    "    x_clean <- na.omit(x)",
    "    ",
    "    # Estadisticos descriptivos",
    "    desc <- data.frame(",
    '      Estadistico = c("n", "Media", "Mediana", "DE", "Minimo", "Maximo", "Q1", "Q3", "CV (%)"),',
    "      Valor = c(",
    "        length(x_clean),",
    "        round(mean(x_clean), 4),",
    "        round(median(x_clean), 4),",
    "        round(sd(x_clean), 4),",
    "        round(min(x_clean), 4),",
    "        round(max(x_clean), 4),",
    "        round(quantile(x_clean, 0.25), 4),",
    "        round(quantile(x_clean, 0.75), 4),",
    '        round(sd(x_clean) / abs(mean(x_clean)) * 100, 2)',
    "      )",
    "    )",
    "    ",
    "    print(desc)",
    "    ",
    "    # Histograma",
    '    p <- ggplot(params$datos, aes_string(x = var)) +',
    '      geom_histogram(aes(y = ..density..), bins = 30, fill = "#667eea", color = "white", alpha = 0.7) +',
    '      geom_density(color = "#764ba2", size = 1) +',
    '      labs(title = paste("Distribucion de", var), x = var, y = "Densidad") +',
    "      theme_minimal() +",
    '      theme(plot.title = element_text(hjust = 0.5, face = "bold"))',
    "    ",
    "    print(p)",
    '    cat("\\n\\n---\\n\\n")',
    "  }",
    "}",
    "```",
    "",
    "---",
    "",
    "# 5. Analisis por Variable Categorica",
    "",
    '```{r, results="asis"}',
    "if (length(params$vars_categoricas) > 0) {",
    '  for (var in params$vars_categoricas) {',
    '    cat("## Variable:", var, "\\n\\n")',
    "    ",
    '    tabla <- table(params$datos[[var]], useNA = "ifany") %>%',
    "      as.data.frame() %>%",
    '      setNames(c("Categoria", "Frecuencia")) %>%',
    "      mutate(Porcentaje = round(Frecuencia / sum(Frecuencia) * 100, 2))",
    "    ",
    "    print(tabla)",
    "    ",
    "    # Grafico de barras",
    '    p <- ggplot(tabla, aes(x = reorder(Categoria, -Frecuencia), y = Frecuencia, fill = Categoria)) +',
    '      geom_bar(stat = "identity", show.legend = FALSE) +',
    "      coord_flip() +",
    '      labs(title = paste("Frecuencias de", var), x = var, y = "Frecuencia") +',
    "      theme_minimal() +",
    '      theme(plot.title = element_text(hjust = 0.5, face = "bold"))',
    "    ",
    "    print(p)",
    '    cat("\\n\\n---\\n\\n")',
    "  }",
    "}",
    "```",
    "",
    "---",
    "",
    "# 6. Matriz de Correlacion (Variables Numericas)",
    "",
    "```{r}",
    "if (length(params$vars_numericas) >= 2) {",
    '  mat_corr <- cor(params$datos[, params$vars_numericas], use = "complete.obs")',
    "  print(round(mat_corr, 3))",
    "}",
    "```",
    "",
    "---",
    "",
    "# 7. Prueba de Normalidad (Shapiro-Wilk)",
    "",
    "```{r}",
    "if (length(params$vars_numericas) > 0) {",
    "  normalidad <- lapply(params$vars_numericas, function(var) {",
    "    x <- na.omit(params$datos[[var]])",
    "    if (length(x) >= 3 && length(x) <= 5000) {",
    "      test <- shapiro.test(x)",
    "      data.frame(",
    "        Variable = var,",
    "        W = round(test$statistic, 4),",
    '        p_valor = format(test$p.value, digits = 4, scientific = TRUE),',
    '        Normalidad = ifelse(test$p.value > 0.05, "Si (p > 0.05)", "No (p <= 0.05)")',
    "      )",
    "    } else {",
    "      data.frame(",
    "        Variable = var,",
    "        W = NA,",
    "        p_valor = NA,",
    '        Normalidad = "Muestra fuera de rango (3-5000)"',
    "      )",
    "    }",
    "  })",
    "  ",
    "  do.call(rbind, normalidad) %>% print()",
    "}",
    "```",
    "",
    "---",
    "",
    "# 8. Datos Completos",
    "",
    "```{r}",
    "if (nrow(params$datos) <= 1000) {",
    "  print(head(params$datos, 100))",
    "} else {",
    '  cat("*Base de datos con mas de 1000 filas. Se muestran las primeras 50.*\\n\\n")',
    "  print(head(params$datos, 50))",
    "}",
    "```",
    "",
    "---",
    "",
    "*Informe generado automaticamente por la Plataforma Analitica Avanzada - Ismael Antonio Cardenas Lopez*"
  )
  
  # Escribir con useBytes = TRUE para evitar problemas de encoding
  writeLines(lineas, archivo, useBytes = TRUE)
  
  # Verificar que se escribió
  if (file.exists(archivo)) {
    cat("Archivo Rmd creado exitosamente:", archivo, "\n")
    cat("Tamano:", file.size(archivo), "bytes\n")
  } else {
    cat("ERROR: El archivo no se creo:", archivo, "\n")
  }
}
