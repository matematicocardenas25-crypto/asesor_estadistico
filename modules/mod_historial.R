# UI
mod_historial_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    h3("📜 Historial de análisis"),
    
    tableOutput(ns("tabla_historial")),
    
    br(),
    h4("📊 Detalle del análisis seleccionado"),
    
    tableOutput(ns("detalle")),
    
    br(),
    actionButton(ns("eliminar"), "🗑️ Eliminar último"),
    downloadButton(ns("exportar"), "💾 Exportar historial")
  )
}
mod_historial_server <- function(id, historial){
  moduleServer(id, function(input, output, session){
    
    selected <- reactiveVal(NULL)
    
    # =============================
    # TABLA PRINCIPAL
    # =============================
    output$tabla_historial <- renderTable({
      
      if(length(historial$log) == 0){
        return(data.frame(
          ID = NA,
          Variables = "Sin registros",
          Tipo = NA,
          Enfoque = NA,
          Fecha = NA
        ))
      }
      
      data.frame(
        ID = seq_along(historial$log),
        Variables = sapply(historial$log, function(x) paste(x$variables, collapse=", ")),
        Tipo = sapply(historial$log, function(x) x$tipo),
        Enfoque = sapply(historial$log, function(x) x$enfoque),
        Fecha = sapply(historial$log, function(x) format(x$fecha, "%Y-%m-%d %H:%M:%S"))
      )
    })
    
    # =============================
    # SELECCIÓN AUTOMÁTICA (último)
    # =============================
    observe({
      if(length(historial$log) > 0){
        selected(length(historial$log))
      }
    })
    
    # =============================
    # DETALLE
    # =============================
    output$detalle <- renderTable({
      
      req(selected())
      
      h <- historial$log[[selected()]]
      
      data.frame(
        Campo = c("Variables", "Tipo", "Enfoque", "Fecha"),
        Valor = c(
          paste(h$variables, collapse=", "),
          h$tipo,
          h$enfoque,
          format(h$fecha, "%Y-%m-%d %H:%M:%S")
        )
      )
    })
    
    # =============================
    # ELIMINAR ÚLTIMO REGISTRO
    # =============================
    observeEvent(input$eliminar, {
      
      if(length(historial$log) > 0){
        historial$log <- historial$log[-length(historial$log)]
      }
      
    })
    
    # =============================
    # EXPORTAR HISTORIAL
    # =============================
    output$exportar <- downloadHandler(
      
      filename = function(){
        paste0("historial_", Sys.Date(), ".csv")
      },
      
      content = function(file){
        
        if(length(historial$log) == 0){
          write.csv(data.frame(Mensaje="Sin datos"), file)
          return()
        }
        
        df_hist <- data.frame(
          Variables = sapply(historial$log, function(x) paste(x$variables, collapse=", ")),
          Tipo = sapply(historial$log, function(x) x$tipo),
          Enfoque = sapply(historial$log, function(x) x$enfoque),
          Fecha = sapply(historial$log, function(x) format(x$fecha, "%Y-%m-%d %H:%M:%S"))
        )
        
        write.csv(df_hist, file, row.names = FALSE)
      }
      
    )
    
  })
}
