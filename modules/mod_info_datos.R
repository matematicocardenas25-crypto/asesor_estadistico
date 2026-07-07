mod_info_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("info")),
    tableOutput(ns("tabla")),
    verbatimTextOutput(ns("estructura"))
  )
}

mod_info_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    
    output$info <- renderUI({
      req(df())
      data <- df()
      
      div(
        strong(paste("Filas:", nrow(data))),
        br(),
        strong(paste("Columnas:", ncol(data)))
      )
    })
    
    output$tabla <- renderTable({
      req(df())
      head(df(), 20)
    })
    
    output$estructura <- renderPrint({
      req(df())
      str(df())
    })
    
  })
}
