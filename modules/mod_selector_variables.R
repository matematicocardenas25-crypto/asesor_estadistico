# Ui
mod_selector_ui <- function(id){
  ns <- NS(id)
  
  selectizeInput(ns("vars"),
                 "Selecciona variables",
                 choices = NULL,
                 multiple = TRUE)
}

# SERVER
mod_selector_server <- function(id, df, input_main){
  moduleServer(id, function(input, output, session){
    
    observe({
      req(df())
      
      max_items <- ifelse(input_main$tipo_cruce == "uni", 1, 2)
      
      updateSelectizeInput(
        session, 
        "vars",
        choices = names(df()),
        options = list(maxItems = max_items)
      )
    })
    
    return(reactive(input$vars))
  })
}
