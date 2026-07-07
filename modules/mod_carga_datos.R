# Ui
mod_carga_datos_ui <- function(id){
  ns <- NS(id)
  
  fileInput(ns("datos"), "Cargar archivo",
            accept = c(".csv", ".xlsx", ".xls", ".txt", ".sav", ".dta", ".sas7bdat"))
}

# SERVER 
mod_carga_datos_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    df <- reactive({
      req(input$datos)
      
      archivo <- input$datos$datapath
      ext <- tolower(tools::file_ext(input$datos$name))
      
      tryCatch({
        
        if (ext == "csv") return(read.csv(archivo, stringsAsFactors = TRUE))
        if (ext %in% c("xlsx", "xls")) return(as.data.frame(readxl::read_excel(archivo)))
        if (ext == "txt") return(read.delim(archivo, stringsAsFactors = TRUE))
        if (ext == "sav") return(as.data.frame(haven::read_sav(archivo)))
        if (ext == "dta") return(as.data.frame(haven::read_dta(archivo)))
        if (ext == "sas7bdat") return(as.data.frame(haven::read_sas(archivo)))
        
        stop("Formato no reconocido.")
        
      }, error = function(e) {
        
        showNotification(
          paste("Error al cargar archivo:", e$message),
          type = "error"
        )
        
        return(NULL)
      })
    })
    
    return(df)
  })
}
