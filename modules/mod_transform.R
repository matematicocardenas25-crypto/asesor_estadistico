# =============================
# UI
# =============================
mod_transform_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    h3("🔄 Transformación de Variables"),
    
    tableOutput(ns("diagnostico")),
    
    br(),
    uiOutput(ns("opciones")),
    
    actionButton(ns("aplicar"), "✅ Aplicar transformación")
  )
}

# =============================
# SERVER
# =============================
mod_transform_server <- function(id, df, vars){
  moduleServer(id, function(input, output, session){
    
    datos_transformados <- reactiveVal(NULL)
    
    # =============================
    # DIAGNÓSTICO
    # =============================
    output$diagnostico <- renderTable({
      req(df(), vars())
      
      data <- if(!is.null(datos_transformados())){
  datos_transformados()
} else {
  df()
}
      v <- vars()
      
      if(length(v) != 1) return()
      
      x <- data[[v[1]]]
      
      if(!is.numeric(x)){
        return(data.frame(Mensaje="Variable no numérica"))
      }
      
      x <- na.omit(x)
      sk <- calcular_skewness(x)
      
      tipo <- if(abs(sk) < 0.5){
        "Simétrica"
      } else if(sk > 0){
        "Sesgo a la derecha"
      } else {
        "Sesgo a la izquierda"
      }
      
      data.frame(
        Variable = v[1],
        Skewness = round(sk, 3),
        Tipo = tipo
      )
    })
    
    # =============================
    # OPCIONES DINÁMICAS
    # =============================
    output$opciones <- renderUI({
      req(df(), vars())
      
      data <- df()
      v <- vars()
      
      if(length(v) != 1) return()
      
      x <- data[[v[1]]]
      if(!is.numeric(x)) return(NULL)
      
      x <- na.omit(x)
      sk <- calcular_skewness(x)
      
      opciones <- c("Sin transformación"="none")
      
      if(sk > 0){
        opciones <- c(opciones,
                      "Log(x)"="log",
                      "Log(x+1)"="log1",
                      "Raíz cuadrada"="sqrt")
      }
      
      if(sk < 0){
        opciones <- c(opciones,
                      "Reflejar + Log"="ref_log")
      }
      
      selectInput(session$ns("metodo"),
                  "Método sugerido:",
                  choices = opciones)
    })
    
    # =============================
    # APLICAR TRANSFORMACIÓN
    # =============================
    observeEvent(input$aplicar, {
      
      req(df(), vars(), input$metodo)
      
      data <- df()
      v <- vars()
      x <- data[[v[1]]]
      metodo <- input$metodo
      
      # Nombre base para nueva variable
      nombre_base <- v[1]
      
      # =============================
      # TRANSFORMACIONES SEGURAS
      # =============================
      if(metodo == "log"){
        if(any(x <= 0, na.rm=TRUE)){
          showNotification("⚠️ Hay valores <= 0. Usando log(x+1)", type="warning")
          data[[paste0(nombre_base, "_log")]] <- log(x + 1)
        } else {
          data[[paste0(nombre_base, "_log")]] <- log(x)
        }
      }
      
      if(metodo == "log1"){
        data[[paste0(nombre_base, "_log1")]] <- log(x + 1)
      }
      
      if(metodo == "sqrt"){
        if(any(x < 0, na.rm=TRUE)){
          showNotification("⚠️ Hay valores negativos. No se puede aplicar sqrt", type="error")
          return()
        }
        data[[paste0(nombre_base, "_sqrt")]] <- sqrt(x)
      }
      
      if(metodo == "ref_log"){
        max_x <- max(x, na.rm=TRUE)
        data[[paste0(nombre_base, "_ref_log")]] <- log(max_x - x + 1)
      }
      
      if(metodo == "none"){
        showNotification("No se aplicó transformación", type="message")
        return()
      }
      
      # =============================
      # GUARDAR DF NUEVO
      # =============================
      datos_transformados(data)
      
      showNotification(
        paste("✅ Variable creada:", paste0(nombre_base, "_", metodo)),
        type="message"
      )
      
    })
    
    # =============================
    # RETORNO
    # =============================
    return(datos_transformados)
    
  })
}
