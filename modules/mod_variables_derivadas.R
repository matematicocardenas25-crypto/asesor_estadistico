# UI
mod_vars_derivadas_ui <- function(id){
  ns <- NS(id)

  tagList(
    h3("🧮 Crear variable (tipo SPSS)"),

    textInput(ns("nombre"), "Nombre nueva variable:"),

    fluidRow(
      column(6,
             h4("Variables disponibles"),
            selectInput(ns("var_select"), NULL, choices = NULL,
            size = 10, selectize = FALSE),
             actionButton(ns("add_var"), "➕ Agregar variable")
      ),

      column(6,
             h4("Operadores"),
             br(),
             actionButton(ns("sum"), "+"),
             actionButton(ns("res"), "-"),
             actionButton(ns("mul"), "*"),
             actionButton(ns("div"), "/"),
             actionButton(ns("pow"), "^"),
             br(), br(),
             actionButton(ns("abre"), "("),
             actionButton(ns("cierra"), ")"),
             br(), br(),
             actionButton(ns("log"), "log()"),
             actionButton(ns("sqrtf"), "sqrt()"),
             actionButton(ns("expf"), "exp()")
      )
    ),

    br(),

    textAreaInput(ns("formula"), "Fórmula:", rows = 3),

    actionButton(ns("limpiar"), "🧹 Limpiar"),
    actionButton(ns("crear"), "✅ Crear variable"),

    br(), br(),

    h4("Vista previa"),
    tableOutput(ns("preview"))
  )
}
# SEVER
mod_vars_derivadas_server <- function(id, df_global){
  moduleServer(id, function(input, output, session){

    # =============================
    # ACTUALIZAR VARIABLES
    # =============================
    observe({
      req(df_global())
      updateSelectInput(session, "var_select",
                        choices = names(df_global()))
    })

    # =============================
    # INSERTAR TEXTO
    # =============================
    insertar <- function(txt){
      actual <- input$formula
      if(is.null(actual)) actual <- ""
      
      nueva_formula <- paste0(actual, txt)

      updateTextAreaInput(
        session,
        "formula",
        value = nueva_formula
      )
    }

    observeEvent(input$add_var, {
      req(input$var_select)
      insertar(input$var_select)
    })

    observeEvent(input$sum, insertar(" + "))
    observeEvent(input$res, insertar(" - "))
    observeEvent(input$mul, insertar(" * "))
    observeEvent(input$div, insertar(" / "))
    observeEvent(input$pow, insertar(" ^ "))
    observeEvent(input$abre, insertar("("))
    observeEvent(input$cierra, insertar(")"))

    # funciones
    observeEvent(input$log, insertar("log( )"))
    observeEvent(input$sqrtf, insertar("sqrt( )"))
    observeEvent(input$expf, insertar("exp( )"))

    # =============================
    # LIMPIAR
    # =============================
    observeEvent(input$limpiar, {
      updateTextInput(session, "nombre", value = "")
      updateTextAreaInput(session, "formula", value = "")
    })

    # =============================
    # PREVIEW
    # =============================
    output$preview <- renderTable({
      req(df_global(), input$formula)

      data <- df_global()

      tryCatch({
        resultado <- eval(parse(text = input$formula), data)
        head(resultado)
      }, error = function(e){
        NULL
      })
    })

    # =============================
    # CREAR VARIABLE
    # =============================
    observeEvent(input$crear, {

      req(df_global(), input$nombre, input$formula)

      data <- df_global()

      # validar nombre vacío
      if(nchar(input$nombre) == 0){
        showNotification("⚠️ Debe ingresar un nombre", type = "warning")
        return()
      }

      # validar duplicado
      if(input$nombre %in% names(data)){
        showNotification("⚠️ La variable ya existe", type = "warning")
        return()
      }

      # evaluar fórmula
      nueva <- try(
        eval(parse(text = input$formula), data),
        silent = TRUE
      )

      if(inherits(nueva, "try-error")){
        showNotification("❌ Error en la fórmula", type = "error")
        return()
      }

      # validar longitud
      if(length(nueva) != nrow(data)){
        showNotification("⚠️ La fórmula no devuelve una variable válida", type = "warning")
        return()
      }

      # agregar variable
      data[[input$nombre]] <- nueva

      df_global(data)

      # limpiar inputs
      updateTextInput(session, "nombre", value = "")
      updateTextAreaInput(session, "formula", value = "")

      showNotification(
        paste("✅ Variable creada:", input$nombre),
        type = "message"
      )
    })

  })
}
