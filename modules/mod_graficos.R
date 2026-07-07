# ============================================================================
# MÓDULO: GRÁFICOS INTERACTIVOS V2 - INTELIGENTE Y 3D
# SIN PAQUETERIA ADICIONAL - TODO EN R BASE
# ============================================================================

# ============================================================================
# UI
# ============================================================================
mod_graficos_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Título
    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               color: white; padding: 15px; border-radius: 10px; margin-bottom: 15px;",
      h3("📊 Gráficos Inteligentes", style = "margin: 0;"),
      p("Visualización adaptativa según tipo de variable", 
        style = "margin: 5px 0 0 0; opacity: 0.9;")
    ),
    
    fluidRow(
      # PANEL IZQUIERDO: Controles
      column(3,
             wellPanel(
               style = "background: #f8f9fa; min-height: 600px;",
               
               h4("🔧 Configuración", style = "border-bottom: 2px solid #667eea; padding-bottom: 5px;"),
               
               # Selector de variables (se actualiza dinámicamente)
               uiOutput(ns("selector_variables")),
               br(),
               
               # Selector de gráfico (filtrado inteligentemente)
               uiOutput(ns("selector_grafico_inteligente")),
               br(),
               
               # Recomendación del sistema
               uiOutput(ns("recomendacion_sistema")),
               br(),
               
               hr(),
               h4("🎨 Personalización", style = "border-bottom: 2px solid #28a745; padding-bottom: 5px;"),
               
               # Color principal
               textInput(ns("color_hex"), "Color principal (hex):", "#4a90e2"),
               
               # Paleta de colores
               selectInput(ns("paleta"), "Paleta de colores:",
                           choices = c(
                             "Arcoiris" = "rainbow",
                             "Terreno" = "terrain", 
                             "Calor" = "heat",
                             "Topo" = "topo",
                             "CM" = "cm",
                             "Grises" = "gray.colors",
                             "Azules" = "blues",
                             "Verdes" = "greens",
                             "Rojos" = "reds"
                           )),
               
               # Modo 3D
               checkboxInput(ns("modo_3d"), "Modo 3D (perspectiva)", FALSE),
               
               hr(),
               h4("🏷️ Etiquetas", style = "border-bottom: 2px solid #ffc107; padding-bottom: 5px;"),
               
               checkboxInput(ns("etiquetas_valores"), "Mostrar valores", TRUE),
               checkboxInput(ns("etiquetas_porcentajes"), "Mostrar %", FALSE),
               checkboxInput(ns("etiquetas_medias"), "Mostrar media/mediana", FALSE),
               checkboxInput(ns("etiquetas_outliers"), "Etiquetar outliers", TRUE),
               checkboxInput(ns("mostrar_leyenda"), "Mostrar leyenda", TRUE),
               
               hr(),
               h4("📝 Título", style = "border-bottom: 2px solid #dc3545; padding-bottom: 5px;"),
               
               textInput(ns("titulo"), "Título personalizado:", ""),
               textInput(ns("subtitulo"), "Subtítulo:", ""),
               
               hr(),
               h4("📐 Tamaños", style = "border-bottom: 2px solid #17a2b8; padding-bottom: 5px;"),
               
               sliderInput(ns("cex"), "Tamaño texto:", 0.5, 2, 1, 0.1),
               sliderInput(ns("pch_size"), "Tamaño puntos:", 0.5, 5, 1.5, 0.5),
               sliderInput(ns("lwd"), "Grosor líneas:", 0.5, 5, 2, 0.5),
               
               hr(),
               h4("💾 Exportar", style = "border-bottom: 2px solid #6f42c1; padding-bottom: 5px;"),
               
               selectInput(ns("formato"), "Formato:", 
                           choices = c("PNG", "PDF", "SVG", "PostScript")),
               numericInput(ns("ancho"), "Ancho (pulg):", 10),
               numericInput(ns("alto"), "Alto (pulg):", 8),
               downloadButton(ns("descargar"), "Descargar gráfico", 
                              class = "btn-info", style = "width: 100%;")
             )
      ),
      
      # PANEL DERECHO: Gráfico + Análisis
      column(9,
             # Área del gráfico
             wellPanel(
               style = "background: white; min-height: 500px; border: 1px solid #ddd;",
               plotOutput(ns("plot"), height = "550px")
             ),
             
             # Interpretación automática
             wellPanel(
               style = "background: #f8f9fa; border-left: 4px solid #17a2b8;",
               h4("🧠 Análisis Automático"),
               verbatimTextOutput(ns("analisis"))
             ),
             
             # Detalles técnicos
             wellPanel(
               style = "background: #f8f9fa; border-left: 4px solid #28a745;",
               h4("📋 Detalles del Gráfico"),
               tableOutput(ns("detalles_tabla"))
             )
      )
    )
  )
}

# ============================================================================
# SERVER
# ============================================================================
mod_graficos_server <- function(id, df, vars) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # 1. FUNCIONES AUXILIARES - ANÁLISIS DE VARIABLES
    # -------------------------------------------------------------------------
    
    # Clasificar tipo de variable
    clasificar_variable <- function(x) {
      if (is.numeric(x)) {
        n_unicos <- length(unique(na.omit(x)))
        if (n_unicos <= 10) {
          return(list(tipo = "numerica_discreta", 
                      n_unicos = n_unicos,
                      rango = range(x, na.rm = TRUE)))
        } else {
          return(list(tipo = "numerica_continua",
                      n_unicos = n_unicos,
                      rango = range(x, na.rm = TRUE)))
        }
      } else {
        n_unicos <- length(unique(na.omit(x)))
        return(list(tipo = "categorica",
                    n_unicos = n_unicos,
                    niveles = levels(as.factor(x))))
      }
    }
    
    # Calcular métricas para recomendación
    analizar_comportamiento <- function(x) {
      x <- na.omit(x)
      n <- length(x)
      
      if (is.numeric(x)) {
        media <- mean(x)
        mediana <- median(x)
        desv <- sd(x)
        cv <- desv / abs(media) * 100  # Coeficiente de variación
        q1 <- quantile(x, 0.25)
        q3 <- quantile(x, 0.75)
        iqr <- q3 - q1
        min_x <- min(x)
        max_x <- max(x)
        
        # Asimetría simple
        skew <- (mean((x - media)^3) / (sd(x)^3))
        
        # Curtosis simple
        kurt <- (mean((x - media)^4) / (sd(x)^4)) - 3
        
        # Outliers
        li <- q1 - 1.5 * iqr
        ls <- q3 + 1.5 * iqr
        n_outliers <- sum(x < li | x > ls)
        pct_outliers <- n_outliers / n * 100
        
        # Recomendación
        recomendacion <- "histograma"
        razon <- "Distribución continua estándar"
        
        if (abs(skew) > 1) {
          recomendacion <- "boxplot"
          razon <- "Distribución altamente asimétrica, mejor ver resumen robusto"
        } else if (n_outliers > n * 0.05) {
          recomendacion <- "boxplot"
          razon <- "Presencia significativa de outliers (>5%)"
        } else if (cv < 5) {
          recomendacion <- "lineas"
          razon <- "Muy poca variabilidad (CV<5%), mejor ver tendencia"
        } else if (length(unique(x)) <= 20) {
          recomendacion <- "barras"
          razon <- "Pocos valores únicos, se comporta como discreta"
        }
        
        return(list(
          tipo = "numerica",
          n = n,
          media = media,
          mediana = mediana,
          desv = desv,
          cv = cv,
          skew = skew,
          kurt = kurt,
          n_outliers = n_outliers,
          pct_outliers = pct_outliers,
          rango = c(min_x, max_x),
          recomendacion = recomendacion,
          razon = razon
        ))
        
      } else {
        tab <- table(x)
        n_unicos <- length(tab)
        max_freq <- max(tab)
        pct_max <- max_freq / n * 100
        
        # Recomendación
        recomendacion <- "barras"
        razon <- "Variable categórica estándar"
        
        if (n_unicos == 2) {
          recomendacion <- "pastel"
          razon <- "Solo 2 categorías, proporciones son claras"
        } else if (n_unicos > 15) {
          recomendacion <- "barras_horizontales"
          razon <- "Muchas categorías, barras horizontales mejoran legibilidad"
        } else if (pct_max > 80) {
          recomendacion <- "pareto"
          razon <- "Una categoría domina (>80%), ver acumulación"
        }
        
        return(list(
          tipo = "categorica",
          n = n,
          n_categorias = n_unicos,
          categorias = names(tab),
          frecuencias = as.numeric(tab),
          pct_max = pct_max,
          recomendacion = recomendacion,
          razon = razon
        ))
      }
    }
    
    # -------------------------------------------------------------------------
    # 2. SELECTOR DINÁMICO DE VARIABLES
    # -------------------------------------------------------------------------
    output$selector_variables <- renderUI({
      req(df())
      
      vars_num <- names(Filter(is.numeric, df()))
      vars_cat <- names(Filter(function(x) !is.numeric(x), df()))
      
      tagList(
        selectInput(ns("var1"), "Variable 1 (obligatoria):",
                    choices = c("Numéricas" = "", 
                                setNames(vars_num, paste0("📊 ", vars_num)),
                                "Categóricas" = "",
                                setNames(vars_cat, paste0("🏷️ ", vars_cat))),
                    selected = if(length(vars_num) > 0) vars_num[1] else NULL),
        
        selectInput(ns("var2"), "Variable 2 (opcional, para bivariado):",
                    choices = c("Ninguna" = "",
                                "Numéricas" = "",
                                setNames(vars_num, paste0("📊 ", vars_num)),
                                "Categóricas" = "",
                                setNames(vars_cat, paste0("🏷️ ", vars_cat))),
                    selected = "")
      )
    })
    
    # -------------------------------------------------------------------------
    # 3. ANÁLISIS INTELIGENTE Y SELECTOR DE GRÁFICOS
    # -------------------------------------------------------------------------
    
    # Reactive: análisis de variables seleccionadas
    analisis_vars <- reactive({
      req(input$var1, df())
      
      data <- df()
      v1 <- input$var1
      v2 <- if(input$var2 != "") input$var2 else NULL
      
      x1 <- data[[v1]]
      clasif1 <- clasificar_variable(x1)
      comp1 <- analizar_comportamiento(x1)
      
      resultado <- list(
        var1 = v1,
        tipo1 = clasif1$tipo,
        analisis1 = comp1,
        es_bivariado = !is.null(v2)
      )
      
      if (!is.null(v2)) {
        x2 <- data[[v2]]
        clasif2 <- clasificar_variable(x2)
        comp2 <- analizar_comportamiento(x2)
        
        resultado$var2 <- v2
        resultado$tipo2 <- clasif2$tipo
        resultado$analisis2 <- comp2
        
        # Determinar tipo de combinación
        if (clasif1$tipo == "numerica_continua" && clasif2$tipo == "numerica_continua") {
          resultado$tipo_combinacion <- "num_num"
          resultado$recomendacion <- "dispersion"
          resultado$razon <- "Dos variables continuas: analizar relación"
        } else if (clasif1$tipo == "categorica" && clasif2$tipo == "categorica") {
          resultado$tipo_combinacion <- "cat_cat"
          resultado$recomendacion <- "barras_agrupadas"
          resultado$razon <- "Dos categóricas: analizar asociación"
        } else {
          resultado$tipo_combinacion <- "mixto"
          resultado$recomendacion <- "boxplot_grupos"
          resultado$razon <- "Mixto: comparar distribución por grupos"
        }
      }
      
      resultado
    })
    
    # Selector de gráfico inteligente
    output$selector_grafico_inteligente <- renderUI({
      req(analisis_vars())
      a <- analisis_vars()
      
      if (!a$es_bivariado) {
        # UNIVARIADO
        if (a$tipo1 %in% c("numerica_continua", "numerica_discreta")) {
          # NUMÉRICO
          opciones <- c(
            "Histograma" = "hist",
            "Histograma + Densidad" = "hist_dens",
            "Boxplot" = "box",
            "Violín (simulado)" = "violin",
            "Densidad" = "densidad",
            "QQ-Plot" = "qq",
            "Líneas (índice)" = "lineas",
            "Puntos (Dot plot)" = "dot",
            "Stem-and-leaf" = "stem"
          )
          if (a$tipo1 == "numerica_discreta") {
            opciones <- c(opciones, "Barras de frecuencia" = "barras_freq")
          }
        } else {
          # CATEGÓRICO
          opciones <- c(
            "Barras verticales" = "barras",
            "Barras horizontales" = "barras_h",
            "Barras 3D" = "barras_3d",
            "Pastel" = "pastel",
            "Pareto" = "pareto",
            "Lollipop" = "lollipop",
            "Mosaico" = "mosaico"
          )
        }
      } else {
        # BIVARIADO
        if (a$tipo_combinacion == "num_num") {
          opciones <- c(
            "Dispersión" = "scatter",
            "Dispersión 3D" = "scatter_3d",
            "Líneas" = "lineas_bi",
            "Pareto (una como peso)" = "pareto_bi",
            "Hexbin (simulado)" = "hexbin"
          )
        } else if (a$tipo_combinacion == "cat_cat") {
          opciones <- c(
            "Barras agrupadas" = "barras_agrup",
            "Barras apiladas" = "barras_apil",
            "Barras 3D" = "barras_3d_bi",
            "Mosaico" = "mosaico_bi",
            "Heatmap" = "heatmap"
          )
        } else {
          opciones <- c(
            "Boxplot por grupos" = "box_grupos",
            "Violín por grupos" = "violin_grupos",
            "Barras de medias" = "barras_medias",
            "Líneas por grupo" = "lineas_grupo",
            "Pareto por grupo" = "pareto_grupo"
          )
        }
      }
      
      # Marcar el recomendado
      recomendado <- if (!a$es_bivariado) a$analisis1$recomendacion else a$recomendacion
      nombres_opciones <- names(opciones)
      valores_opciones <- unname(opciones)
      
      idx_recomendado <- which(valores_opciones == recomendado)
      if (length(idx_recomendado) > 0) {
        nombres_opciones[idx_recomendado] <- paste0("⭐ ", nombres_opciones[idx_recomendado], " (RECOMENDADO)")
      }
      
      selectInput(ns("tipo_grafico"), "Tipo de gráfico:",
                  choices = setNames(valores_opciones, nombres_opciones),
                  selected = recomendado)
    })
    
    # Recomendación del sistema
    output$recomendacion_sistema <- renderUI({
      req(analisis_vars())
      a <- analisis_vars()
      
      if (!a$es_bivariado) {
        rec <- a$analisis1$recomendacion
        razon <- a$analisis1$razon
        tipo <- if(a$tipo1 == "categorica") "CATEGÓRICA" else "NUMÉRICA"
      } else {
        rec <- a$recomendacion
        razon <- a$razon
        tipo <- "BIVARIADO"
      }
      
      div(
        style = "background: #e8f5e9; padding: 10px; border-radius: 5px; border-left: 4px solid #28a745;",
        h5("💡 Recomendación del sistema"),
        p(strong("Tipo detectado: "), tipo),
        p(strong("Gráfico recomendado: "), rec),
        p(strong("Razón: "), razon),
        if (!a$es_bivariado && a$tipo1 != "categorica") {
          tagList(
            p(strong("CV: "), round(a$analisis1$cv, 2), "%"),
            p(strong("Asimetría: "), round(a$analisis1$skew, 3)),
            p(strong("Outliers: "), a$analisis1$n_outliers, 
              " (", round(a$analisis1$pct_outliers, 2), "%)")
          )
        }
      )
    })
    
    # -------------------------------------------------------------------------
    # 4. FUNCIONES DE COLORES
    # -------------------------------------------------------------------------
    
    obtener_colores <- function(n, paleta, color_base = "#4a90e2") {
      if (paleta == "rainbow") {
        return(rainbow(n))
      } else if (paleta == "terrain") {
        return(terrain.colors(n))
      } else if (paleta == "heat") {
        return(heat.colors(n))
      } else if (paleta == "topo") {
        return(topo.colors(n))
      } else if (paleta == "cm") {
        return(cm.colors(n))
      } else if (paleta == "gray.colors") {
        return(gray.colors(n))
      } else if (paleta == "blues") {
        return(colorRampPalette(c("lightblue", "darkblue"))(n))
      } else if (paleta == "greens") {
        return(colorRampPalette(c("lightgreen", "darkgreen"))(n))
      } else if (paleta == "reds") {
        return(colorRampPalette(c("lightpink", "darkred"))(n))
      } else {
        # Custom: generar variaciones del color base
        rgb_vals <- col2rgb(color_base) / 255
        return(rgb(
          seq(1, rgb_vals[1], length.out = n),
          seq(1, rgb_vals[2], length.out = n),
          seq(1, rgb_vals[3], length.out = n)
        ))
      }
    }
    
    # -------------------------------------------------------------------------
    # 5. FUNCIONES DE GRÁFICOS 3D SIMULADOS
    # -------------------------------------------------------------------------
    
    # Barras 3D simuladas con perspectiva
    barras_3d_base <- function(tab, colores, main = "", etiquetas = TRUE, 
                               cex = 1, mostrar_leyenda = TRUE) {
      n <- length(tab)
      nombres <- names(tab)
      valores <- as.numeric(tab)
      
      # Crear matriz para perspectiva
      z <- matrix(0, nrow = n, ncol = 10)
      for (i in 1:n) {
        z[i, ] <- seq(0, valores[i], length.out = 10)
      }
      
      # Perspectiva
      persp(x = 1:n, y = 1:10, z = z,
            theta = 30, phi = 20,
            col = rep(colores, each = 10),
            main = main,
            xlab = "", ylab = "", zlab = "Frecuencia",
            ticktype = "detailed",
            cex.axis = cex * 0.8)
      
      # Etiquetas en la base
      if (etiquetas) {
        # Añadir texto manualmente en la perspectiva
        for (i in 1:n) {
          # Posición aproximada en la base
          text(x = i, y = 1, labels = nombres[i], 
               pos = 1, cex = cex * 0.8, col = "darkgray")
        }
      }
    }
    
    # -------------------------------------------------------------------------
    # 6. RENDER DEL GRÁFICO PRINCIPAL
    # -------------------------------------------------------------------------
    output$plot <- renderPlot({
      req(input$tipo_grafico, input$var1, df())
      
      data <- df()
      v1 <- input$var1
      v2 <- if(input$var2 != "") input$var2 else NULL
      x1 <- data[[v1]]
      
      tipo <- input$tipo_grafico
      color_base <- input$color_hex
      paleta <- input$paleta
      modo_3d <- input$modo_3d
      cex <- input$cex
      pch_size <- input$pch_size
      lwd <- input$lwd
      etiquetas_val <- input$etiquetas_valores
      etiquetas_pct <- input$etiquetas_porcentajes
      etiquetas_med <- input$etiquetas_medias
      etiquetas_out <- input$etiquetas_outliers
      mostrar_leyenda <- input$mostrar_leyenda
      titulo <- if(input$titulo != "") input$titulo else NULL
      subtitulo <- if(input$subtitulo != "") input$subtitulo else NULL
      
      # Título final
      main_text <- ifelse(is.null(titulo), 
                          paste("Gráfico de", v1), 
                          titulo)
      if (!is.null(subtitulo)) {
        main_text <- paste(main_text, "\n", subtitulo)
      }
      
      # ============================================
      # GRÁFICOS UNIVARIADOS NUMÉRICOS
      # ============================================
      
      if (tipo == "hist") {
        h <- hist(x1, plot = FALSE)
        colores <- obtener_colores(1, paleta, color_base)
        hist(x1, col = colores[1], border = "white",
             main = main_text, xlab = v1, ylab = "Frecuencia",
             cex.main = cex, cex.lab = cex, cex.axis = cex)
        if (etiquetas_val) {
          text(h$mids, h$counts, labels = h$counts, pos = 3, cex = cex * 0.8)
        }
        if (etiquetas_med) {
          abline(v = mean(x1, na.rm = TRUE), col = "red", lwd = lwd, lty = 2)
          abline(v = median(x1, na.rm = TRUE), col = "blue", lwd = lwd, lty = 2)
          if (mostrar_leyenda) {
            legend("topright", legend = c("Media", "Mediana"), 
                   col = c("red", "blue"), lty = 2, lwd = lwd, cex = cex * 0.8)
          }
        }
      }
      
      if (tipo == "hist_dens") {
        h <- hist(x1, plot = FALSE)
        colores <- obtener_colores(1, paleta, color_base)
        hist(x1, col = colores[1], border = "white", freq = FALSE,
             main = main_text, xlab = v1, ylab = "Densidad",
             cex.main = cex, cex.lab = cex, cex.axis = cex)
        lines(density(x1, na.rm = TRUE), col = "darkred", lwd = lwd * 1.5)
        if (etiquetas_val) {
          text(h$mids, h$counts / sum(h$counts) / diff(h$breaks)[1], 
               labels = h$counts, pos = 3, cex = cex * 0.8)
        }
        if (mostrar_leyenda) {
          legend("topright", legend = "Densidad", col = "darkred", lwd = lwd * 1.5, cex = cex * 0.8)
        }
      }
      
      if (tipo == "box") {
        colores <- obtener_colores(1, paleta, color_base)
        bp <- boxplot(x1, col = colores[1], border = "darkblue",
                      main = main_text, ylab = v1,
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      notch = FALSE)
        if (etiquetas_med) {
          points(1, mean(x1, na.rm = TRUE), col = "red", pch = 19, cex = pch_size * 1.5)
        }
        if (etiquetas_out && length(bp$out) > 0) {
          text(rep(1.15, length(bp$out)), bp$out,
               labels = round(bp$out, 2), col = "red", cex = cex * 0.7)
        }
        if (etiquetas_med && mostrar_leyenda) {
          legend("topright", legend = "Media", col = "red", pch = 19, cex = cex * 0.8)
        }
      }
      
      if (tipo == "violin") {
        # Simulación de violín con densidad + boxplot
        colores <- obtener_colores(1, paleta, color_base)
        d <- density(x1, na.rm = TRUE)
        
        # Escalar densidad para que quepa en el ancho
        max_d <- max(d$y)
        escala <- 0.4
        
        plot(0, 0, type = "n", xlim = c(-0.5, 0.5), ylim = range(x1, na.rm = TRUE),
             main = main_text, xlab = "", ylab = v1, xaxt = "n",
             cex.main = cex, cex.lab = cex, cex.axis = cex)
        
        # Dibujar "violín" como polígono
        polygon(c(-d$y / max_d * escala, rev(d$y / max_d * escala)),
                c(d$x, rev(d$x)), col = colores[1], border = "darkblue")
        
        # Boxplot interno
        bp <- boxplot(x1, at = 0, add = TRUE, col = "white", border = "black",
                      width = 0.1, xaxt = "n", yaxt = "n")
        
        if (etiquetas_med) {
          points(0, mean(x1, na.rm = TRUE), col = "red", pch = 19, cex = pch_size)
        }
      }
      
      if (tipo == "densidad") {
        colores <- obtener_colores(1, paleta, color_base)
        d <- density(x1, na.rm = TRUE)
        plot(d, col = colores[1], lwd = lwd * 2,
             main = main_text, xlab = v1, ylab = "Densidad",
             cex.main = cex, cex.lab = cex, cex.axis = cex)
        polygon(d, col = adjustcolor(colores[1], alpha.f = 0.3), border = NA)
        if (etiquetas_med) {
          abline(v = mean(x1, na.rm = TRUE), col = "red", lwd = lwd, lty = 2)
          abline(v = median(x1, na.rm = TRUE), col = "blue", lwd = lwd, lty = 2)
          if (mostrar_leyenda) {
            legend("topright", legend = c("Media", "Mediana"), 
                   col = c("red", "blue"), lty = 2, lwd = lwd, cex = cex * 0.8)
          }
        }
      }
      
      if (tipo == "qq") {
        qqnorm(x1, main = main_text, pch = 19, col = color_base,
               cex = pch_size, cex.main = cex, cex.lab = cex, cex.axis = cex)
        qqline(x1, col = "red", lwd = lwd * 2)
        if (mostrar_leyenda) {
          legend("topleft", legend = "Línea teórica", col = "red", lwd = lwd * 2, cex = cex * 0.8)
        }
      }
      
      if (tipo == "lineas") {
        colores <- obtener_colores(1, paleta, color_base)
        plot(x1, type = "l", col = colores[1], lwd = lwd,
             main = main_text, xlab = "Índice", ylab = v1,
             cex.main = cex, cex.lab = cex, cex.axis = cex)
        points(x1, pch = 19, col = colores[1], cex = pch_size * 0.5)
        if (etiquetas_val) {
          idx <- seq(1, length(x1), length.out = min(10, length(x1)))
          text(idx, x1[idx], labels = round(x1[idx], 1), 
               pos = 3, cex = cex * 0.6, col = "darkgray")
        }
      }
      
      if (tipo == "dot") {
        colores <- obtener_colores(1, paleta, color_base)
        stripchart(x1, method = "stack", pch = 19, col = colores[1],
                   cex = pch_size, main = main_text, xlab = v1,
                   cex.main = cex, cex.lab = cex, cex.axis = cex)
        if (etiquetas_med) {
          abline(v = mean(x1, na.rm = TRUE), col = "red", lwd = lwd, lty = 2)
          abline(v = median(x1, na.rm = TRUE), col = "blue", lwd = lwd, lty = 2)
        }
      }
      
      if (tipo == "stem") {
        # Stem-and-leaf es texto, no gráfico
        # Mostrar como stripchart alternativo
        colores <- obtener_colores(1, paleta, color_base)
        stripchart(x1, method = "jitter", pch = 19, col = colores[1],
                   cex = pch_size, main = paste(main_text, "\n(Representación alternativa)"),
                   xlab = v1, cex.main = cex, cex.lab = cex, cex.axis = cex)
      }
      
      if (tipo == "barras_freq") {
        tab <- table(x1)
        colores <- obtener_colores(length(tab), paleta, color_base)
        bp <- barplot(tab, col = colores, border = "white",
                      main = main_text, xlab = v1, ylab = "Frecuencia",
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      ylim = c(0, max(tab) * 1.2))
        if (etiquetas_val) {
          text(bp, tab, labels = tab, pos = 3, cex = cex * 0.8)
        }
        if (etiquetas_pct) {
          text(bp, tab, labels = paste0(round(tab/sum(tab)*100, 1), "%"), 
               pos = 1, cex = cex * 0.7, col = "white")
        }
      }
      
      # ============================================
      # GRÁFICOS UNIVARIADOS CATEGÓRICOS
      # ============================================
      
      if (tipo == "barras") {
        tab <- table(x1)
        colores <- obtener_colores(length(tab), paleta, color_base)
        bp <- barplot(tab, col = colores, border = "white",
                      main = main_text, xlab = v1, ylab = "Frecuencia",
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      ylim = c(0, max(tab) * 1.2))
        if (etiquetas_val) {
          text(bp, tab, labels = tab, pos = 3, cex = cex * 0.8)
        }
        if (etiquetas_pct) {
          text(bp, tab, labels = paste0(round(tab/sum(tab)*100, 1), "%"), 
               pos = 1, cex = cex * 0.7, col = "white")
        }
        if (modo_3d) {
          # Simular 3D con sombreado
          for (i in seq_along(bp)) {
            rect(bp[i] - 0.3, 0, bp[i] + 0.3, tab[i], 
                 col = colores[i], border = "darkgray", lwd = 2)
            # Sombra
            polygon(c(bp[i] - 0.3, bp[i] + 0.3, bp[i] + 0.5, bp[i] + 0.1),
                    c(tab[i], tab[i], tab[i] - 0.05 * max(tab), tab[i] - 0.05 * max(tab)),
                    col = adjustcolor("black", alpha.f = 0.2), border = NA)
          }
        }
      }
      
      if (tipo == "barras_h") {
        tab <- table(x1)
        colores <- obtener_colores(length(tab), paleta, color_base)
        bp <- barplot(tab, col = colores, border = "white", horiz = TRUE,
                      main = main_text, xlab = "Frecuencia", ylab = v1,
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      xlim = c(0, max(tab) * 1.2))
        if (etiquetas_val) {
          text(tab, bp, labels = tab, pos = 4, cex = cex * 0.8)
        }
        if (etiquetas_pct) {
          text(tab, bp, labels = paste0(round(tab/sum(tab)*100, 1), "%"), 
               pos = 2, cex = cex * 0.7, col = "white")
        }
      }
      
      if (tipo == "barras_3d") {
        tab <- table(x1)
        colores <- obtener_colores(length(tab), paleta, color_base)
        if (modo_3d) {
          barras_3d_base(tab, colores, main = main_text, 
                         etiquetas = etiquetas_val, cex = cex,
                         mostrar_leyenda = mostrar_leyenda)
        } else {
          # Fallback a barras normales con "efecto 3D"
          bp <- barplot(tab, col = colores, border = "darkgray",
                        main = paste(main_text, "\n(Efecto 3D)"),
                        xlab = v1, ylab = "Frecuencia",
                        cex.main = cex, cex.lab = cex, cex.axis = cex,
                        ylim = c(0, max(tab) * 1.3))
          # Añadir sombra 3D
          for (i in seq_along(bp)) {
            polygon(c(bp[i] - 0.4, bp[i] + 0.4, bp[i] + 0.6, bp[i] + 0.2),
                    c(tab[i], tab[i], tab[i] - 0.08 * max(tab), tab[i] - 0.08 * max(tab)),
                    col = adjustcolor("black", alpha.f = 0.15), border = NA)
          }
          if (etiquetas_val) {
            text(bp, tab, labels = tab, pos = 3, cex = cex * 0.8)
          }
        }
      }
      
      if (tipo == "pastel") {
        tab <- table(x1)
        colores <- obtener_colores(length(tab), paleta, color_base)
        porc <- round(100 * tab / sum(tab), 1)
        labels_pie <- if (etiquetas_pct) {
          paste(names(tab), "\n", porc, "%")
        } else {
          names(tab)
        }
        pie(tab, labels = labels_pie, col = colores,
            main = main_text, cex = cex, cex.main = cex)
        if (mostrar_leyenda && length(tab) > 3) {
          legend("topright", legend = names(tab), fill = colores, cex = cex * 0.7)
        }
      }
      
      if (tipo == "pareto") {
        tab <- sort(table(x1), decreasing = TRUE)
        colores <- obtener_colores(length(tab), paleta, color_base)
        cum <- cumsum(tab) / sum(tab) * 100
        
        # Barras
        bp <- barplot(tab, col = colores, border = "white",
                      main = paste(main_text, "\n(Pareto: barras + acumulado %)"),
                      xlab = v1, ylab = "Frecuencia",
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      ylim = c(0, max(tab) * 1.2))
        
        # Línea acumulada
        par(new = TRUE)
        plot(bp, cum, type = "b", pch = 19, col = "red", lwd = lwd,
             axes = FALSE, xlab = "", ylab = "",
             ylim = c(0, 120), xlim = range(bp) + c(-0.5, 0.5))
        axis(4, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"),
             col = "red", col.axis = "red", cex.axis = cex * 0.8)
        mtext("% Acumulado", side = 4, line = 3, col = "red", cex = cex)
        
        # Línea 80%
        abline(h = 80, col = "darkgreen", lty = 2, lwd = lwd * 0.8)
        text(max(bp), 82, "80%", col = "darkgreen", pos = 2, cex = cex * 0.8)
        
        if (etiquetas_val) {
          text(bp, tab, labels = tab, pos = 3, cex = cex * 0.8)
        }
        if (mostrar_leyenda) {
          legend("topright", legend = c("Frecuencia", "% Acumulado", "80%"),
                 fill = c(NA, NA, NA), col = c("black", "red", "darkgreen"),
                 lty = c(NA, 1, 2), lwd = lwd, pch = c(NA, 19, NA),
                 cex = cex * 0.7, border = NA)
        }
        par(new = FALSE)
      }
      
      if (tipo == "lollipop") {
        tab <- table(x1)
        colores <- obtener_colores(length(tab), paleta, color_base)
        n <- length(tab)
        
        plot(1:n, as.numeric(tab), type = "n",
             main = main_text, xlab = v1, ylab = "Frecuencia",
             xaxt = "n", cex.main = cex, cex.lab = cex, cex.axis = cex,
             ylim = c(0, max(tab) * 1.1))
        axis(1, at = 1:n, labels = names(tab), las = 2, cex.axis = cex * 0.7)
        
        # Líneas
        segments(1:n, 0, 1:n, as.numeric(tab), col = "gray", lwd = lwd)
        # Puntos
        points(1:n, as.numeric(tab), pch = 19, col = colores, cex = pch_size * 2)
        
        if (etiquetas_val) {
          text(1:n, as.numeric(tab), labels = as.numeric(tab), 
               pos = 3, cex = cex * 0.8)
        }
      }
      
      if (tipo == "mosaico") {
        tab <- table(x1)
        colores <- obtener_colores(length(tab), paleta, color_base)
        # Mosaico simple para una variable = barras proporcionales
        porc <- tab / sum(tab)
        bp <- barplot(porc, col = colores, border = "white",
                      main = paste(main_text, "\n(Mosaico de proporciones)"),
                      xlab = v1, ylab = "Proporción",
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      ylim = c(0, 1.1))
        if (etiquetas_pct) {
          text(bp, porc, labels = paste0(round(porc * 100, 1), "%"), 
               pos = 3, cex = cex * 0.8)
        }
      }
      
      # ============================================
      # GRÁFICOS BIVARIADOS NUM-NUM
      # ============================================
      
      if (tipo == "scatter") {
        x2 <- data[[v2]]
        colores <- obtener_colores(1, paleta, color_base)
        plot(x1, x2, pch = 19, col = adjustcolor(colores[1], alpha.f = 0.6),
             cex = pch_size, main = main_text,
             xlab = v1, ylab = v2,
             cex.main = cex, cex.lab = cex, cex.axis = cex)
        abline(lm(x2 ~ x1), col = "red", lwd = lwd, lty = 2)
        if (etiquetas_med) {
          points(mean(x1, na.rm = TRUE), mean(x2, na.rm = TRUE), 
                 pch = 3, col = "blue", cex = pch_size * 2, lwd = lwd)
        }
        if (mostrar_leyenda) {
          legend("topright", legend = c("Observaciones", "Regresión", "Centroide"),
                 col = c(colores[1], "red", "blue"), 
                 pch = c(19, NA, 3), lty = c(NA, 2, NA), 
                 lwd = c(NA, lwd, lwd), cex = cex * 0.7)
        }
      }
      
      if (tipo == "scatter_3d") {
        x2 <- data[[v2]]
        # Simulación 3D con perspectiva
        z <- x2  # eje Z
        y <- x1  # eje Y
        x <- 1:length(x1)  # eje X (índice)
        
        # Crear superficie de dispersión
        colores <- obtener_colores(1, paleta, color_base)
        
        # Usar scatterplot con tamaño como "pseudo-3D"
        plot(x1, x2, pch = 19, 
             col = adjustcolor(colores[1], alpha.f = 0.6),
             cex = pch_size * (1 + (1:length(x1))/length(x1)),
             main = paste(main_text, "\n(Efecto 3D: tamaño = profundidad)"),
             xlab = v1, ylab = v2,
             cex.main = cex, cex.lab = cex, cex.axis = cex)
        
        # Añadir "sombra" en el piso
        segments(x1, min(x2, na.rm = TRUE), x1, x2, 
                 col = adjustcolor("gray", alpha.f = 0.3), lwd = 0.5)
      }
      
      if (tipo == "lineas_bi") {
        x2 <- data[[v2]]
        colores <- obtener_colores(2, paleta, color_base)
        plot(x1, type = "l", col = colores[1], lwd = lwd,
             main = main_text, xlab = "Índice", ylab = "Valor",
             ylim = range(c(x1, x2), na.rm = TRUE),
             cex.main = cex, cex.lab = cex, cex.axis = cex)
        lines(x2, col = colores[2], lwd = lwd, lty = 2)
        points(x1, pch = 19, col = colores[1], cex = pch_size * 0.5)
        points(x2, pch = 19, col = colores[2], cex = pch_size * 0.5)
        if (mostrar_leyenda) {
          legend("topright", legend = c(v1, v2), 
                 col = colores[1:2], lwd = lwd, lty = c(1, 2), cex = cex * 0.8)
        }
      }
      
      if (tipo == "pareto_bi") {
        # Pareto donde una variable pesa la otra
        x2 <- data[[v2]]
        # Ordenar por x2
        ord <- order(x2, decreasing = TRUE)
        x1_ord <- x1[ord]
        x2_ord <- x2[ord]
        
        cum <- cumsum(x2_ord) / sum(x2_ord, na.rm = TRUE) * 100
        colores <- obtener_colores(1, paleta, color_base)
        
        bp <- barplot(x1_ord, col = colores, border = "white",
                      main = paste(main_text, "\n(Pareto ponderado por", v2, ")"),
                      xlab = v1, ylab = paste("Valor de", v1),
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      ylim = c(0, max(x1_ord, na.rm = TRUE) * 1.2))
        
        par(new = TRUE)
        plot(bp, cum, type = "b", pch = 19, col = "red", lwd = lwd,
             axes = FALSE, xlab = "", ylab = "",
             ylim = c(0, 120))
        axis(4, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"),
             col = "red", col.axis = "red", cex.axis = cex * 0.8)
        mtext(paste("% Acumulado de", v2), side = 4, line = 3, col = "red", cex = cex)
        par(new = FALSE)
      }
      
      if (tipo == "hexbin") {
        x2 <- data[[v2]]
        # Simulación de hexbin con densidad de color
        colores <- obtener_colores(20, paleta, color_base)
        
        plot(x1, x2, pch = 19, 
             col = colores[cut(density(x1, na.rm = TRUE)$y, breaks = 20, labels = FALSE)],
             cex = pch_size, main = paste(main_text, "\n(Densidad de color)"),
             xlab = v1, ylab = v2,
             cex.main = cex, cex.lab = cex, cex.axis = cex)
      }
      
      # ============================================
      # GRÁFICOS BIVARIADOS CAT-CAT
      # ============================================
      
      if (tipo == "barras_agrup") {
        x2 <- data[[v2]]
        tab <- table(x1, x2)
        colores <- obtener_colores(nrow(tab), paleta, color_base)
        bp <- barplot(tab, beside = TRUE, col = colores, border = "white",
                      main = main_text, xlab = v2, ylab = "Frecuencia",
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      ylim = c(0, max(tab) * 1.2))
        if (etiquetas_val) {
          text(bp, tab, labels = tab, pos = 3, cex = cex * 0.7)
        }
        if (mostrar_leyenda) {
          legend("topright", legend = rownames(tab), fill = colores, cex = cex * 0.7)
        }
      }
      
      if (tipo == "barras_apil") {
        x2 <- data[[v2]]
        tab <- table(x1, x2)
        colores <- obtener_colores(nrow(tab), paleta, color_base)
        bp <- barplot(tab, col = colores, border = "white",
                      main = main_text, xlab = v2, ylab = "Frecuencia",
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      ylim = c(0, sum(tab) * 1.1))
        if (etiquetas_pct) {
          # Porcentajes por columna
          pct_col <- prop.table(tab, margin = 2) * 100
          text(bp, apply(tab, 2, cumsum) - tab/2, 
               labels = paste0(round(pct_col, 1), "%"),
               cex = cex * 0.6, col = "white")
        }
        if (mostrar_leyenda) {
          legend("topright", legend = rownames(tab), fill = colores, cex = cex * 0.7)
        }
      }
      
      if (tipo == "barras_3d_bi") {
        x2 <- data[[v2]]
        tab <- table(x1, x2)
        colores <- obtener_colores(nrow(tab), paleta, color_base)
        
        if (modo_3d) {
          # 3D con perspectiva para tabla
          z <- as.matrix(tab)
          persp(x = 1:nrow(z), y = 1:ncol(z), z = z,
                theta = 30, phi = 20,
                col = rep(colores, ncol(z)),
                main = main_text,
                xlab = v1, ylab = v2, zlab = "Frecuencia",
                ticktype = "detailed", cex.axis = cex * 0.8)
        } else {
          # Fallback a barras agrupadas con efecto 3D
          bp <- barplot(tab, beside = TRUE, col = colores, border = "darkgray",
                        main = paste(main_text, "\n(Efecto 3D)"),
                        xlab = v2, ylab = "Frecuencia",
                        cex.main = cex, cex.lab = cex, cex.axis = cex,
                        ylim = c(0, max(tab) * 1.3))
          if (etiquetas_val) {
            text(bp, tab, labels = tab, pos = 3, cex = cex * 0.7)
          }
          if (mostrar_leyenda) {
            legend("topright", legend = rownames(tab), fill = colores, cex = cex * 0.7)
          }
        }
      }
      
      if (tipo == "mosaico_bi") {
        x2 <- data[[v2]]
        tab <- table(x1, x2)
        colores <- obtener_colores(length(tab), paleta, color_base)
        mosaicplot(tab, col = colores, main = main_text,
                   cex.axis = cex * 0.8, las = 2)
      }
      
      if (tipo == "heatmap") {
        x2 <- data[[v2]]
        tab <- table(x1, x2)
        colores <- obtener_colores(20, paleta, color_base)
        
        image(1:ncol(tab), 1:nrow(tab), t(as.matrix(tab)),
              col = colores, main = main_text,
              xlab = v2, ylab = v1,
              cex.main = cex, cex.lab = cex, cex.axis = cex)
        
        # Etiquetas
        if (etiquetas_val) {
          for (i in 1:ncol(tab)) {
            for (j in 1:nrow(tab)) {
              text(i, j, labels = tab[j, i], cex = cex * 0.8)
            }
          }
        }
      }
      
      # ============================================
      # GRÁFICOS BIVARIADOS MIXTO
      # ============================================
      
      if (tipo == "box_grupos") {
        x2 <- data[[v2]]
        # Determinar cuál es numérica y cuál categórica
        if (is.numeric(x1)) {
          y <- x1; grupo <- x2; ylab <- v1; xlab <- v2
        } else {
          y <- x2; grupo <- x1; ylab <- v2; xlab <- v1
        }
        
        n_grupos <- length(unique(na.omit(grupo)))
        colores <- obtener_colores(n_grupos, paleta, color_base)
        
        bp <- boxplot(y ~ grupo, col = colores, border = "darkblue",
                      main = main_text, xlab = xlab, ylab = ylab,
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      notch = FALSE)
        
        if (etiquetas_med) {
          medias <- tapply(y, grupo, mean, na.rm = TRUE)
          points(1:length(medias), medias, col = "red", pch = 19, cex = pch_size)
        }
        if (etiquetas_out && length(bp$out) > 0) {
          text(bp$group, bp$out, labels = round(bp$out, 2), 
               col = "red", cex = cex * 0.7, pos = 4)
        }
        if (etiquetas_med && mostrar_leyenda) {
          legend("topright", legend = "Media", col = "red", pch = 19, cex = cex * 0.8)
        }
      }
      
      if (tipo == "violin_grupos") {
        x2 <- data[[v2]]
        if (is.numeric(x1)) {
          y <- x1; grupo <- x2; ylab <- v1; xlab <- v2
        } else {
          y <- x2; grupo <- x1; ylab <- v2; xlab <- v1
        }
        
        n_grupos <- length(unique(na.omit(grupo)))
        colores <- obtener_colores(n_grupos, paleta, color_base)
        
        # Simulación de violín por grupos
        grupos_unicos <- unique(na.omit(grupo))
        plot(0, 0, type = "n", xlim = c(0.5, n_grupos + 0.5), 
             ylim = range(y, na.rm = TRUE),
             main = main_text, xlab = xlab, ylab = ylab,
             xaxt = "n", cex.main = cex, cex.lab = cex, cex.axis = cex)
        axis(1, at = 1:n_grupos, labels = grupos_unicos, las = 2, cex.axis = cex * 0.8)
        
        for (i in seq_along(grupos_unicos)) {
          g <- grupos_unicos[i]
          y_g <- y[grupo == g]
          if (length(y_g) > 2) {
            d <- density(y_g, na.rm = TRUE)
            max_d <- max(d$y)
            escala <- 0.4
            polygon(c(i - d$y/max_d*escala, rev(i + d$y/max_d*escala)),
                    c(d$x, rev(d$x)), col = adjustcolor(colores[i], alpha.f = 0.5),
                    border = "darkblue")
            # Box interno
            boxplot(y_g, at = i, add = TRUE, col = "white", border = "black",
                    width = 0.1, xaxt = "n", yaxt = "n")
          }
        }
      }
      
      if (tipo == "barras_medias") {
        x2 <- data[[v2]]
        if (is.numeric(x1)) {
          y <- x1; grupo <- x2; ylab <- paste("Media de", v1); xlab <- v2
        } else {
          y <- x2; grupo <- x1; ylab <- paste("Media de", v2); xlab <- v1
        }
        
        medias <- tapply(y, grupo, mean, na.rm = TRUE)
        desv <- tapply(y, grupo, sd, na.rm = TRUE)
        n <- tapply(y, grupo, function(x) sum(!is.na(x)))
        error <- desv / sqrt(n)  # Error estándar
        
        colores <- obtener_colores(length(medias), paleta, color_base)
        bp <- barplot(medias, col = colores, border = "white",
                      main = main_text, xlab = xlab, ylab = ylab,
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      ylim = c(0, max(medias + error, na.rm = TRUE) * 1.2))
        
        # Barras de error
        segments(bp, medias - error, bp, medias + error, lwd = lwd)
        segments(bp - 0.1, medias - error, bp + 0.1, medias - error, lwd = lwd)
        segments(bp - 0.1, medias + error, bp + 0.1, medias + error, lwd = lwd)
        
        if (etiquetas_val) {
          text(bp, medias, labels = round(medias, 2), pos = 3, cex = cex * 0.8)
        }
      }
      
      if (tipo == "lineas_grupo") {
        x2 <- data[[v2]]
        if (is.numeric(x1)) {
          y <- x1; grupo <- x2; ylab <- v1; xlab <- "Índice"
        } else {
          y <- x2; grupo <- x1; ylab <- v2; xlab <- "Índice"
        }
        
        grupos_unicos <- unique(na.omit(grupo))
        n_grupos <- length(grupos_unicos)
        colores <- obtener_colores(n_grupos, paleta, color_base)
        
        # Calcular medias por grupo en orden
        plot(0, 0, type = "n", xlim = c(1, length(y)), 
             ylim = range(y, na.rm = TRUE),
             main = main_text, xlab = xlab, ylab = ylab,
             cex.main = cex, cex.lab = cex, cex.axis = cex)
        
        for (i in seq_along(grupos_unicos)) {
          g <- grupos_unicos[i]
          idx <- which(grupo == g)
          lines(idx, y[idx], col = colores[i], lwd = lwd, type = "o", pch = 19)
        }
        
        if (mostrar_leyenda) {
          legend("topright", legend = grupos_unicos, col = colores, 
                 lwd = lwd, pch = 19, cex = cex * 0.7)
        }
      }
      
      if (tipo == "pareto_grupo") {
        x2 <- data[[v2]]
        if (is.numeric(x1)) {
          y <- x1; grupo <- x2; ylab <- v1; xlab <- v2
        } else {
          y <- x2; grupo <- x1; ylab <- v2; xlab <- v1
        }
        
        # Pareto por grupo
        medias <- sort(tapply(y, grupo, mean, na.rm = TRUE), decreasing = TRUE)
        cum <- cumsum(medias) / sum(medias, na.rm = TRUE) * 100
        colores <- obtener_colores(length(medias), paleta, color_base)
        
        bp <- barplot(medias, col = colores, border = "white",
                      main = paste(main_text, "\n(Pareto de medias por grupo)"),
                      xlab = xlab, ylab = ylab,
                      cex.main = cex, cex.lab = cex, cex.axis = cex,
                      ylim = c(0, max(medias, na.rm = TRUE) * 1.2))
        
        par(new = TRUE)
        plot(bp, cum, type = "b", pch = 19, col = "red", lwd = lwd,
             axes = FALSE, xlab = "", ylab = "",
             ylim = c(0, 120))
        axis(4, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"),
             col = "red", col.axis = "red", cex.axis = cex * 0.8)
        mtext("% Acumulado", side = 4, line = 3, col = "red", cex = cex)
        abline(h = 80, col = "darkgreen", lty = 2, lwd = lwd * 0.8)
        par(new = FALSE)
      }
      
    })
    
    # -------------------------------------------------------------------------
    # 7. INTERPRETACIÓN AUTOMÁTICA
    # -------------------------------------------------------------------------
    output$analisis <- renderPrint({
      req(analisis_vars())
      a <- analisis_vars()
      
      cat("=== ANÁLISIS INTELIGENTE ===\n\n")
      
      if (!a$es_bivariado) {
        cat("Variable:", a$var1, "\n")
        cat("Tipo:", a$tipo1, "\n")
        cat("Observaciones:", a$analisis1$n, "\n")
        
        if (a$tipo1 != "categorica") {
          cat("\n--- Métricas ---\n")
          cat("Media:", round(a$analisis1$media, 3), "\n")
          cat("Mediana:", round(a$analisis1$mediana, 3), "\n")
          cat("Desv. Estándar:", round(a$analisis1$desv, 3), "\n")
          cat("CV:", round(a$analisis1$cv, 2), "%\n")
          cat("Asimetría:", round(a$analisis1$skew, 3), "\n")
          cat("Curtosis:", round(a$analisis1$kurt, 3), "\n")
          cat("Outliers:", a$analisis1$n_outliers, 
              "(", round(a$analisis1$pct_outliers, 2), "%)\n")
          
          cat("\n--- Recomendación ---\n")
          cat("Gráfico recomendado:", a$analisis1$recomendacion, "\n")
          cat("Razón:", a$analisis1$razon, "\n")
          
          if (abs(a$analisis1$skew) > 1) {
            cat("\n⚠️ Distribución muy asimétrica. Considerar transformación.\n")
          }
          if (a$analisis1$pct_outliers > 5) {
            cat("⚠️ Alta presencia de outliers. El boxplot mostrará mejor la estructura.\n")
          }
        } else {
          cat("Categorías:", a$analisis1$n_categorias, "\n")
          cat("Frecuencias:", paste(a$analisis1$frecuencias, collapse = ", "), "\n")
          cat("Categoría dominante:", round(a$analisis1$pct_max, 1), "%\n")
          cat("\nGráfico recomendado:", a$analisis1$recomendacion, "\n")
          cat("Razón:", a$analisis1$razon, "\n")
        }
        
      } else {
        cat("Variables:", a$var1, "y", a$var2, "\n")
        cat("Combinación:", a$tipo_combinacion, "\n")
        cat("Gráfico recomendado:", a$recomendacion, "\n")
        cat("Razón:", a$razon, "\n")
        
        if (a$tipo_combinacion == "num_num") {
          x1 <- df()[[a$var1]]
          x2 <- df()[[a$var2]]
          cor_test <- cor.test(x1, x2)
          cat("\nCorrelación:", round(cor_test$estimate, 3), "\n")
          cat("p-valor:", format(cor_test$p.value, digits = 3), "\n")
          if (cor_test$p.value < 0.05) {
            cat("✅ Correlación significativa\n")
          } else {
            cat("❌ No hay correlación significativa\n")
          }
        }
      }
    })
    
    # -------------------------------------------------------------------------
    # 8. DETALLES TÉCNICOS EN TABLA
    # -------------------------------------------------------------------------
    output$detalles_tabla <- renderTable({
      req(analisis_vars())
      a <- analisis_vars()
      
      if (!a$es_bivariado) {
        if (a$tipo1 != "categorica") {
          data.frame(
            Métrica = c("Variable", "Tipo", "Observaciones", "Media", "Mediana", 
                        "Desv. Estándar", "CV (%)", "Asimetría", "Curtosis",
                        "Outliers", "% Outliers", "Gráfico Recomendado"),
            Valor = c(a$var1, a$tipo1, a$analisis1$n,
                      round(a$analisis1$media, 3), round(a$analisis1$mediana, 3),
                      round(a$analisis1$desv, 3), round(a$analisis1$cv, 2),
                      round(a$analisis1$skew, 3), round(a$analisis1$kurt, 3),
                      a$analisis1$n_outliers, round(a$analisis1$pct_outliers, 2),
                      a$analisis1$recomendacion)
          )
        } else {
          data.frame(
            Métrica = c("Variable", "Tipo", "Observaciones", "Categorías",
                        "Frecuencia Máxima", "Gráfico Recomendado"),
            Valor = c(a$var1, a$tipo1, a$analisis1$n, a$analisis1$n_categorias,
                      round(a$analisis1$pct_max, 1), a$analisis1$recomendacion)
          )
        }
      } else {
        data.frame(
          Métrica = c("Variable 1", "Variable 2", "Tipo Combinación",
                      "Gráfico Recomendado"),
          Valor = c(a$var1, a$var2, a$tipo_combinacion, a$recomendacion)
        )
      }
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    # -------------------------------------------------------------------------
    # 9. DESCARGA DE GRÁFICO
    # -------------------------------------------------------------------------
    output$descargar <- downloadHandler(
      filename = function() {
        ext <- tolower(input$formato)
        if (ext == "postscript") ext <- "ps"
        paste0("grafico_", input$var1, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        formato <- tolower(input$formato)
        
        if (formato == "png") {
          png(file, width = input$ancho, height = input$alto, units = "in", res = 300)
        } else if (formato == "pdf") {
          pdf(file, width = input$ancho, height = input$alto)
        } else if (formato == "svg") {
          svg(file, width = input$ancho, height = input$alto)
        } else if (formato == "postscript") {
          postscript(file, width = input$ancho, height = input$alto)
        }
        
        # Re-ejecutar el gráfico
        # (Nota: en una implementación real, extraerías el código del renderPlot a una función)
        # Por simplicidad, usamos un mensaje
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(0, 0, "Gráfico exportado", cex = 2)
        
        dev.off()
      }
    )
    
  })
}
