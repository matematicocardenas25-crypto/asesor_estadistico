# ============================================================================
# MÓDULO: DIAGNÓSTICO INTELIGENTE DE NORMALIDAD Y TRANSFORMACIONES
# VERSIÓN: Análisis interno secuencial por interpretabilidad
# ============================================================================
# No requiere paquetes externos. Todo en R base.

# ============================================================================
# UI
# ============================================================================
mod_normalidad_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Título principal
    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               color: white; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
      h2("📈 Diagnóstico Inteligente de Normalidad", style = "margin: 0;"),
      p("Analiza transformaciones en orden de interpretabilidad y recomienda la primera que normalice.",
        style = "margin: 5px 0 0 0; opacity: 0.9;")
    ),
    
    # Panel de selección
    wellPanel(
      style = "background: #f8f9fa; border-left: 4px solid #667eea;",
      
      h4("🔧 Configuración del Análisis"),
      
      fluidRow(
        column(6,
               selectInput(
                 ns("variable"),
                 "Variable numérica a analizar:",
                 choices = NULL,
                 width = "100%"
               )
        ),
        column(6,
               br(),
               actionButton(
                 ns("analizar"),
                 "🔍 Analizar Distribución",
                 class = "btn-primary",
                 style = "margin-top: 5px; width: 100%; background: #667eea; border: none;"
               )
        )
      )
    ),
    
    # Panel de diagnóstico original
    conditionalPanel(
      condition = sprintf("input['%s'] > 0", ns("analizar")),
      
      # Diagnóstico original
      wellPanel(
        style = "border-left: 4px solid #17a2b8;",
        h4("🔎 Diagnóstico de la Distribución Original"),
        tableOutput(ns("diagnostico_original")),
        br(),
        uiOutput(ns("alerta_distribucion"))
      ),
      
      # ANÁLISIS SECUENCIAL
      wellPanel(
        style = "border-left: 4px solid #6f42c1;",
        h4("🧠 Análisis Secuencial de Transformaciones"),
        p("La app prueba transformaciones del más simple al más complejo. 
          Se detiene en la primera que logra |asimetría| < 0.5."),
        br(),
        uiOutput(ns("analisis_secuencial")),
        br(),
        tableOutput(ns("tabla_secuencial"))
      ),
      
      # Tabla comparativa completa
      wellPanel(
        style = "border-left: 4px solid #28a745;",
        h4("🏆 Comparación Completa de Transformaciones"),
        p("Todas las transformaciones evaluadas, ordenadas por interpretabilidad."),
        div(style = "overflow-x: auto;",
            tableOutput(ns("tabla_comparacion"))
        ),
        br(),
        uiOutput(ns("recomendacion_principal"))
      ),
      
      # Selección y aplicación
      wellPanel(
        style = "border-left: 4px solid #ffc107;",
        h4("⚙️ Aplicar Transformación"),
        
        fluidRow(
          column(8,
                 uiOutput(ns("selector_transformacion"))
          ),
          column(4,
                 br(),
                 actionButton(
                   ns("aplicar"),
                   "✅ Aplicar Transformación",
                   class = "btn-success",
                   style = "margin-top: 5px; width: 100%;"
                 )
          )
        ),
        
        br(),
        verbatimTextOutput(ns("detalle_transformacion"))
      ),
      
      # Comparación antes/después
      conditionalPanel(
        condition = sprintf("input['%s'] > 0", ns("aplicar")),
        
        wellPanel(
          style = "border-left: 4px solid #dc3545;",
          h4("📊 Comparación Antes / Después"),
          tableOutput(ns("comparacion_antes_despues")),
          br(),
          plotOutput(ns("histograma_comparacion"), height = "400px")
        )
      )
    )
  )
}

# ============================================================================
# SERVER
# ============================================================================
mod_normalidad_server <- function(id, df_global) {
  
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------------------------------------------
    # 1. ACTUALIZAR SELECTOR DE VARIABLES
    # -------------------------------------------------------------------------
    observe({
      req(df_global())
      
      vars_num <- names(Filter(is.numeric, df_global()))
      
      updateSelectInput(
        session,
        "variable",
        choices = vars_num,
        selected = if (length(vars_num) > 0) vars_num[1] else NULL
      )
    })
    
    # -------------------------------------------------------------------------
    # 2. FUNCIONES NATIVAS (100% R base)
    # -------------------------------------------------------------------------
    
    # Asimetría de Pearson (momento 3 estándarizado)
    skewness_nativo <- function(x) {
      x <- na.omit(x)
      n <- length(x)
      if (n < 3) return(NA)
      
      media <- mean(x)
      desv <- sd(x)
      if (desv == 0) return(0)
      
      m3 <- mean((x - media)^3)
      m3 / (desv^3)
    }
    
    # Curtosis exceso (momento 4 estándarizado - 3)
    kurtosis_nativo <- function(x) {
      x <- na.omit(x)
      n <- length(x)
      if (n < 4) return(NA)
      
      media <- mean(x)
      desv <- sd(x)
      if (desv == 0) return(-3)
      
      m4 <- mean((x - media)^4)
      (m4 / (desv^4)) - 3
    }
    
    # Test de normalidad - CORREGIDO: forzar valores escalares
    test_normalidad_nativo <- function(x) {
      x <- na.omit(x)
      n <- length(x)
      
      if (n < 3) {
        return(list(nombre = "N/A", estadistico = NA_real_, p_valor = NA_real_, es_normal = NA))
      }
      
      if (n <= 5000) {
        sw <- shapiro.test(x)
        return(list(
          nombre = "Shapiro-Wilk",
          estadistico = as.numeric(sw$statistic)[1],
          p_valor = as.numeric(sw$p.value)[1],
          es_normal = as.logical(sw$p.value > 0.05)[1]
        ))
      } else {
        # Para n > 5000, usar ks.test con manejo de empates
        # Crear una muestra sin empates para evitar el warning
        x_unicos <- unique(x)
        if (length(x_unicos) < n * 0.9) {
          # Hay muchos empates, usar shapiro.test con muestra
          set.seed(123)
          x_muestra <- sample(x, min(5000, n))
          sw <- shapiro.test(x_muestra)
          return(list(
            nombre = "Shapiro-Wilk (muestra)",
            estadistico = as.numeric(sw$statistic)[1],
            p_valor = as.numeric(sw$p.value)[1],
            es_normal = as.logical(sw$p.value > 0.05)[1]
          ))
        } else {
          ks <- ks.test(x, "pnorm", mean(x), sd(x))
          return(list(
            nombre = "Kolmogorov-Smirnov",
            estadistico = as.numeric(ks$statistic)[1],
            p_valor = as.numeric(ks$p.value)[1],
            es_normal = as.logical(ks$p.value > 0.05)[1]
          ))
        }
      }
    }
    
    # Calcular métricas completas de una transformación
    calcular_metricas_nativo <- function(x, nombre) {
      x <- na.omit(x)
      n <- length(x)
      
      if (n < 3) return(NULL)
      
      media <- mean(x)
      mediana <- median(x)
      desv <- sd(x)
      
      skew <- skewness_nativo(x)
      curt <- kurtosis_nativo(x)
      
      test <- test_normalidad_nativo(x)
      
      # Outliers IQR
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr_val <- q3 - q1
      li <- q1 - 1.5 * iqr_val
      ls <- q3 + 1.5 * iqr_val
      outliers <- sum(x < li | x > ls, na.rm = TRUE)
      pct_outliers <- round(outliers / n * 100, 2)
      
      list(
        nombre = nombre,
        n = n,
        media = round(media, 3),
        mediana = round(mediana, 3),
        desv = round(desv, 3),
        skewness = round(skew, 4),
        curtosis = round(curt, 4),
        test_nombre = test$nombre,
        test_estadistico = round(test$estadistico, 4),
        test_pvalor = ifelse(is.na(test$p_valor), "N/A",
                             ifelse(test$p_valor < 0.001, "< 0.001", 
                                    round(test$p_valor, 4))),
        es_normal = test$es_normal,
        outliers = outliers,
        pct_outliers = pct_outliers,
        li = round(li, 2),
        ls = round(ls, 2)
      )
    }
    
    # -------------------------------------------------------------------------
    # 3. ANÁLISIS COMPLETO CON SECUENCIA INTELIGENTE
    # -------------------------------------------------------------------------
    analisis_completo <- eventReactive(input$analizar, {
      
      req(df_global())
      req(input$variable)
      
      x <- na.omit(df_global()[[input$variable]])
      
      if (length(x) < 3) {
        showNotification("La variable debe tener al menos 3 observaciones.", type = "error")
        return(NULL)
      }
      
      # Características de la variable
      tiene_ceros <- any(x == 0, na.rm = TRUE)
      tiene_negativos <- any(x < 0, na.rm = TRUE)
      min_x <- min(x, na.rm = TRUE)
      
      # Diagnóstico original
      diag_original <- calcular_metricas_nativo(x, "Original")
      
      # ================================================================
      # ANÁLISIS SECUENCIAL: Probar en orden de interpretabilidad
      # Se detiene en la primera que logra |skew| < 0.5
      # ================================================================
      
      # ORDEN DE PRIORIDAD (del más interpretable al menos):
      # 1. Sin transformar (si ya es normal)
      # 2. Log(x)       - muy interpretable, solo > 0
      # 3. Log(x+1)     - interpretable, maneja ceros
      # 4. Raíz cuadrada - moderadamente interpretable, solo >= 0
      # 5. Box-Cox      - menos interpretable, solo > 0
      # 6. Yeo-Johnson  - poco interpretable, pero universal
      # 7. Reflejar+Log - para sesgo negativo
      
      secuencia <- list()
      secuencia_nombres <- character()
      
      # 1. ORIGINAL (ya calculado)
      secuencia[["Original"]] <- x
      secuencia_nombres <- c(secuencia_nombres, "Original")
      
      # 2. LOG(X): Solo si todos > 0
      if (!tiene_negativos && !tiene_ceros) {
        secuencia[["Log(x)"]] <- log(x)
        secuencia_nombres <- c(secuencia_nombres, "Log(x)")
      }
      
      # 3. LOG(X+1): Siempre aplicable
      if (tiene_negativos) {
        # Si hay negativos, shift para hacer positivo
        secuencia[["Log(x+1)"]] <- log(x - min_x + 1)
      } else {
        secuencia[["Log(x+1)"]] <- log(x + 1)
      }
      if (!"Log(x+1)" %in% secuencia_nombres) {
        secuencia_nombres <- c(secuencia_nombres, "Log(x+1)")
      }
      
      # 4. RAÍZ CUADRADA: Solo si >= 0
      if (!tiene_negativos) {
        secuencia[["Raíz cuadrada"]] <- sqrt(x)
        secuencia_nombres <- c(secuencia_nombres, "Raíz cuadrada")
      }
      
      # 5. BOX-COX: Solo > 0
      if (!tiene_negativos && !tiene_ceros) {
        tryCatch({
          # Estimación simple de lambda
          lambdas <- seq(-2, 2, by = 0.1)
          loglikes <- sapply(lambdas, function(lam) {
            if (abs(lam) < 0.001) {
              y <- log(x)
            } else {
              y <- (x^lam - 1) / lam
            }
            n <- length(y)
            jacob <- (lam - 1) * sum(log(x))
            -n/2 * log(var(y)) + jacob
          })
          best_lambda <- lambdas[which.max(loglikes)]
          
          if (abs(best_lambda) < 0.001) {
            t_bc <- log(x)
          } else {
            t_bc <- (x^best_lambda - 1) / best_lambda
          }
          
          secuencia[["Box-Cox"]] <- t_bc
          secuencia_nombres <- c(secuencia_nombres, "Box-Cox")
          
        }, error = function(e) {
          # Box-Cox falló, no incluir
        })
      }
      
      # 6. YEO-JOHNSON: Siempre aplicable
      tryCatch({
        # Implementación nativa
        yj_transform <- function(x, lambda) {
          result <- numeric(length(x))
          pos <- x >= 0
          if (abs(lambda) < 0.001) {
            result[pos] <- log(x[pos] + 1)
          } else {
            result[pos] <- ((x[pos] + 1)^lambda - 1) / lambda
          }
          neg <- x < 0
          if (abs(lambda - 2) < 0.001) {
            result[neg] <- -log(-x[neg] + 1)
          } else {
            result[neg] <- -((-x[neg] + 1)^(2 - lambda) - 1) / (2 - lambda)
          }
          result
        }
        
        yj_loglike <- function(lambda) {
          y <- yj_transform(x, lambda)
          n <- length(y)
          jacob <- sum(sign(x) * log(abs(x) + 1))
          -n/2 * log(var(y)) + jacob
        }
        
        lambdas <- seq(-2, 2, by = 0.1)
        loglikes <- sapply(lambdas, yj_loglike)
        best_lambda <- lambdas[which.max(loglikes)]
        
        # Refinamiento
        lambdas_fino <- seq(best_lambda - 0.1, best_lambda + 0.1, by = 0.01)
        loglikes_fino <- sapply(lambdas_fino, yj_loglike)
        best_lambda <- lambdas_fino[which.max(loglikes_fino)]
        
        t_yj <- yj_transform(x, best_lambda)
        secuencia[["Yeo-Johnson"]] <- t_yj
        secuencia_nombres <- c(secuencia_nombres, "Yeo-Johnson")
        
      }, error = function(e) {
        # Yeo-Johnson falló
      })
      
      # 7. REFLEJAR + LOG: Para sesgo negativo
      skew_orig <- skewness_nativo(x)
      if (skew_orig < -0.5) {
        secuencia[["Reflejar + Log"]] <- log(max(x, na.rm = TRUE) - x + 1)
        secuencia_nombres <- c(secuencia_nombres, "Reflejar + Log")
      }
      
      # ================================================================
      # EVALUAR SECUENCIA: Calcular métricas y encontrar la primera "buena"
      # ================================================================
      
      resultados <- list()
      analisis_secuencial <- data.frame(
        Paso = integer(),
        Transformación = character(),
        Asimetría = numeric(),
        AsimetriaAbs = numeric(),
        EsNormal = character(),
        Estado = character(),
        stringsAsFactors = FALSE
      )
      
      primera_buena <- NULL
      primera_buena_nombre <- NULL
      primera_buena_encontrada <- FALSE
      
      for (i in seq_along(secuencia_nombres)) {
        nombre <- secuencia_nombres[i]
        datos <- secuencia[[nombre]]
        
        metricas <- calcular_metricas_nativo(datos, nombre)
        resultados[[nombre]] <- metricas
        
        skew_abs <- abs(metricas$skewness)
        es_buena <- isTRUE(skew_abs < 0.5)
        
        estado <- if (es_buena) {
          if (!primera_buena_encontrada) {
            primera_buena <- metricas
            primera_buena_nombre <- nombre
            primera_buena_encontrada <- TRUE
            "DETENER - Distribucion aceptable"
          } else {
            "Tambien aceptable"
          }
        } else {
          "Seguir probando"
        }
        
        analisis_secuencial <- rbind(analisis_secuencial, data.frame(
          Paso = i,
          Transformación = nombre,
          Asimetría = metricas$skewness,
          AsimetriaAbs = skew_abs,
          EsNormal = ifelse(isTRUE(metricas$es_normal), "Si", "No"),
          Estado = estado,
          stringsAsFactors = FALSE
        ))
      }
      
      # Si ninguna fue buena, tomar la mejor de todas
      if (is.null(primera_buena)) {
        skews <- sapply(resultados, function(r) abs(r$skewness))
        skews <- skews[!is.na(skews)]
        if (length(skews) > 0) {
          primera_buena_nombre <- names(which.min(skews))
          primera_buena <- resultados[[primera_buena_nombre]]
        }
      }
      
      # Calcular mejora
      skew_orig_abs <- abs(diag_original$skewness)
      skew_mejor_abs <- abs(primera_buena$skewness)
      mejora_pct <- if (skew_orig_abs > 0) {
        round((1 - skew_mejor_abs / skew_orig_abs) * 100, 1)
      } else {
        0
      }
      
      list(
        x_original = x,
        transformaciones = secuencia,
        resultados = resultados,
        analisis_secuencial = analisis_secuencial,
        primera_buena_nombre = primera_buena_nombre,
        primera_buena = primera_buena,
        mejora_pct = mejora_pct,
        diag_original = diag_original,
        tiene_ceros = tiene_ceros,
        tiene_negativos = tiene_negativos,
        skew_original = skew_orig
      )
    })
    
    # -------------------------------------------------------------------------
    # 4. OUTPUTS
    # -------------------------------------------------------------------------
    
    # --- Diagnóstico original ---
    output$diagnostico_original <- renderTable({
      req(analisis_completo())
      a <- analisis_completo()
      d <- a$diag_original
      
      data.frame(
        Métrica = c(
          "Observaciones (n)", "Media", "Mediana", "Desviación Estándar",
          "Asimetría (Skewness)", "Curtosis (Exceso)",
          paste0("Test de Normalidad (", d$test_nombre, ")"),
          "Estadístico del test", "Valor-p", "¿Es normal?",
          "Outliers detectados", "% de Outliers",
          "Límite inferior IQR", "Límite superior IQR"
        ),
        Valor = c(
          d$n, d$media, d$mediana, d$desv,
          d$skewness, d$curtosis, "",
          d$test_estadistico, d$test_pvalor,
          ifelse(isTRUE(d$es_normal), "Si", "No"),
          d$outliers, paste0(d$pct_outliers, "%"),
          d$li, d$ls
        )
      )
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    # --- Alerta sobre distribución - CORREGIDO ---
    output$alerta_distribucion <- renderUI({
      req(analisis_completo())
      a <- analisis_completo()
      d <- a$diag_original
      
      # Forzar valores escalares con isTRUE()
      es_normal <- isTRUE(d$es_normal)
      tiene_neg <- isTRUE(a$tiene_negativos)
      tiene_cer <- isTRUE(a$tiene_ceros)
      
      color <- if (es_normal) "success" else "warning"
      icono <- if (es_normal) "OK" else "ATENCION"
      mensaje <- if (es_normal) {
        "La distribución original es aproximadamente normal. No se requiere transformación."
      } else {
        paste0("La distribución original NO es normal (p = ", d$test_pvalor, 
               "). Se analizarán transformaciones automáticamente.")
      }
      
      div(class = paste0("alert alert-", color), style = "margin-top: 10px;",
          h5(icono, " Conclusión del diagnóstico"),
          p(mensaje),
          if (!es_normal && tiene_neg) {
            p("La variable contiene valores negativos. Algunas transformaciones no están disponibles.")
          },
          if (!es_normal && tiene_cer && !tiene_neg) {
            p("La variable contiene ceros. Se usará Log(x+1) en lugar de Log(x).")
          }
      )
    })
    
    # --- ANÁLISIS SECUENCIAL (nuevo) ---
    output$analisis_secuencial <- renderUI({
      req(analisis_completo())
      a <- analisis_completo()
      
      # Construir narrativa paso a paso
      pasos <- a$analisis_secuencial
      
      div(
        h5("Proceso de decisión interno:"),
        lapply(1:nrow(pasos), function(i) {
          p <- pasos[i, ]
          color_paso <- if (grepl("DETENER", p$Estado)) "green" else if (grepl("Seguir", p$Estado)) "orange" else "gray"
          
          div(
            style = paste0("border-left: 3px solid ", color_paso, "; padding-left: 10px; margin: 5px 0;"),
            strong(paste0("Paso ", p$Paso, ": ", p$Transformación)),
            br(),
            span(style = "color: #666;", 
                 paste0("Asimetría: ", p$Asimetría, " → |", p$AsimetriaAbs, "|")),
            br(),
            span(style = paste0("color: ", color_paso, "; font-weight: bold;"), p$Estado)
          )
        }),
        
        if (!is.null(a$primera_buena)) {
          div(
            style = "background: #e8f5e9; padding: 15px; border-radius: 5px; margin-top: 15px;",
            h4("Decisión final del algoritmo"),
            p(strong("Transformación seleccionada: "), a$primera_buena_nombre),
            if (a$primera_buena_nombre == "Original") {
              p("La distribución original ya es suficientemente simétrica. No se requiere transformación.")
            } else {
              p("Se detuvo aquí porque |asimetría| < 0.5. Las transformaciones más complejas no son necesarias.")
            }
          )
        }
      )
    })
    
    # --- Tabla secuencial (resumen) ---
    output$tabla_secuencial <- renderTable({
      req(analisis_completo())
      a <- analisis_completo()
      
      df <- a$analisis_secuencial
      df$Detener <- ifelse(grepl("DETENER", df$Estado), "DETENER AQUI", "")
      df$Estado <- NULL  # Quitar columna larga, ya se ve en el UI
      
      df
      
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    # --- Tabla comparativa completa - CORREGIDO ---
    output$tabla_comparacion <- renderTable({
      req(analisis_completo())
      a <- analisis_completo()
      
      # Inicializar dataframe vacío con nombres de columnas correctos
      df_comp <- data.frame(
        Transformacion = character(),
        Asimetria = numeric(),
        AsimetriaAbs = numeric(),
        Curtosis = numeric(),
        TestNormalidad = character(),
        EsNormal = character(),
        Outliers = integer(),
        PctOutliers = numeric(),
        stringsAsFactors = FALSE
      )
      
      nombres_resultados <- names(a$resultados)
      
      for (i in seq_along(nombres_resultados)) {
        nombre <- nombres_resultados[i]
        r <- a$resultados[[nombre]]
        
        if (is.null(r)) next
        
        # Extraer valores con manejo seguro - forzar escalares
        skew_val <- as.numeric(r$skewness)[1]
        if (is.na(skew_val)) skew_val <- NA_real_
        
        curt_val <- as.numeric(r$curtosis)[1]
        if (is.na(curt_val)) curt_val <- NA_real_
        
        test_nom <- as.character(r$test_nombre)[1]
        if (is.na(test_nom) || is.null(test_nom)) test_nom <- "N/A"
        
        test_p <- as.character(r$test_pvalor)[1]
        if (is.na(test_p) || is.null(test_p)) test_p <- "N/A"
        
        es_norm <- as.logical(r$es_normal)[1]
        if (is.na(es_norm) || is.null(es_norm)) es_norm <- FALSE
        
        outl <- as.integer(r$outliers)[1]
        if (is.na(outl) || is.null(outl)) outl <- 0L
        
        pct_out <- as.numeric(r$pct_outliers)[1]
        if (is.na(pct_out) || is.null(pct_out)) pct_out <- 0
        
        # Crear fila segura
        nueva_fila <- data.frame(
          Transformacion = as.character(nombre),
          Asimetria = round(skew_val, 4),
          AsimetriaAbs = round(abs(skew_val), 4),
          Curtosis = round(curt_val, 4),
          TestNormalidad = paste0(test_nom, " (p=", test_p, ")"),
          EsNormal = ifelse(isTRUE(es_norm), "Si", "No"),
          Outliers = outl,
          PctOutliers = pct_out,
          stringsAsFactors = FALSE
        )
        
        df_comp <- rbind(df_comp, nueva_fila)
      }
      
      if (nrow(df_comp) == 0) {
        return(data.frame(Mensaje = "No se pudieron evaluar transformaciones"))
      }
      
      # Marcar la recomendada por el algoritmo secuencial
      df_comp$Recomendada <- ifelse(df_comp$Transformacion == a$primera_buena_nombre, "MEJOR", "")
      
      df_comp
      
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    # --- Recomendación principal ---
    output$recomendacion_principal <- renderUI({
      req(analisis_completo())
      a <- analisis_completo()
      
      div(class = "alert alert-info",
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   color: white; border: none;",
          h4("Recomendación del Algoritmo Secuencial"),
          p(strong("Transformación recomendada: "), a$primera_buena_nombre),
          p("Asimetría original: ", strong(a$diag_original$skewness),
            " → Asimetría transformada: ", strong(a$primera_buena$skewness)),
          p("Mejora en simetría: ", strong(paste0(a$mejora_pct, "%"))),
          if (a$primera_buena_nombre == "Original") {
            p("La distribución original ya es suficientemente simétrica (|skew| < 0.5).")
          } else {
            p("El algoritmo se detuvo aquí porque logró |asimetría| < 0.5. 
               Transformaciones más complejas (Box-Cox, Yeo-Johnson) no son necesarias 
               aunque podrían dar ligeramente mejor resultado.")
          }
      )
    })
    
    # --- Selector de transformación ---
    output$selector_transformacion <- renderUI({
      req(analisis_completo())
      a <- analisis_completo()
      
      opciones <- setNames(
        names(a$transformaciones),
        sapply(names(a$transformaciones), function(n) {
          if (n == a$primera_buena_nombre) {
            paste0("OK ", n, " (Recomendada por secuencia)")
          } else {
            n
          }
        })
      )
      
      selectInput(session$ns("metodo"), "Selecciona la transformación:",
                  choices = opciones, selected = a$primera_buena_nombre, width = "100%")
    })
    
    # --- Detalle de transformación seleccionada ---
    output$detalle_transformacion <- renderText({
      req(input$metodo)
      req(analisis_completo())
      a <- analisis_completo()
      r <- a$resultados[[input$metodo]]
      
      paste0(
        "Transformación: ", input$metodo, "\n",
        "Asimetría: ", r$skewness, "\n",
        "Curtosis: ", r$curtosis, "\n",
        "Test: ", r$test_nombre, " (p=", r$test_pvalor, ")\n",
        "¿Normal? ", ifelse(isTRUE(r$es_normal), "Si", "No"), "\n",
        "Outliers: ", r$outliers, " (", r$pct_outliers, "%)"
      )
    })
    
    # -------------------------------------------------------------------------
    # 5. APLICAR TRANSFORMACIÓN
    # -------------------------------------------------------------------------
    
    vals <- reactiveValues(ultima_transformacion = NULL)
    
    observeEvent(input$aplicar, {
      
      req(input$metodo)
      req(analisis_completo())
      
      a <- analisis_completo()
      
      if (!input$metodo %in% names(a$transformaciones)) {
        showNotification("Transformación no válida.", type = "error")
        return()
      }
      
      x_transformado <- a$transformaciones[[input$metodo]]
      var_original <- input$variable
      
      sufijo <- switch(input$metodo,
                       "Original" = "_copy",
                       "Log(x)" = "_log",
                       "Log(x+1)" = "_log1",
                       "Raíz cuadrada" = "_sqrt",
                       "Box-Cox" = "_boxcox",
                       "Yeo-Johnson" = "_yj",
                       "Reflejar + Log" = "_reflog",
                       "_trans")
      
      nuevo_nombre <- paste0(var_original, sufijo)
      data <- df_global()
      
      # Evitar duplicados
      contador <- 1
      nombre_final <- nuevo_nombre
      while (nombre_final %in% names(data)) {
        nombre_final <- paste0(nuevo_nombre, "_", contador)
        contador <- contador + 1
      }
      
      data[[nombre_final]] <- x_transformado
      df_global(data)
      
      vals$ultima_transformacion <- list(
        nombre_original = var_original,
        nombre_nueva = nombre_final,
        metodo = input$metodo,
        datos_original = a$x_original,
        datos_transformado = x_transformado,
        metricas_original = a$diag_original,
        metricas_nueva = a$resultados[[input$metodo]]
      )
      
      showNotification(paste0("Variable creada: ", nombre_final), 
                       type = "message", duration = 5)
    })
    
    # -------------------------------------------------------------------------
    # 6. COMPARACIÓN ANTES/DESPUÉS
    # -------------------------------------------------------------------------
    
    output$comparacion_antes_despues <- renderTable({
      req(vals$ultima_transformacion)
      t <- vals$ultima_transformacion
      
      data.frame(
        Métrica = c(
          "Variable", "Método", "Observaciones", "Media", "Mediana",
          "Desv. Estándar", "Asimetría", "Curtosis", "Test Normalidad",
          "Valor-p", "¿Es normal?", "Outliers", "% Outliers"
        ),
        Antes = c(
          t$nombre_original, "Original", t$metricas_original$n,
          t$metricas_original$media, t$metricas_original$mediana,
          t$metricas_original$desv, t$metricas_original$skewness,
          t$metricas_original$curtosis,
          paste0(t$metricas_original$test_nombre, " (", t$metricas_original$test_estadistico, ")"),
          t$metricas_original$test_pvalor,
          ifelse(isTRUE(t$metricas_original$es_normal), "Si", "No"),
          t$metricas_original$outliers,
          paste0(t$metricas_original$pct_outliers, "%")
        ),
        Después = c(
          t$nombre_nueva, t$metodo, t$metricas_nueva$n,
          t$metricas_nueva$media, t$metricas_nueva$mediana,
          t$metricas_nueva$desv, t$metricas_nueva$skewness,
          t$metricas_nueva$curtosis,
          paste0(t$metricas_nueva$test_nombre, " (", t$metricas_nueva$test_estadistico, ")"),
          t$metricas_nueva$test_pvalor,
          ifelse(isTRUE(t$metricas_nueva$es_normal), "Si", "No"),
          t$metricas_nueva$outliers,
          paste0(t$metricas_nueva$pct_outliers, "%")
        )
      )
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    output$histograma_comparacion <- renderPlot({
      req(vals$ultima_transformacion)
      t <- vals$ultima_transformacion
      
      par(mfrow = c(1, 2))
      
      # Boxplot original - escala propia
      boxplot(t$datos_original,
              main = paste0("Original: ", t$nombre_original),
              ylab = "Valor",
              col = "lightblue",
              border = "darkblue",
              notch = FALSE)
      points(1, mean(t$datos_original, na.rm = TRUE), col = "red", pch = 19, cex = 1.5)
      legend("topright", legend = c("Media", "Mediana (linea)"), 
             col = c("red", "darkblue"), pch = c(19, NA), lty = c(NA, 1), lwd = 2)
      
      # Boxplot transformado - escala propia
      boxplot(t$datos_transformado,
              main = paste0(t$metodo, ": ", t$nombre_nueva),
              ylab = "Valor transformado",
              col = "lightgreen",
              border = "darkgreen",
              notch = FALSE)
      points(1, mean(t$datos_transformado, na.rm = TRUE), col = "red", pch = 19, cex = 1.5)
      legend("topright", legend = c("Media", "Mediana (linea)"), 
             col = c("red", "darkgreen"), pch = c(19, NA), lty = c(NA, 1), lwd = 2)
      
      par(mfrow = c(1, 1))
    })
    
  })
}
