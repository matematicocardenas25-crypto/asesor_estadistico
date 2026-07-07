# =============================
# UI
# =============================
mod_guia_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("guia"))
}

# =============================
# SERVER
# =============================
mod_guia_server <- function(id, df, vars, input_main){
  moduleServer(id, function(input, output, session){

    es_categorica <- function(x){
      is.factor(x) || is.character(x) || is.logical(x)
    }

    output$guia <- renderUI({

      # =============================
      # SIN DATOS
      # =============================
      if(is.null(df())){
        return(HTML("
        <div style='text-align: center; padding: 60px 20px;'>
          <h2>🎓 Bienvenido a tu Asesor Estadístico</h2>
          <p>Plataforma interactiva para análisis descriptivo e inferencial</p>
          <p><b>Para comenzar:</b> Carga un archivo de datos</p>
          
          <div style='margin-top:25px; padding:15px; background:#f8f9fa; border-radius:8px;'>
            <b>Formatos soportados:</b><br>
            CSV, Excel (.xlsx, .xls), TXT, SPSS (.sav), Stata (.dta), SAS
          </div>
        </div>
        "))
      }

      # =============================
      # SIN VARIABLES
      # =============================
      if(is.null(vars()) || length(vars()) == 0){
        return(HTML("
        <div style='text-align: center; padding: 60px 20px;'>
          <h3>📁 Archivo cargado correctamente</h3>
          <p>Ahora selecciona variables para analizar</p>
        </div>
        "))
      }

      data <- df()
      v <- vars()
      cruce <- input_main$tipo_cruce
      enfoque <- input_main$tipo_enfoque

      # VALIDACIÓN
      if(cruce == "uni" && length(v) != 1){
        return(HTML("<p>Selecciona exactamente 1 variable.</p>"))
      }

      if(cruce == "bi" && length(v) != 2){
        return(HTML("<p>Selecciona exactamente 2 variables.</p>"))
      }

      # =============================
      # UNIVARIADO
      # =============================
      if(cruce == "uni"){

        x <- data[[v[1]]]

        # -------- OUTLIERS ----------
        alerta <- ""
        if(is.numeric(x)){
          at <- detectar_atipicos(x)

          if(at$hay_atipicos){
            alerta <- paste0(
              "<div class='alert-box alert-warning'>
              <b>⚠️ Valores atípicos detectados</b><br>
              Cantidad: ", at$n_atipicos, " (", at$porcentaje, "%)<br>
              Valores: ", paste(round(at$valores,2), collapse=", "), "<br>
              Límites IQR: [", round(at$lim_inf,2), ", ", round(at$lim_sup,2), "]<br><br>
              <b>Recomendación:</b> evaluar eliminación, winsorización o uso de medidas robustas.
              </div>"
            )
          } else {
            alerta <- "
            <div class='alert-box alert-info'>
            ✅ No se detectaron valores atípicos
            </div>"
          }
        }

        # -------- NUMÉRICO ----------
        if(is.numeric(x)){

          if(enfoque == "desc"){

            return(HTML(paste0(
              "<h3>Diagnóstico: Univariado Numérico - Descriptivo</h3>
              <p>La variable es cuantitativa continua. Se aplican estadísticos descriptivos completos.</p>",
              alerta,
              "<ul>
                <li>Media y mediana</li>
                <li>Varianza y desviación estándar</li>
                <li>Coeficiente de variación</li>
                <li>Asimetría y curtosis</li>
                <li>Histograma y densidad</li>
              </ul>"
            )))

          } else {

            return(HTML(paste0(
              "<h3>Diagnóstico: Univariado Numérico - Inferencial</h3>
              <p>Se evalúan supuestos de normalidad antes de inferencia.</p>",
              alerta,
              "<ul>
                <li>Shapiro-Wilk (n ≤ 50)</li>
                <li>Kolmogorov-Smirnov (n > 50)</li>
                <li>Jarque-Bera</li>
              </ul>
              <p><b>Decisión:</b> si no hay normalidad → pruebas no paramétricas.</p>"
            )))
          }

        } else {

          # -------- CATEGÓRICO ----------
          if(enfoque == "desc"){

            return(HTML("
              <h3>Diagnóstico: Variable Categórica</h3>
              <p>Análisis de frecuencias absolutas y relativas.</p>
            "))

          } else {

            return(HTML("
              <h3>Diagnóstico: Variable Categórica - Inferencial</h3>
              <p>Aplicar Chi-cuadrado o prueba binomial.</p>
            "))
          }
        }
      }

      # =============================
      # BIVARIADO
      # =============================
      else {

        x1 <- data[[v[1]]]
        x2 <- data[[v[2]]]

        if(is.numeric(x1) && is.numeric(x2)){

          if(enfoque == "desc"){
            return(HTML("
              <h3>Bivariado Numérico</h3>
              <p>Análisis de covarianza y dispersión.</p>
            "))
          } else {
            return(HTML("
              <h3>Bivariado Numérico - Inferencial</h3>
              <p>Correlación de Pearson y regresión lineal.</p>
            "))
          }

        } else if(es_categorica(x1) && es_categorica(x2)){

          if(enfoque == "desc"){
            return(HTML("
              <h3>Categórico vs Categórico</h3>
              <p>Tablas de contingencia.</p>
            "))
          } else {
            return(HTML("
              <h3>Categórico vs Categórico - Inferencial</h3>
              <p>Prueba Chi-cuadrado de independencia.</p>
            "))
          }

        } else {

          if(enfoque == "desc"){
            return(HTML("
              <h3>Mixto (Numérico vs Categórico)</h3>
              <p>Comparación descriptiva por grupos.</p>
            "))
          } else {
            return(HTML("
              <h3>Mixto - Inferencial</h3>
              <p>ANOVA, Bartlett y pruebas de varianza.</p>
            "))
          }
        }
      }

    })

  })
}
