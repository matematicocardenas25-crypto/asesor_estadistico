# --- FUNCIÓN PARA DETECTAR VALORES ATÍPICOS (IQR METHOD) ---
detectar_atipicos <- function(x) {
  x_clean <- na.omit(x)
  if(length(x_clean) < 4) return(list(hay_atipicos = FALSE, n_atipicos = 0, valores = numeric(0)))
  
  Q1 <- quantile(x_clean, 0.25)
  Q3 <- quantile(x_clean, 0.75)
  IQR_val <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR_val
  lim_sup <- Q3 + 1.5 * IQR_val
  
  atipicos <- x_clean[x_clean < lim_inf | x_clean > lim_sup]
  
  list(
    hay_atipicos = length(atipicos) > 0,
    n_atipicos = length(atipicos),
    valores = sort(atipicos),
    lim_inf = lim_inf,
    lim_sup = lim_sup,
    porcentaje = round(100 * length(atipicos) / length(x_clean), 2)
  )
}