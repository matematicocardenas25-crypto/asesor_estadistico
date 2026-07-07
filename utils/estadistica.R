# Funciones matemáticas internas para eludir dependencias pesadas y asegurar estabilidad
calcular_skewness <- function(x) {
  n <- length(x)
  if(n < 3) return(NA)
  m2 <- sum((x - mean(x))^2)/n
  m3 <- sum((x - mean(x))^3)/n
  (m3 / (m2^(1.5))) * sqrt(n * (n - 1)) / (n - 2)
}

calcular_kurtosis <- function(x) {
  n <- length(x)
  if(n < 4) return(NA)
  m2 <- sum((x - mean(x))^2)/n
  m4 <- sum((x - mean(x))^4)/n
  ((m4 / (m2^2)) * ((n + 1) * (n - 1)) / ((n - 2) * (n - 3))) - (3 * (n - 1)^2 / ((n - 2) * (n - 3)))
}