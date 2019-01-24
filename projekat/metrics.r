rmseMetric <- function(v1, v2) {
  return (sqrt(mean((v1 - v2) ^ 2 , na.rm = TRUE)))
}

rSquareMetric <- function(lm) {
  return (1 - lm$deviance / lm$null.deviance)
}

rSquareAdjustedMetric <- function(lm, n) {
  return (1 - (lm$deviance / (n - length(lm$coefficients))) / (lm$null.deviance / (n - 1)))
}

normalizeDataFrame <- function(data, min = NULL, max = NULL) {
  if(is.null(min)) {
    min <- apply(data, 2, min)
    max <- apply(data, 2, max)
  }
  
  dataNorm <- as.data.frame(scale(data, center = min, scale = max - min))
  
  return (dataNorm)
}

unnormalizeDataFrame <- function(dataNorm, min, max) {
  data <- t((t(dataNorm) * (max - min)) + min)
  
  return (data)
}

getRange <- function(v) {
  return (max(v)-min(v))
}