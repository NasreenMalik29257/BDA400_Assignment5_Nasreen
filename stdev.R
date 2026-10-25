stdev <- function(data) {
  if (length(data) == 0) stop("Empty dataset")
  avg <- sum(data) / length(data)
  differences <- data - avg
  squared <- differences^2
  variance <- sum(squared) / length(data)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}
