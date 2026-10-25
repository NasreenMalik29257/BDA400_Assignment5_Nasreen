sma <- function(data, period) {
  if (period < 1 || period > length(data)) {
    stop("Invalid SMA period")
  }
  result <- numeric(length(data) - period + 1)
  for (i in seq_along(result)) {
    result[i] <- sum(data[i:(i + period - 1)]) / period
  }
  return(result)
}
