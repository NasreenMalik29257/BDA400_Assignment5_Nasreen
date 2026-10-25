ema <- function(data, period) {
  if (length(data) == 0 || period < 1) {
    stop("Invalid EMA input")
  }
  multiplier <- 2 / (period + 1)
  values <- numeric(length(data))
  values[1] <- data[1]
  if (length(data) > 1) {
    for (i in 2:length(data)) {
      values[i] <- (data[i] - values[i-1]) *
        multiplier + values[i-1]
    }
  }
  return(values)
}
