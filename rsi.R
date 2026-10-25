rsi <- function(data, period) {
  n <- length(data)
  if (period < 1 || n <= period) {
    stop("Insufficient data for RSI")
  }
  changes <- diff(data)
  gains <- pmax(changes, 0)
  losses <- pmax(-changes, 0)
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  result <- rep(NA_real_, n)

  calculate_rsi <- function(gain, loss) {
    if (loss == 0 && gain == 0) return(50)
    if (loss == 0) return(100)
    100 - 100 / (1 + gain / loss)
  }

  result[period + 1] <- calculate_rsi(avg_gain, avg_loss)

  if (n > period + 1) {
    for (i in (period + 2):n) {
      avg_gain <- (avg_gain * (period - 1) +
                   gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) +
                   losses[i - 1]) / period
      result[i] <- calculate_rsi(avg_gain, avg_loss)
    }
  }
  return(result)
}
