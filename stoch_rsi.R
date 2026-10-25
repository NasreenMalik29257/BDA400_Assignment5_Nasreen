stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  if (length(valid_rsi) < k_period + d_period - 1) {
    stop("Insufficient RSI values")
  }

  minimum <- min(valid_rsi)
  maximum <- max(valid_rsi)

  if (maximum == minimum) {
    normalized <- rep(0, length(valid_rsi))
  } else {
    normalized <- (valid_rsi - minimum) /
                  (maximum - minimum)
  }

  k_line <- sma(normalized, k_period)
  d_line <- sma(k_line, d_period)

  return(list(k_line = k_line, d_line = d_line))
}
