linreg <- function(regressionSource,
                   regressionLength,
                   regressionOffset) {
  n <- length(regressionSource)
  if (regressionLength < 2 ||
      regressionLength > n) {
    stop("Invalid regression length")
  }
  if (regressionOffset < 0 ||
      regressionOffset >= regressionLength ||
      regressionOffset + regressionLength > n) {
    stop("Invalid regression offset")
  }

  end_index <- n - regressionOffset
  start_index <- end_index - regressionLength + 1
  subset <- regressionSource[start_index:end_index]
  x <- seq_along(subset)
  mean_x <- sum(x) / length(x)
  mean_y <- sum(subset) / length(subset)
  numerator <- sum((x - mean_x) * (subset - mean_y))
  denominator <- sum((x - mean_x)^2)
  slope <- numerator / denominator
  intercept <- mean_y - slope * mean_x
  predicted_values <- slope * x + intercept

  return(list(slope = slope,
              intercept = intercept,
              predicted_values = predicted_values))
}
