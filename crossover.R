crossover <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Arrays must have equal length")
  }
  n <- length(arr1)
  signals <- rep("None", n)
  if (n > 1) {
    for (i in 2:n) {
      if (arr1[i] > arr2[i] &&
          arr1[i-1] <= arr2[i-1]) {
        signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] &&
                 arr1[i-1] >= arr2[i-1]) {
        signals[i] <- "Down"
      }
    }
  }
  return(signals)
}
