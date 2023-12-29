# Estimation Approach to Statistical Inference
## Comparison Means

### Confidence Intervals

estimateComparison <- function(x, ...) {
  UseMethod("estimateComparisonMeans")
}

estimateComparisonMeans <- function(x, ...) {
  UseMethod("estimateComparisonMeans")
}

estimateComparisonMeans.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
  Levels <- estimateMeans(moments, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifference(moments, conf.level = conf.level, mu = 0, ...)
  output <- c(Levels, Diff)
  class(output) <- c("easi", "list")
  return(output)
}

estimateComparisonMeans.wsm <- function(moments, corrs, conf.level = .95, mu = 0, ...) {
  Levels <- estimateMeans(moments, corrs, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifference(moments, corrs, conf.level = conf.level, mu = 0, ...)
  output <- c(Levels, Diff)
  class(output) <- c("easi", "list")
  return(output)
}

### Confidence Interval Plots

plotComparison <- function(x, ...) {
  UseMethod("plotComparisonMeans")
}

plotComparisonMeans <- function(x, ...) {
  UseMethod("plotComparisonMeans")
}

plotComparisonMeans.wsm <- function(..., add = FALSE, main = NULL, ylab = "outputcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, ylim = NULL, digits = 3, connect = TRUE, pos = c(2, 2, 4), pch = c(16, 16, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateComparisonMeans(..., conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotComparisonMeans.bsm <- function(..., add = FALSE, main = NULL, ylab = "outputcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, ylim = NULL, digits = 3, connect = FALSE, pos = c(2, 2, 4), pch = c(16, 16, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateComparisonMeans(..., conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}
