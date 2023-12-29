# Estimation Approach to Statistical Inference
## Means Comparison

### Confidence Intervals

estimateComparison <- function(x, ...) {
  UseMethod("estimateComparison")
}

estimateComparison.bsm <- function(DescStats, conf.level = .95, mu = 0, ...) {
  Levels <- estimateMeans(DescStats, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifference(DescStats, conf.level = conf.level, mu = 0, ...)
  out <- c(Levels, Diff)
  class(out) <- c("easi", "list")
  return(out)
}

estimateComparison.wsm <- function(DescStats, CorrStats, conf.level = .95, mu = 0, ...) {
  Levels <- estimateMeans(DescStats, CorrStats, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifference(DescStats, CorrStats, conf.level = conf.level, mu = 0, ...)
  out <- c(Levels, Diff)
  class(out) <- c("easi", "list")
  return(out)
}

### Confidence Interval Plots

plotComparison <- function(x, ...) {
  UseMethod("plotComparison")
}

plotComparison.wsm <- function(..., add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, ylim = NULL, digits = 3, connect = TRUE, pos = c(2, 2, 4), pch = c(16, 16, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateComparison(..., conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotComparison.bsm <- function(..., add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, ylim = NULL, digits = 3, connect = FALSE, pos = c(2, 2, 4), pch = c(16, 16, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateComparison(..., conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}