# Estimation Approach to Statistical Inference
## Comparison Means

### Confidence Intervals

estimateComparison <- function(x, ...) {
  UseMethod("estimateComparisonMeans")
}

estimateComparisonMeans <- function(x, ...) {
  UseMethod("estimateComparisonMeans")
}

estimateComparisonMeans.bsm <- function(moments, conf.level = .95, mu = 0, main = NULL, ...) {
  Levels <- estimateSet(moments, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifference(moments, conf.level = conf.level, mu = 0, ...)
  output <- c(Levels, Diff)
  class(output) <- c("easi", "list")
  return(output)
}

estimateComparisonMeans.wsm <- function(moments, corrs, conf.level = .95, mu = 0, main = NULL, ...) {
  Levels <- estimateSet(moments, corrs, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifference(moments, corrs, conf.level = conf.level, mu = 0, ...)
  output <- c(Levels, Diff)
  class(output) <- c("easi", "list")
  return(output)
}

estimateComparisonMeans.data.frame <- function(frame, conf.level = .95, mu = 0, main = NULL, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateComparisonMeans(moments, corrs, conf.level = conf.level, mu = 0, main = main, labels = labels, ...)
}

estimateComparisonMeans.formula <- function(formula, conf.level = .95, mu = 0, main = NULL, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateComparisonMeans(moments, conf.level = conf.level, mu = 0, main = main, labels = labels, ...)
}

### Confidence Interval Plots

plotComparison <- function(x, ...) {
  UseMethod("plotComparisonMeans")
}

plotComparisonMeans <- function(x, ...) {
  UseMethod("plotComparisonMeans")
}

plotComparisonMeans.wsm <- function(moments, corrs, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, ylim = NULL, digits = 3, connect = TRUE, pos = c(2, 2, 4), pch = c(16, 16, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateComparisonMeans(moments, corrs, conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotComparisonMeans.bsm <- function(moments, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, ylim = NULL, digits = 3, connect = FALSE, pos = c(2, 2, 4), pch = c(16, 16, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateComparisonMeans(moments, conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotComparisonMeans.data.frame <- function(frame, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, ylim = NULL, digits = 3, connect = TRUE, pos = c(2, 2, 4), pch = c(16, 16, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateComparisonMeans(frame, conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotComparisonMeans.formula <- function(formula, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, ylim = NULL, digits = 3, connect = FALSE, pos = c(2, 2, 4), pch = c(16, 16, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateComparisonMeans(formula, conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}