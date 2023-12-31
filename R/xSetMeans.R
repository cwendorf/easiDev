# Estimation Approach to Statistical Inference
## All Means

### Confidence Intervals

estimateSet <- function(x, ...) {
  UseMethod("estimateSetMeans")
}

estimateSetMeans <- function(x, ...) {
  UseMethod("estimateSetMeans")
}

estimateSetMeans.wsm <- estimateSetMeans.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  Est <- M - mu
  df <- N - 1
  SE <- SD / sqrt(N)
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- rownames(moments)
  output <- list(results)
  names(output) <- "Confidence Intervals for the Means"
  class(output) <- c("easi", "list")
  return(output)
}

estimateSetMeans.data.frame <- function(frame, conf.level = .95, mu = 0, ...) {
  moments <- describeMoments(frame)
  estimateSetMeans(moments, conf.level = conf.level, mu = mu, ...)
}

estimateSetMeans.formula <- function(formula, conf.level = .95, mu = 0, ...) {
  moments <- describeMoments(formula)
  estimateSetMeans(moments, conf.level = conf.level, mu = mu, ...)
}

### Confidence Interval Plots

plotSet <- function(x, ...) {
  UseMethod("plotSetMeans")
}

plotSetMeans <- function(x, ...) {
  UseMethod("plotSetMeans")
}

plotSetMeans.wsm <- plotSetMeans.data.frame <- function(..., conf.level = .95, mu = 0, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, line = NULL, rope = NULL, values = TRUE, digits = 3, pos = 2, pch = 16, col = "black", connect = TRUE, offset = 0, intervals = TRUE) {
  results <- estimateSetMeans(..., mu = mu, conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, line = line, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
  invisible(eval(...))
}

plotSetMeans.bsm <- plotSetMeans.formula <- function(..., conf.level = .95, mu = 0, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, line = NULL, rope = NULL, values = TRUE, digits = 3, pos = 2, pch = 16, col = "black", connect = FALSE, offset = 0, intervals = TRUE) {
  results <- estimateSetMeans(..., mu = mu, conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, line = line, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
  invisible(eval(...))
}

addSet <- function(...) {
  plotSetMeans(..., add = TRUE)
}
