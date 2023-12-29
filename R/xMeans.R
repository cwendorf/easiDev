# Estimation Approach to Statistical Inference
## Means

### Confidence Intervals

estimateMeans <- function(x, ...) {
  UseMethod("estimateMeans")
}

estimateMeans.wss <- estimateMeans.bss <- function(DescStats, mu = 0, conf.level = .95, ...) {
  N <- DescStats[, "N"]
  M <- DescStats[, "M"]
  SD <- DescStats[, "SD"]
  Est <- M - mu
  df <- N - 1
  SE <- SD / sqrt(N)
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- rownames(DescStats)
  out <- list(results)
  names(out) <- "Confidence Intervals for the Means"
  class(out) <- c("easi", "list")
  return(out)
}

### Confidence Interval Plots

plotMeans <- function(x, ...) {
  UseMethod("plotMeans")
}

plotMeans.wss <- function(..., mu = 0, conf.level = .95, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, line = NULL, rope = NULL, values = TRUE, digits = 3, pos = 2, pch = 16, col = "black", connect = TRUE, offset = 0, intervals = TRUE) {
  results <- estimateMeans(..., mu = mu, conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, line = line, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
  invisible(eval(...))
}

plotMeans.bss <- function(..., mu = 0, conf.level = .95, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, line = NULL, rope = NULL, values = TRUE, digits = 3, pos = 2, pch = 16, col = "black", connect = FALSE, offset = 0, intervals = TRUE) {
  results <- estimateMeans(..., mu = mu, conf.level = conf.level, main = main, digits = digits)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, line = line, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
  invisible(eval(...))
}

addMeans <- function(...) {
  plotMeans(..., add = TRUE)
}