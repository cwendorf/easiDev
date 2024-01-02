# Estimation Approach to Statistical Inference
## Means Set

### Estimate

estimateMeansSet <- function(x, ...) {
  UseMethod("estimateMeansSet")
}

estimateMeansSet.wsm <- estimateMeansSet.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  Est <- M - mu
  df <- N - 1
  SE <- SD / sqrt(N)
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- cbind(Est, SE, df, LL, UL)
  rownames(results) <- rownames(moments)
  comment(results) <- "Confidence Intervals for the Means"
  class(results) <- c("easi", "intervalsMain")
  return(results)
}

estimateMeansSet.data.frame <- function(frame, conf.level = .95, mu = 0, ...) {
  moments <- describeMoments(frame)
  estimateMeansSet(moments, conf.level = conf.level, mu = mu, ...)
}

estimateMeansSet.formula <- function(formula, conf.level = .95, mu = 0, ...) {
  moments <- describeMoments(formula)
  estimateMeansSet(moments, conf.level = conf.level, mu = mu, ...)
}

### Plot

plotMeansSet <- function(x, ...) {
  UseMethod("plotMeansSet")
}

plotMeansSet.wsm <- plotMeansSet.data.frame <- function(..., conf.level = .95, mu = 0, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, line = NULL, rope = NULL, values = TRUE, digits = 3, pos = 2, pch = 16, col = "black", connect = TRUE, offset = 0, intervals = TRUE) {
  results <- estimateMeansSet(..., mu = mu, conf.level = conf.level, main = main, digits = digits)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, line = line, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
  invisible(eval(...))
}

plotMeansSet.bsm <- plotMeansSet.formula <- function(..., conf.level = .95, mu = 0, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, line = NULL, rope = NULL, values = TRUE, digits = 3, pos = 2, pch = 16, col = "black", connect = FALSE, offset = 0, intervals = TRUE) {
  results <- estimateMeansSet(..., mu = mu, conf.level = conf.level, main = main, digits = digits)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, line = line, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
  invisible(eval(...))
}

addMeansSet <- function(...) {
  plotMeansSet(..., add = TRUE)
}
