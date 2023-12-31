# Estimation Approach to Statistical Inference
## Difference Means

### Confidence Intervals

estimateDifference <- function(x, ...) {
  UseMethod("estimateDifferenceMeans")
}

estimateDifferenceMeans <- function(x, ...) {
  UseMethod("estimateDifferenceMeans")
}

estimateDifferenceMeans.wsm <- function(moments, corrs, conf.level = .95, mu = 0, main = NULL, ...) {
  moments <- moments[1:2,]
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  rn <- rownames(moments)
  R <- corrs[rn[1], rn[2]]
  Est <- M[2] - M[1] - mu
  SE <- SD / sqrt(N)
  SE <- sqrt(SE[1]^2 + SE[2]^2 - 2 * R * SE[1] * SE[2])
  df <- min(N) - 1
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- "Comparison"
  output <- list(results)
  if (is.null(main)) main <- "Confidence Interval for the Difference of Means"
  names(output) <- main
  class(output) <- c("easi", "list")
  return(output)
}

estimateDifferenceMeans.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
  moments <- moments[1:2,]
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  Est <- M[2] - M[1] - mu
  SE <- sqrt((SD[1]^2 / N[1]) + (SD[2]^2 / N[2]))
  df <- ((SD[1]^2 / N[1] + SD[2]^2 / N[2])^2) / ((SD[1]^2 / N[1])^2 / (N[1] - 1) + (SD[2]^2 / N[2])^2 / (N[2] - 1))
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- "Comparison"
  output <- list(results)
  names(output) <- "Confidence Interval for the Difference of Means"
  class(output) <- c("easi", "list")
  return(output)
}

estimateDifferenceMeans.data.frame <- function(frame, conf.level = .95, mu = 0, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateDifferenceMeans(moments, corrs, conf.level = conf.level, mu = mu, labels = labels)
}

estimateDifferenceMeans.formula <- function(formula, conf.level = .95, mu = 0, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateDifferenceMeans(moments,conf.level = conf.level,  mu = mu, labels = labels)
}

### Confidence Interval Plots

plotDifference <- function(x, ...) {
  UseMethod("plotDifferenceMeans")
}

plotDifferenceMeans <- function(x, ...) {
  UseMethod("plotDifferenceMeans")
}

plotDifferenceMeans.bsm <- function(moments, add = FALSE, main = NULL, ylab = "Mean Difference", xlab = "", conf.level = .95, mu = 0, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateDifferenceMeans(moments, conf.level = conf.level, mu = mu, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotDifferenceMeans.wsm <- function(moments, corrs, add = FALSE, main = NULL, ylab = "Mean Difference", xlab = "", conf.level = .95, mu = 0, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateDifferenceMeans(moments, corrs, conf.level = conf.level, mu = mu, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotDifferenceMeans.formula <- function(formula, add = FALSE, main = NULL, ylab = "Mean Difference", xlab = "", conf.level = .95, mu = 0, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateDifferenceMeans(formula, conf.level = conf.level, mu = mu, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotDifferenceMeans.data.frame <- function(frame, add = FALSE, main = NULL, ylab = "Mean Difference", xlab = "", conf.level = .95, mu = 0, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateDifferenceMeans(frame, conf.level = conf.level, mu = mu, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

