# Estimation Approach to Statistical Inference
## Contrast Means

### Confidence Intervals

estimateContrast <- function(x, ...) {
  UseMethod("estimateContrastMeans")
}

estimateContrastMeans <- function(x, ...) {
  UseMethod("estimateContrastMeans")
}

estimateContrastMeans.wsm <- function(moments, corrs, contrast, conf.level = .95, ...) {
  N <- min(moments[, "N"])
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  df <- N - 1
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  covstats <- .cortocov(corrs, SD)
  Est <- (t(contrast) %*% M)
  SE <- sqrt(t(contrast) %*% covstats %*% contrast / N)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- as.data.frame(t(c(Est, SE, df, LL, UL)))
  colnames(results) <- c("Est", "SE", "df", "LL", "UL")
  rownames(results) <- c("Contrast")
  output <- list(results)
  names(output) <- "Confidence Interval for the Contrast of Means"
  class(output) <- c("easi", "list")
  return(output)
}

estimateContrastMeans.bsm <- function(moments, contrast, conf.level = .95, ...) {
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  Est <- t(contrast) %*% M
  v <- diag(SD^2) %*% (solve(diag(N)))
  SE <- sqrt(t(contrast) %*% v %*% contrast)
  df <- (SE^4) / sum(((contrast^4) * (SD^4) / (N^2 * (N - 1))))
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- as.data.frame(t(c(Est, SE, df, LL, UL)))
  colnames(results) <- c("Est", "SE", "df", "LL", "UL")
  rownames(results) <- c("Contrast")
  output <- list(results)
  names(output) <- "Confidence Interval for the Contrast of Means"
  class(output) <- c("easi", "list")
  return(output)
}

estimateContrastMeans.data.frame <- function(frame, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateContrastMeans(moments, corrs, contrast, conf.level = conf.level, labels = labels, ...)
}

estimateContrastMeans.formula <- function(formula, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateContrastMeans(moments, contrast, conf.level = conf.level, labels = labels, ...)
}

### Confidence Interval Plots

plotContrast <- function(x, ...) {
  UseMethod("plotContrastMeans")
}

plotContrastMeans <- function(x, ...) {
  UseMethod("plotContrastMeans")
}

plotContrastMeans.bsm <- function(moments, contrast, add = FALSE, main = NULL, ylab = "Mean Contrast", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateContrastMeans(moments, contrast = contrast, conf.level = conf.level, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotContrastMeans.wsm <- function(moments, corrs, contrast, add = FALSE, main = NULL, ylab = "Mean Contrast", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateContrastMeans(moments, corrs, contrast = contrast, conf.level = conf.level, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotContrastMeans.formula <- function(formula, contrast, add = FALSE, main = NULL, ylab = "Mean Contrast", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateContrastMeans(formula, contrast = contrast, conf.level = conf.level, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotContrastMeans.data.frame <- function(frame, contrast, add = FALSE, main = NULL, ylab = "Mean Contrast", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateContrastMeans(frame, contrast = contrast, conf.level = conf.level, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}
