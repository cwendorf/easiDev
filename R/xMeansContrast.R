# Estimation Approach to Statistical Inference
## Means Contrast

### Estimate

estimateMeansContrast <- function(x, ...) {
  UseMethod("estimateMeansContrast")
}

estimateMeansContrast.wsm <- function(moments, corrs, contrast, conf.level = .95, ...) {
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
  results <- cbind(t(c(Est, SE, df, LL, UL)))
  colnames(results) <- c("Est", "SE", "df", "LL", "UL")
  rownames(results) <- c("Contrast")
  comment(results) <- "Confidence Interval for the Contrast of Means"
  class(results) <- c("easi", "intervalsMain")
  return(results)
}

estimateMeansContrast.bsm <- function(moments, contrast, conf.level = .95, ...) {
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
  results <- cbind(t(c(Est, SE, df, LL, UL)))
  colnames(results) <- c("Est", "SE", "df", "LL", "UL")
  rownames(results) <- c("Contrast")
  comment(results) <- "Confidence Interval for the Contrast of Means"
  class(results) <- c("easi", "intervalsMain")
  return(results)
}

estimateMeansContrast.data.frame <- function(frame, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateMeansContrast(moments, corrs, contrast, conf.level = conf.level, labels = labels, ...)
}

estimateMeansContrast.formula <- function(formula, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateMeansContrast(moments, contrast, conf.level = conf.level, labels = labels, ...)
}

### Plot

plotMeansContrast <- function(x, ...) {
  UseMethod("plotMeansContrast")
}

plotMeansContrast.bsm <- function(moments, contrast, add = FALSE, main = NULL, ylab = "Mean Contrast", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateMeansContrast(moments, contrast = contrast, conf.level = conf.level, labels = labels)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotMeansContrast.wsm <- function(moments, corrs, contrast, add = FALSE, main = NULL, ylab = "Mean Contrast", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateMeansContrast(moments, corrs, contrast = contrast, conf.level = conf.level, labels = labels)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotMeansContrast.formula <- function(formula, contrast, add = FALSE, main = NULL, ylab = "Mean Contrast", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateMeansContrast(formula, contrast = contrast, conf.level = conf.level, labels = labels)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotMeansContrast.data.frame <- function(frame, contrast, add = FALSE, main = NULL, ylab = "Mean Contrast", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateMeansContrast(frame, contrast = contrast, conf.level = conf.level, labels = labels)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

addMeansContrast <- function(...) {
  plotMeansContrast(..., add = TRUE)
}
