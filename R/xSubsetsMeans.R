# Estimation Approach to Statistical Inference
## Subsets Means

### Confidence Interval Functions

estimateSubsets <- function(x, ...) {
  UseMethod("estimateSubsetsMeans")
}

estimateSubsetsMeans <- function(x, ...) {
  UseMethod("estimateSubsetsMeans")
}

estimateSubsetsMeans.wsm <- function(moments, corrs, contrast, conf.level = .95, labels = NULL, ...) {
  con1 <- ifelse(contrast < 0, 0, contrast)
  res1 <- estimateContrastMeans(moments, corrs, contrast = con1, conf.level = conf.level)[[1]]
  con2 <- ifelse(contrast > 0, 0, abs(contrast))
  res2 <- estimateContrastMeans(moments, corrs, contrast = con2, conf.level = conf.level)[[1]]
  Subsets <- rbind(res2, res1)
  if (is.null(labels)) {
    rownames(Subsets) <- c("Neg Weighted", "Pos Weighted")
  } else {
    rownames(Subsets) <- labels
  }
  Diff <- estimateContrastMeans(moments, corrs, contrast = contrast, conf.level = conf.level)[[1]]
  output <- list(Subsets, Diff)
  names(output) <- c("Confidence Intervals for the Subsets of Means", "Confidence Interval for the Contrast of Means")
  class(output) <- c("easi", "list")
  return(output)
}

estimateSubsetsMeans.bsm <- function(moments, contrast, conf.level = .95, labels = NULL, ...) {
  con1 <- ifelse(contrast < 0, 0, contrast)
  res1 <- estimateContrastMeans(moments, contrast = con1, conf.level = conf.level)[[1]]
  con2 <- ifelse(contrast > 0, 0, abs(contrast))
  res2 <- estimateContrastMeans(moments, contrast = con2, conf.level = conf.level)[[1]]
  Subsets <- rbind(res2, res1)
  if (is.null(labels)) {
    rownames(Subsets) <- c("Neg Weighted", "Pos Weighted")
  } else {
    rownames(Subsets) <- labels
  }
  Diff <- estimateContrastMeans(moments, contrast = contrast, conf.level = conf.level)[[1]]
  output <- list(Subsets, Diff)
  names(output) <- c("Confidence Intervals for the Subsets of Means", "Confidence Interval for the Contrast of Means")
  class(output) <- c("easi", "list")
  return(output)
}

estimateSubsetsMeans.data.frame <- function(frame, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateSubsetsMeans(moments, corrs, contrast, conf.level = conf.level, labels = labels, ...)
}

estimateSubsetsMeans.formula <- function(formula, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateSubsetsMeans(moments, contrast, conf.level = conf.level, labels = labels, ...)
}

### Confidence Interval Plots

plotSubsets <- function(x, ...) {
  UseMethod("plotSubsetsMeans")
}

plotSubsetsMeans <- function(x, ...) {
  UseMethod("plotSubsetsMeans")
}

plotSubsetsMeans.bsm <- function(moments, contrast, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = c(15, 15, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateSubsetsMeans(moments, contrast = contrast, conf.level = conf.level, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotSubsetsMeans.wsm <- function(moments, corrs, contrast, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = c(15, 15, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateSubsetsMeans(moments, corrs, contrast = contrast, conf.level = conf.level, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotSubsetsMeans.formula <- function(formula, contrast, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = c(15, 15, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateSubsetsMeans(formula, contrast = contrast, conf.level = conf.level, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotSubsetsMeans.data.frame <- function(frame, contrast, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", conf.level = .95, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = c(15, 15, 17), col = "black", offset = 0, intervals = TRUE) {
  results <- estimateSubsetsMeans(frame, contrast = contrast, conf.level = conf.level, labels = labels)
  plotIntervals(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}
