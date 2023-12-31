# Estimation Approach to Statistical Inference
## Subsets Means

### Confidence Interval Functions

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
