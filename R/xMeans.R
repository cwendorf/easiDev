# Estimation Approach to Statistical Inference
## Means

### Method Selector

estimateMeans <- function(x, ..., contrast = NULL) {
  howmany <- nrow(describeMoments(x))
  if (!is.null(contrast)) {
    estimateMeansSubsets(x, ..., contrast = contrast)
  } else if (howmany == 2) {
    estimateMeansComparison(x, ...)
  } else {
    estimateMeansSet(x, ...)
  }
}

plotMeans <- function(x, ..., contrast = NULL, connect = FALSE) {
  desc <- describeMoments(x)
  howmany <- nrow(desc)
  if (any(class(desc) == "wsm")) connect <- TRUE
  if (!is.null(contrast)) {
    plot(estimateMeansSubsets(x, ..., contrast = contrast), connect = connect)
  } else if (howmany == 2) {
    plot(estimateMeansComparison(x, ...), connect = connect)
  } else {
    plot(estimateMeansSet(x, ...), connect = connect)
  }
  invisible(eval(x))
}
