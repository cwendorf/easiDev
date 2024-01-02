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

plotMeans <- function(x, ..., contrast = NULL) {
  howmany <- nrow(describeMoments(x))
  if (!is.null(contrast)) {
    plotMeansSubsets(x, ..., contrast = contrast)
  } else if (howmany == 2) {
    plotMeansComparison(x, ...)
  } else {
    plotMeansSet(x, ...)
  }
  invisible(eval(x))
}

testMeans <- function(x, ..., contrast = NULL) {
  howmany <- nrow(describeMoments(x))
  if (!is.null(contrast)) {
    testMeansSubsets(x, ..., contrast = contrast)
  } else if (howmany == 2) {
    testMeansComparison(x, ...)
  } else {
    testMeansSet(x, ...)
  }
}
