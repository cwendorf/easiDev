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
    plot(estimateMeansSubsets(x, ..., contrast = contrast))
  } else if (howmany == 2) {
    plot(estimateMeansComparison(x, ...))
  } else {
    plot(estimateMeansSet(x, ...))
  }
  invisible(eval(x))
}


### Means Set

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
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- rownames(moments)
  comment(results) <- "Confidence Intervals for the Means"
  class(results) <- c("easi")
  return(results)
}
