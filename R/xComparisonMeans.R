# Estimation Approach to Statistical Inference
## Comparison Means

### Confidence Intervals

estimateComparisonMeans <- function(x, ...) {
  UseMethod("estimateComparisonMeans")
}

estimateComparisonMeans.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
  Levels <- estimateSet(moments, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifference(moments, conf.level = conf.level, mu = 0, ...)
  output <- c(Levels, Diff)
  class(output) <- c("easi", "list")
  return(output)
}

estimateComparisonMeans.wsm <- function(moments, corrs, conf.level = .95, mu = 0, ...) {
  Levels <- estimateSet(moments, corrs, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifference(moments, corrs, conf.level = conf.level, mu = 0, ...)
  output <- c(Levels, Diff)
  class(output) <- c("easi", "list")
  return(output)
}
