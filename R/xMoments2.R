# Estimation Approach to Statistical Inference
## Summary


describeMoments <- function(x, ...) {
  UseMethod("describeMoments")
}

describeMoments.bsm <- describeMoments.wsm <- function(frame, ...) {
  return(frame)
}

describeMoments.data.frame <- function(frame, ...) {
  results <- .summarize(frame)
  class(results) <- "wsm"
  return(results)
}

describeMoments.formula <- function(formula, ...) {
  results <- aggregate(formula, FUN = .summarize)
  rn <- results[, 1]
  results <- results[[2]]
  rownames(results) <- rn
  colnames(results) <- c("N", "M", "SD", "Skew", "Kurt")
  class(results) <- "bsm"
  comment(results) <- "Statistics for the Data"
  return(results)
}
