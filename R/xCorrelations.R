# Estimation Approach to Statistical Inference
## Correlations

### Correlations

describeCorrelations <- function(x, ...) {
  UseMethod("describeCorrelations")
}

describeCorrelations.data.frame <- function(frame, ...) {
  output <- cor(frame)
  class(output) <- "cor"
  return(output)
}

describeCorrelations.cor <- function(cor, ...) {
  return(cor)
}

describeCorrelations.wsm <- function(wsm, cor, ...) {
  return(cor)
}

describeCorrelations.list <- function(list, ...) {
  results <- lapply(list, describeCorrelations.cor)
  return(results)
}

### Covariances

.cortocov <- function(corrs, SD) {
  sdsquare <- SD %*% t(SD)
  covstats <- sdsquare * corrs
  return(covstats)
}
