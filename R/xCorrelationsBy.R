# Estimation Approach to Statistical Inference
## Correlations By

### Describe

describeCorrelationsBy <- function(x, ...) {
  UseMethod("describeCorrelationsBy")
}

describeCorrelationsBy.data.frame <- function(frame, by, ...) {
  MixedData <- data.frame(by, frame)
  SplitData <- split(MixedData[-1], by)
  results <- lapply(SplitData, describeCorrelations)
  results
}

describeCorrelationsBy.list <- function(list, ...) {
  results <- lapply(list, describeCorrelations, ...)
  return(results)
}
