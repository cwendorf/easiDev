# Estimation Approach to Statistical Inference
## Moments By

#### Describe

describeMomentsBy <- function(x, ...) {
  UseMethod("describeMomentsBy")
}

describeMomentsBy.data.frame <- function(frame, by, ...) {
  MixedData <- data.frame(by, frame)
  SplitData <- split(MixedData[-1], by)
  results <- lapply(SplitData, describeMoments)
  results
}

describeMomentsBy.formula <- function(formula, by, ...) {
  Group <- eval(formula[[3]])
  Outcome <- eval(formula[[2]])
  FactorialData <- data.frame(by, Group, Outcome)
  SplitData <- split(FactorialData, by)
  results <- lapply(SplitData, function(x) with(x, describeMoments(Outcome ~ Group)))
  results
}

describeMomentsBy.list <- function(list, ...) {
  results <- lapply(list, describeMoments, ...)
  return(results)
}
