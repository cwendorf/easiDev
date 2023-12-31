# Estimation Approach to Statistical Inference
## All Means

### Confidence Intervals

estimateSetMeans <- function(x, ...) {
  UseMethod("estimateSetMeans")
}

estimateSetMeans.wsm <- estimateSetMeans.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
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
  output <- list(results)
  names(output) <- "Confidence Intervals for the Means"
  class(output) <- c("easi", "list")
  return(output)
}
