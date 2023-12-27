# Estimation Approach to Statistical Inference
## Means

### Confidence Intervals

ciMeans <- function(x, ...) {
  UseMethod("ciMeans")
}

ciMeans.wss <- ciMeans.bss <- function(DescStats, mu = 0, conf.level = .95, ...) {
  N <- DescStats[, "N"]
  M <- DescStats[, "M"]
  SD <- DescStats[, "SD"]
  Est <- M - mu
  df <- N - 1
  SE <- SD / sqrt(N)
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- rownames(DescStats)
  comment(results) <- "Confidence Intervals for the Means"
  class(results) <- c("easi", "data.frame")
  return(results)
}
