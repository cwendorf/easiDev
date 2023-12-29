# Estimation Approach to Statistical Inference
## Means Difference

### Confidence Intervals

estimateDifference <- function(x, ...) {
  UseMethod("estimateDifference")
}

estimateDifference.wsm <- function(CompStats, CorrStats, mu = 0, conf.level = .95, ...) {
  CompStats <- CompStats[1:2,]
  N <- CompStats[, "N"]
  M <- CompStats[, "M"]
  SD <- CompStats[, "SD"]
  rn <- rownames(CompStats)
  R <- CorrStats[rn[1], rn[2]]
  Est <- M[2] - M[1] - mu
  SE <- SD / sqrt(N)
  SE <- sqrt(SE[1]^2 + SE[2]^2 - 2 * R * SE[1] * SE[2])
  df <- min(N) - 1
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- "Comparison"
  out <- list(results)
  names(out) <- "Confidence Intervals for the Difference of Means"
  class(out) <- c("easi", "list")
  return(out)
}

estimateDifference.bsm <- function(CompStats, mu = 0, conf.level = .95, ...) {
  CompStats <- CompStats[1:2,]
  N <- CompStats[, "N"]
  M <- CompStats[, "M"]
  SD <- CompStats[, "SD"]
  Est <- M[2] - M[1] - mu
  SE <- sqrt((SD[1]^2 / N[1]) + (SD[2]^2 / N[2]))
  df <- ((SD[1]^2 / N[1] + SD[2]^2 / N[2])^2) / ((SD[1]^2 / N[1])^2 / (N[1] - 1) + (SD[2]^2 / N[2])^2 / (N[2] - 1))
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- "Comparison"
  out <- list(results)
  names(out) <- "Confidence Intervals for the Difference of Means"
  class(out) <- c("easi", "list")
  return(out)
}
