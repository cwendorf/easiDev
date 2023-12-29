# Estimation Approach to Statistical Inference
## Difference Means

### Confidence Intervals

estimateDifference <- function(x, ...) {
  UseMethod("estimateDifferenceMeans")
}

estimateDifferenceMeans <- function(x, ...) {
  UseMethod("estimateDifferenceMeans")
}

estimateDifferenceMeans.wsm <- function(moments, corrs, mu = 0, conf.level = .95, ...) {
  moments <- moments[1:2,]
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  rn <- rownames(moments)
  R <- corrs[rn[1], rn[2]]
  Est <- M[2] - M[1] - mu
  SE <- SD / sqrt(N)
  SE <- sqrt(SE[1]^2 + SE[2]^2 - 2 * R * SE[1] * SE[2])
  df <- min(N) - 1
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- "Comparison"
  output <- list(results)
  names(output) <- "Confidence Intervals for the Difference of Means"
  class(output) <- c("easi", "list")
  return(output)
}

estimateDifferenceMeans.bsm <- function(moments, mu = 0, conf.level = .95, ...) {
  moments <- moments[1:2,]
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  Est <- M[2] - M[1] - mu
  SE <- sqrt((SD[1]^2 / N[1]) + (SD[2]^2 / N[2]))
  df <- ((SD[1]^2 / N[1] + SD[2]^2 / N[2])^2) / ((SD[1]^2 / N[1])^2 / (N[1] - 1) + (SD[2]^2 / N[2])^2 / (N[2] - 1))
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- data.frame(Est, SE, df, LL, UL)
  rownames(results) <- "Comparison"
  output <- list(results)
  names(output) <- "Confidence Intervals for the Difference of Means"
  class(output) <- c("easi", "list")
  return(output)
}

estimateDifferenceMeans.data.frame <- function(frame, mu = 0, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateDifferenceMeans(moments, corrs, conf.level = conf.level, labels = labels)
}

estimateDifferenceMeans.formula <- function(formula, mu = 0, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateDifferenceMeans(moments, conf.level = conf.level, labels = labels)
}
