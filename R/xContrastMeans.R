# Estimation Approach to Statistical Inference
## Contrast Means

### Confidence Intervals

estimateContrastMeans <- function(x, ...) {
  UseMethod("estimateContrastMeans")
}

estimateContrastMeans.wsm <- function(moments, corrs, contrast, conf.level = .95, ...) {
  N <- min(moments[, "N"])
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  df <- N - 1
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  covstats <- .cortocov(corrs, SD)
  Est <- (t(contrast) %*% M)
  SE <- sqrt(t(contrast) %*% covstats %*% contrast / N)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- as.data.frame(t(c(Est, SE, df, LL, UL)))
  colnames(results) <- c("Est", "SE", "df", "LL", "UL")
  rownames(results) <- c("Contrast")
  output <- list(results)
  names(output) <- "Confidence Interval for the Contrast of Means"
  class(output) <- c("easi", "list")
  return(output)
}

estimateContrastMeans.bsm <- function(moments, contrast, conf.level = .95, ...) {
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  Est <- t(contrast) %*% M
  v <- diag(SD^2) %*% (solve(diag(N)))
  SE <- sqrt(t(contrast) %*% v %*% contrast)
  df <- (SE^4) / sum(((contrast^4) * (SD^4) / (N^2 * (N - 1))))
  tcrit <- qt((1 - conf.level) / 2, df, lower.tail = FALSE)
  LL <- Est - tcrit * SE
  UL <- Est + tcrit * SE
  results <- as.data.frame(t(c(Est, SE, df, LL, UL)))
  colnames(results) <- c("Est", "SE", "df", "LL", "UL")
  rownames(results) <- c("Contrast")
  output <- list(results)
  names(output) <- "Confidence Interval for the Contrast of Means"
  class(output) <- c("easi", "list")
  return(output)
}
