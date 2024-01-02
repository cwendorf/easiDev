# Estimation Approach to Statistical Inference
## Correlations

### Corrrelations

estimateCorrelations <- function(x, ...) {
  UseMethod("estimateCorrelations")
}

estimateCorrelations.wsm <- function(moments, corrs, conf.level = .95, ...) {
  N <- moments[, "N"]
  rn <- rownames(moments)
  nr <- nrow(moments)
  ncomp <- (nr) * (nr - 1) / 2
  results <- as.data.frame(matrix(ncol = 4, nrow = ncomp))
  colnames(results) <- c("R", "SE", "LL", "UL")
  comp <- 1
  for (i in 1:(nr - 1)) {
    for (j in (i + 1):nr) {
      rownames(results)[comp] <- paste(rn[i], "&", rn[j])
      n <- min(N[rn[j]], N[rn[i]])
      R <- corrs[rn[i], rn[j]]
      z <- qnorm((1 + conf.level) / 2)
      SE <- sqrt(1 / ((n - 3)))
      zR <- log((1 + R) / (1 - R)) / 2
      LL0 <- zR - z * SE
      UL0 <- zR + z * SE
      LL <- (exp(2 * LL0) - 1) / (exp(2 * LL0) + 1)
      UL <- (exp(2 * UL0) - 1) / (exp(2 * UL0) + 1)
      results[comp, ] <- c(R, SE, LL, UL)
      comp <- comp + 1
    }
  }
  results <- as.matrix(results)
  comment(results) <- "Confidence Intervals for the Correlations"
  class(results) <- c("easi", "cor")
  return(results)
}

estimateCorrelations.data.frame <- function(frame, conf.level = .95, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateCorrelations(moments, corrs, conf.level = conf.level)
}
