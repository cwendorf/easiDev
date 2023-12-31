# Estimation Approach to Statistical Inference
## Means

### Method Selector

estimateMeans <- function(x, ..., contrast = NULL) {
  howmany <- nrow(describeMoments(x))
  if (!is.null(contrast)) {
    estimateMeansSubsets(x, ..., contrast = contrast)
  } else if (howmany == 2) {
    estimateMeansComparison(x, ...)
  } else {
    estimateMeansSet(x, ...)
  }
}

### Means Set

estimateMeansSet <- function(x, ...) {
  UseMethod("estimateMeansSet")
}

estimateMeansSet.wsm <- estimateMeansSet.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
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


### Means Difference

estimateMeansDifference <- function(x, ...) {
  UseMethod("estimateMeansDifference")
}

estimateMeansDifference.wsm <- function(moments, corrs, conf.level = .95, mu = 0, main = NULL, ...) {
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
  if (is.null(main)) main <- "Confidence Interval for the Difference of Means"
  names(output) <- main
  class(output) <- c("easi", "list")
  return(output)
}

estimateMeansDifference.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
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
  names(output) <- "Confidence Interval for the Difference of Means"
  class(output) <- c("easi", "list")
  return(output)
}


### Means Comparison

estimateMeansComparison <- function(x, ...) {
  UseMethod("estimateMeansComparison")
}

estimateMeansComparison.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
  Levels <- estimateSetMeans(moments, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifferenceMeans(moments, conf.level = conf.level, mu = 0, ...)
  output <- c(Levels, Diff)
  class(output) <- c("easi", "list")
  return(output)
}

estimateMeansComparison.wsm <- function(moments, corrs, conf.level = .95, mu = 0, ...) {
  Levels <- estimateSetMeans(moments, corrs, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateDifferenceMeans(moments, corrs, conf.level = conf.level, mu = 0, ...)
  output <- c(Levels, Diff)
  class(output) <- c("easi", "list")
  return(output)
}


### Means Contrast

estimateMeansContrast <- function(x, ...) {
  UseMethod("estimateMeansContrast")
}

estimateMeansContrast.wsm <- function(moments, corrs, contrast, conf.level = .95, ...) {
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

estimateMeansContrast.bsm <- function(moments, contrast, conf.level = .95, ...) {
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


### Means Subsets

estimateMeansSubsets <- function(x, ...) {
  UseMethod("estimateMeansSubsets")
}

estimateMeansSubsets.wsm <- function(moments, corrs, contrast, conf.level = .95, labels = NULL, ...) {
  con1 <- ifelse(contrast < 0, 0, contrast)
  res1 <- estimateMeansContrast(moments, corrs, contrast = con1, conf.level = conf.level)[[1]]
  con2 <- ifelse(contrast > 0, 0, abs(contrast))
  res2 <- estimateMeansContrast(moments, corrs, contrast = con2, conf.level = conf.level)[[1]]
  Subsets <- rbind(res2, res1)
  if (is.null(labels)) {
    rownames(Subsets) <- c("Neg Weighted", "Pos Weighted")
  } else {
    rownames(Subsets) <- labels
  }
  Diff <- estimateMeansContrast(moments, corrs, contrast = contrast, conf.level = conf.level)[[1]]
  output <- list(Subsets, Diff)
  names(output) <- c("Confidence Intervals for the Subsets of Means", "Confidence Interval for the Contrast of Means")
  class(output) <- c("easi", "list")
  return(output)
}

estimateMeansSubsets.bsm <- function(moments, contrast, conf.level = .95, labels = NULL, ...) {
  con1 <- ifelse(contrast < 0, 0, contrast)
  res1 <- estimateMeansContrast(moments, contrast = con1, conf.level = conf.level)[[1]]
  con2 <- ifelse(contrast > 0, 0, abs(contrast))
  res2 <- estimateMeansContrast(moments, contrast = con2, conf.level = conf.level)[[1]]
  Subsets <- rbind(res2, res1)
  if (is.null(labels)) {
    rownames(Subsets) <- c("Neg Weighted", "Pos Weighted")
  } else {
    rownames(Subsets) <- labels
  }
  Diff <- estimateMeansContrast(moments, contrast = contrast, conf.level = conf.level)[[1]]
  output <- list(Subsets, Diff)
  names(output) <- c("Confidence Intervals for the Subsets of Means", "Confidence Interval for the Contrast of Means")
  class(output) <- c("easi", "list")
  return(output)
}
