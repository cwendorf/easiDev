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


plotMeans <- function(x, ..., contrast = NULL, connect = FALSE) {
  desc <- describeMoments(x)
  howmany <- nrow(desc)
  if (class(desc) == "wsm") connect <- TRUE
  if (!is.null(contrast)) {
    plot(estimateMeansSubsets(x, ..., contrast = contrast), connect = connect)
  } else if (howmany == 2) {
    plot(estimateMeansComparison(x, ...), connect = connect)
  } else {
    plot(estimateMeansSet(x, ...), connect = connect)
  }
  invisible(eval(x))
}

plotMeans <- function(x, ..., contrast = NULL) {
  howmany <- nrow(describeMoments(x))
  #print(class(describeMoments(x)))
  if (!is.null(contrast)) {
    plot(estimateMeansSubsets(x, ..., contrast = contrast), ...)
  } else if (howmany == 2) {
    plot(estimateMeansComparison(x, ...), ...)
  } else {
    plot(estimateMeansSet(x, ...), ...)
  }
  invisible(eval(x))
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
  results <- cbind(Est, SE, df, LL, UL)
  rownames(results) <- rownames(moments)
  comment(results) <- "Confidence Intervals for the Means"
  class(results) <- "intervalsMain"
  return(results)
}

estimateMeansSet.data.frame <- function(frame, conf.level = .95, mu = 0, ...) {
  moments <- describeMoments(frame)
  estimateMeansSet(moments, conf.level = conf.level, mu = mu, ...)
}

estimateMeansSet.formula <- function(formula, conf.level = .95, mu = 0, ...) {
  moments <- describeMoments(formula)
  estimateMeansSet(moments, conf.level = conf.level, mu = mu, ...)
}

### Means Difference

estimateMeansDifference <- function(x, ...) {
  UseMethod("estimateMeansDifference")
}

estimateMeansDifference.wsm <- function(moments, corrs, conf.level = .95, mu = 0,...) {
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
  results <- cbind(Est, SE, df, LL, UL)
  rownames(results) <- "Comparison"
  comment(results) <- "Confidence Interval for the Difference of Means"
  class(results) <- "intervalsMain"
  return(results)
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
  results <- cbind(Est, SE, df, LL, UL)
  rownames(results) <- "Comparison"
  comment(results) <- "Confidence Interval for the Difference of Means"
  class(results) <- "intervalsMain"
  return(results)
}

estimateMeansDifference.data.frame <- function(frame, conf.level = .95, mu = 0, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateMeansDifference(moments, corrs, conf.level = conf.level, mu = mu, labels = labels)
}

estimateMeansDifference.formula <- function(formula, conf.level = .95, mu = 0, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateMeansDifference(moments,conf.level = conf.level, mu = mu, labels = labels)
}

### Means Comparison

estimateMeansComparison <- function(x, ...) {
  UseMethod("estimateMeansComparison")
}

estimateMeansComparison.bsm <- function(moments, conf.level = .95, mu = 0, ...) {
  Levels <- estimateMeansSet(moments, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateMeansDifference(moments, conf.level = conf.level, mu = 0, ...)
  results <- rbind(Levels, Diff)
  comment(results) <- "Confidence Intervals for the Comparison of Means"
  class(results) <- "intervalsComp"
  return(results)
}

estimateMeansComparison.wsm <- function(moments, corrs, conf.level = .95, mu = 0, ...) {
  Levels <- estimateMeansSet(moments, corrs, conf.level = conf.level, mu = 0, ...)
  Diff <- estimateMeansDifference(moments, corrs, conf.level = conf.level, mu = 0, ...)
  results <- rbind(Levels, Diff)
  comment(results) <- "Confidence Intervals for the Comparison of Means"
  class(results) <- "intervalsComp"
  return(results)
}

estimateMeansComparison.data.frame <- function(frame, conf.level = .95, mu = 0, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateMeansComparison(moments, corrs, conf.level = conf.level, mu = 0, labels = labels, ...)
}

estimateMeansComparison.formula <- function(formula, conf.level = .95, mu = 0, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateMeansComparison(moments, conf.level = conf.level, mu = 0, labels = labels, ...)
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
  results <- cbind(t(c(Est, SE, df, LL, UL)))
  colnames(results) <- c("Est", "SE", "df", "LL", "UL")
  rownames(results) <- c("Contrast")
  comment(results) <- "Confidence Interval for the Contrast of Means"
  class(results) <- "intervalsMain"
  return(results)
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
  results <- cbind(t(c(Est, SE, df, LL, UL)))
  colnames(results) <- c("Est", "SE", "df", "LL", "UL")
  rownames(results) <- c("Contrast")
  comment(results) <- "Confidence Interval for the Contrast of Means"
  class(results) <- "intervalsMain"
  return(results)
}

estimateMeansContrast.data.frame <- function(frame, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateMeansContrast(moments, corrs, contrast, conf.level = conf.level, labels = labels, ...)
}

estimateMeansContrast.formula <- function(formula, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateMeansContrast(moments, contrast, conf.level = conf.level, labels = labels, ...)
}

### Means Subsets

estimateMeansSubsets <- function(x, ...) {
  UseMethod("estimateMeansSubsets")
}

estimateMeansSubsets.wsm <- function(moments, corrs, contrast, conf.level = .95, labels = NULL, ...) {
  con1 <- ifelse(contrast < 0, 0, contrast)
  res1 <- estimateMeansContrast(moments, corrs, contrast = con1, conf.level = conf.level)
  con2 <- ifelse(contrast > 0, 0, abs(contrast))
  res2 <- estimateMeansContrast(moments, corrs, contrast = con2, conf.level = conf.level)
  Subsets <- rbind(res2, res1)
  if (is.null(labels)) {
    rownames(Subsets) <- c("Neg Weighted", "Pos Weighted")
  } else {
    rownames(Subsets) <- labels
  }
  Diff <- estimateMeansContrast(moments, corrs, contrast = contrast, conf.level = conf.level)
  results <- rbind(Subsets, Diff)
  comment(results) <- "Confidence Intervals for the Subsets of Means"
  class(results) <- "intervalsComp"
  return(results)
}

estimateMeansSubsets.bsm <- function(moments, contrast, conf.level = .95, labels = NULL, ...) {
  con1 <- ifelse(contrast < 0, 0, contrast)
  res1 <- estimateMeansContrast(moments, contrast = con1, conf.level = conf.level)
  con2 <- ifelse(contrast > 0, 0, abs(contrast))
  res2 <- estimateMeansContrast(moments, contrast = con2, conf.level = conf.level)
  Subsets <- rbind(res2, res1)
  if (is.null(labels)) {
    rownames(Subsets) <- c("Neg Weighted", "Pos Weighted")
  } else {
    rownames(Subsets) <- labels
  }
  Diff <- estimateMeansContrast(moments, contrast = contrast, conf.level = conf.level)
  results <- rbind(Subsets, Diff)
  comment(results) <- "Confidence Intervals for the Subsets of Means"
  class(results) <- "intervalsComp"
  return(results)
}

estimateMeansSubsets.data.frame <- function(frame, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  estimateMeansSubsets(moments, corrs, contrast, conf.level = conf.level, labels = labels, ...)
}

estimateMeansSubsets.formula <- function(formula, contrast, conf.level = .95, labels = NULL, ...) {
  moments <- describeMoments(formula)
  estimateMeansSubsets(moments, contrast, conf.level = conf.level, labels = labels, ...)
}
