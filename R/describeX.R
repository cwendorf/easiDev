# Estimation Approach to Statistical Inference
## Describe

### Moments

.skewness <- function(x, na.rm = FALSE, type = 2) {
  if (any(ina <- is.na(x))) {
    if (na.rm) {
      x <- x[!ina]
    } else {
      return(NA)
    }
  }
  if (!(type %in% (1:3))) stop("Invalid 'type' argument.")
  n <- length(x)
  x <- x - mean(x)
  y <- sqrt(n) * sum(x^3) / (sum(x^2)^(3 / 2))
  if (type == 2) {
    if (n < 3) stop("Need at least 3 complete observations.")
    y <- y * sqrt(n * (n - 1)) / (n - 2)
  } else if (type == 3) y <- y * ((1 - 1 / n))^(3 / 2)
  y
}

.kurtosis <- function(x, na.rm = FALSE, type = 2) {
  if (any(ina <- is.na(x))) {
    if (na.rm) {
      x <- x[!ina]
    } else {
      return(NA)
    }
  }
  if (!(type %in% (1:3))) stop("Invalid 'type' argument.")
  n <- length(x)
  x <- x - mean(x)
  r <- n * sum(x^4) / (sum(x^2)^2)
  y <- if (type == 1) {
    r - 3
  } else if (type == 2) {
    if (n < 4) stop("Need at least 4 complete observations.")
    ((n + 1) * (r - 3) + 6) * (n - 1) / ((n - 2) * (n - 3))
  } else {
    r * (1 - 1 / n)^2 - 3
  }
  y
}

.summarize <- function(frame) {
  if (typeof(frame) == "double") {
    data <- data.frame(frame)
    if (ncol(data) == 1) {
      colnames(data) <- deparse(substitute(frame))
    }
  } else {
    data <- frame
  }
  N <- sapply(data, length)
  M <- sapply(data, mean, na.rm = TRUE)
  SD <- sapply(data, sd, na.rm = TRUE)
  Skew <- sapply(data, .skewness, na.rm = TRUE)
  Kurt <- sapply(data, .kurtosis, na.rm = TRUE)
  results <- cbind(N = N, M = M, SD = SD, Skew = Skew, Kurt = Kurt)
  return(results)
}

describeMoments <- function(x, ...) {
  UseMethod("describeMoments")
}

describeMoments.bsm <- describeMoments.wsm <- function(frame, ...) {
  return(frame)
}

describeMoments.data.frame <- function(frame, ...) {
  results <- .summarize(frame)
  class(results) <- c("easi", "wsm")
  comment(results) <- "Statistics for the Data"
  return(results)
}

describeMoments.formula <- function(formula, ...) {
  results <- aggregate(formula, FUN = .summarize)
  rn <- results[, 1]
  results <- results[[2]]
  rownames(results) <- rn
  colnames(results) <- c("N", "M", "SD", "Skew", "Kurt")
  class(results) <- c("easi", "bsm")
  comment(results) <- "Statistics for the Data"
  return(results)
}

### Correlations

.cortocov <- function(corrs, SD) {
  sdsquare <- SD %*% t(SD)
  covstats <- sdsquare * corrs
  return(covstats)
}

describeCorrelations <- function(x, ...) {
  UseMethod("describeCorrelations")
}

describeCorrelations.data.frame <- function(frame, ...) {
  results <- cor(frame)
  class(results) <- c("easi", "cor")
  comment(results) <- "Correlations for the Data"
  return(results)
}

describeCorrelations.cor <- function(cor, ...) {
  return(cor)
}

describeCorrelations.wsm <- function(wsm, cor, ...) {
  return(cor)
}

### Percentiles

describePercentiles <- function(x, ...) {
  UseMethod("describePercentiles")
}

describePercentiles.data.frame <- function(frame, ...) {
  data <- data.frame(frame)
  if (ncol(data) == 1) {
    colnames(data) <- deparse(substitute(frame))
  }
  results <- t(sapply(data, quantile))
  colnames(results) <- c("Min", "25%", "50%", "75%", "Max")
  class(results) <- "easi"
  comment(results) <- "Percentiles for the Data"
  return(results)
}

describePercentiles.formula <- function(formula, ...) {
  results <- aggregate(formula, FUN = describePercentiles.data.frame)
  rn <- results[, 1]
  results <- results[[2]]
  rownames(results) <- rn
  colnames(results) <- c("Min", "25%", "50%", "75%", "Max")
  class(results) <- "easi"
  comment(results) <- "Percentiles for the data"
  return(results)
}
