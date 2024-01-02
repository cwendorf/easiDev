# Estimation Approach to Statistical Inference
## Means Difference

### Estimate

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
  class(results) <- c("easi", "intervalsMain")
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
  class(results) <- c("easi", "intervalsMain")
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

### Plot

plotMeansDifference <- function(x, ...) {
  UseMethod("plotMeansDifference")
}

plotMeansDifference.bsm <- function(moments, add = FALSE, main = NULL, ylab = "Mean Difference", xlab = "", conf.level = .95, mu = 0, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateMeansDifference(moments, conf.level = conf.level, mu = mu, labels = labels)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotMeansDifference.wsm <- function(moments, corrs, add = FALSE, main = NULL, ylab = "Mean Difference", xlab = "", conf.level = .95, mu = 0, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateMeansDifference(moments, corrs, conf.level = conf.level, mu = mu, labels = labels)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotMeansDifference.formula <- function(formula, add = FALSE, main = NULL, ylab = "Mean Difference", xlab = "", conf.level = .95, mu = 0, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = FALSE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateMeansDifference(formula, conf.level = conf.level, mu = mu, labels = labels)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

plotMeansDifference.data.frame <- function(frame, add = FALSE, main = NULL, ylab = "Mean Difference", xlab = "", conf.level = .95, mu = 0, rope = NULL, labels = NULL, values = TRUE, pos = c(2, 2, 4), connect = TRUE, ylim = NULL, digits = 3, pch = 17, col = "black", offset = 0, intervals = TRUE) {
  results <- estimateMeansDifference(frame, conf.level = conf.level, mu = mu, labels = labels)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
}

addMeansDifference <- function(...) {
  plotMeansDifference(..., add = TRUE)
}

### Test

testMeansDifference <- function(x, ...) {
  UseMethod("testMeansDifference")
}

testMeansDifference.wsm <- function(moments, corrs, mu = 0, labels = NULL, ...) {
  moments <- moments[1:2, ]
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  rn <- rownames(moments)
  R <- corrs[rn[1], rn[2]]
  MD <- M[2] - M[1] - mu
  SE <- SD / sqrt(N)
  SE <- sqrt(SE[1]^2 + SE[2]^2 - 2 * R * SE[1] * SE[2])
  df <- min(N) - 1
  t <- MD / SE
  p <- 2 * (1 - pt(abs(t), df))
  results <- cbind(Diff = MD, SE = SE, df = df, t = t, p = p)
  if (is.null(labels)) {
    rownames(results) <- c("Comparison")
  } else {
    rownames(results) <- labels
  }
  comment(results) <- "Hypothesis Test for the Difference of Means"
  class(results) <- c("easi")
  return(results)
}

testMeansDifference.bsm <- function(moments, mu = 0, labels = NULL, ...) {
  moments <- moments[1:2, ]
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  MD <- M[2] - M[1] - mu
  SE <- sqrt((SD[1]^2 / N[1]) + (SD[2]^2 / N[2]))
  df <- ((SD[1]^2 / N[1] + SD[2]^2 / N[2])^2) / ((SD[1]^2 / N[1])^2 / (N[1] - 1) + (SD[2]^2 / N[2])^2 / (N[2] - 1))
  t <- MD / SE
  p <- 2 * (1 - pt(abs(t), df))
  results <- cbind(Diff = MD, SE = SE, df = df, t = t, p = p)
  if (is.null(labels)) {
    rownames(results) <- c("Comparison")
  } else {
    rownames(results) <- labels
  }
  comment(results) <- "Hypothesis Test for the Difference of Means"
  class(results) <- c("easi")
  return(results)
}

testMeansDifference.data.frame <- function(frame, mu = 0, labels = NULL, ...) {
  moments <- describeMoments(frame)
  corrs <- describeCorrelations(frame)
  testMeansDifference(moments, corrs, conf.level = conf.level, mu = mu, labels = labels)
}

testMeansDifference.formula <- function(formula, mu = 0, labels = NULL, ...) {
  moments <- describeMoments(formula)
  testMeansDifference(moments,conf.level = conf.level, mu = mu, labels = labels)
}
