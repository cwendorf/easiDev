# Estimation Approach to Statistical Inference
## Means Set

### Estimate

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
  class(results) <- c("easi", "intervalsMain")
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

### Plot

plotMeansSet <- function(x, ...) {
  UseMethod("plotMeansSet")
}

plotMeansSet.wsm <- plotMeansSet.data.frame <- function(..., conf.level = .95, mu = 0, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, line = NULL, rope = NULL, values = TRUE, digits = 3, pos = 2, pch = 16, col = "black", connect = TRUE, offset = 0, intervals = TRUE) {
  results <- estimateMeansSet(..., mu = mu, conf.level = conf.level, main = main, digits = digits)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, line = line, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
  invisible(eval(...))
}

plotMeansSet.bsm <- plotMeansSet.formula <- function(..., conf.level = .95, mu = 0, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, line = NULL, rope = NULL, values = TRUE, digits = 3, pos = 2, pch = 16, col = "black", connect = FALSE, offset = 0, intervals = TRUE) {
  results <- estimateMeansSet(..., mu = mu, conf.level = conf.level, main = main, digits = digits)
  plot(results, add = add, main = main, xlab = xlab, ylab = ylab, ylim = ylim, values = values, line = line, rope = rope, digits = digits, connect = connect, pos = pos, pch = pch, col = col, offset = offset, intervals = intervals)
  invisible(eval(...))
}

addMeansSet <- function(...) {
  plotMeansSet(..., add = TRUE)
}

### Test

testMeansSet <- function(x, ...) {
  UseMethod("testMeansSet")
}

testMeansSet.wsm <- testMeansSet.bsm <- function(moments, mu = 0, ...) {
  N <- moments[, "N"]
  M <- moments[, "M"]
  SE <- moments[, "SD"] / sqrt(N)
  Diff <- M - mu
  t <- Diff / SE
  df <- N - 1
  p <- 2 * (1 - pt(abs(t), df))
  results <- cbind(Diff = Diff, SE = SE, df = df, t = t, p = p)
  rownames(results) <- rownames(moments)
  comment(results) <- "Hypothesis Tests for the Means"
  class(results) <- c("easi")
  return(results)
}

testMeansSet.data.frame <- function(frame, mu = 0, ...) {
  moments <- describeMoments(frame)
  testMeansSet(moments, mu = mu, ...)
}

testMeansSet.formula <- function(formula, mu = 0, ...) {
  moments <- describeMoments(formula)
  testMeansSet(moments, mu = mu, ...)
}

### Standardize

standardizeMeansSet <- function(x, ...) {
  UseMethod("standardizeMeansSet")
}

standardizeMeansSet.wsm <- standardizeMeansSet.bsm <- function(moments, mu = 0, conf.level = .95, ...) {
  N <- moments[, "N"]
  M <- moments[, "M"]
  SD <- moments[, "SD"]
  SE <- SD / sqrt(N)
  Diff <- M - mu
  t <- Diff / SE
  df <- N - 1
  alpha <- 1 - conf.level
  CD <- Diff / SD
  CDU <- (1 - 3 / (4 * df - 1)) * CD
  SE <- sqrt((df + 2) / (N^2) + ((CD^2) / (2 * (df + 2))))
  k <- exp(((log(2) - log(df)) / 2) + lgamma((df + 1) / 2) - lgamma(df / 2))
  m <- t * k
  v <- 1 + (t^2) * (1 - k^2)
  w <- (2 * m^3) - ((2 * df - 1) / df) * (t^2 * m)
  skew <- abs(w / sqrt(v)^3)
  sdz <- sqrt(v)
  llz <- qnorm(1 - alpha / 2, lower.tail = FALSE)
  ulz <- qnorm(1 - alpha / 2)
  ll1 <- m + sdz * llz
  ul1 <- m + sdz * ulz
  c <- w / (4 * v)
  q <- v / (2 * c^2)
  a <- m - (q * c)
  llp <- 2 * (qgamma(alpha / 2, q / 2, rate = 1))
  ulp <- 2 * (qgamma(1 - alpha / 2, q / 2, rate = 1))
  ll2 <- ifelse(t > 0, a + c * llp, a + c * ulp)
  ul2 <- ifelse(t > 0, a + c * ulp, a + c * llp)
  LL <- ifelse(skew < .001, ll1 * sqrt(1 / N), ll2 * sqrt(1 / N))
  UL <- ifelse(skew < .001, ul1 * sqrt(1 / N), ul2 * sqrt(1 / N))
  results <- cbind(d = CD, SE = SE, LL = LL, UL = UL)
  rownames(results) <- rownames(moments)
  comment(results) <- "Confidence Intervals for the Standardized Means"
  class(results) <- c("easi", "intervalsMain")
  return(results)
}

standardizeMeansSet.data.frame <- function(frame, mu = 0, conf.level = .95, ...) {
  moments <- describeMoments(data)
  standardizeMeansSet(moments, mu = mu, conf.level = conf.level)
}

standardizeMeansSet.formula <- function(formula, mu = 0, conf.level = .95, ...) {
  moments <- describeMoments(formula)
  standardizeMeansSet(moments, mu = mu, conf.level = conf.level)
}
