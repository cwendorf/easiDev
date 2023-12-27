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

print.easi <- function (x, ..., digits = 3, quote = FALSE, right = TRUE, width = NULL, trim = TRUE) {
  if (is.null(width)) width <- digits + 4
  m <- as.matrix(format.data.frame(x, digits = digits, na.encode = FALSE, width = width, trim = trim, nsmall = digits))
  cat("\n", comment(x), "\n\n")
  print(m, ..., quote = quote, right = right)
  cat("\n")
  invisible(x)
}
