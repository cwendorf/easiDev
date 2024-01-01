# Estimation Approach to Statistical Inference
## Formatting

### Frames

.formatFrame <- function(results, digits = 3, width = NULL, ...) {
  if (is.null(width)) width <- digits + 4
  format(as.data.frame(round(results, digits = digits)), width = width, trim = TRUE, nsmall = digits, ...)
}

.unformatFrame <- function(results) {
  apply(results, c(1, 2), FUN = as.numeric)
}

### Print

print.bsm <- print.wsm <- print.easi_main <- print.easi_comp <- function(x, ...) {
  cat("\n", comment(x), "\n\n", sep = "")
  m <- unclass(x)
  print(.formatFrame(m, ...))
  cat("\n", sep = "")
  invisible(x)
}
