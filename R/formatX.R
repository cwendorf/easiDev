# Estimation Approach to Statistical Inference
## Formatting

### Confidence Intervals

print.easi <- function (x, ..., digits = 3, quote = FALSE, right = TRUE, width = NULL, trim = TRUE) {
  if (is.null(width)) width <- digits + 4
  m <- as.matrix(format.data.frame(x, digits = digits, na.encode = FALSE, width = width, trim = trim, nsmall = digits))
  cat("\n", comment(x), "\n\n")
  print(m, ..., quote = quote, right = right)
  cat("\n")
  invisible(x)
}
