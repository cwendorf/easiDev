# Estimation Approach to Statistical Inference
## Formatting

### Frames

.formatFrame <- function(results, digits = 3, width = NULL) {
  if (is.null(width)) width <- digits + 4
  format(as.data.frame(round(results, digits = digits)), width = width, trim = TRUE, nsmall = digits)
}

.unformatFrame <- function(results) {
  apply(results, c(1, 2), FUN = as.numeric)
}

### Lists

.formatList <- function(results, main = NULL, digits = 3, width = NULL) {
  results <- lapply(results, .formatFrame, digits, width)
  if (!is.null(main)) names(results) <- main
  results
}

.unformatList <- function(results) {
  lapply(results, .unformatFrame)
}

.deList <- function(results) {
  out <- results[[1]]
  colnames(out)[1] <- "Est"
  if (length(results) > 1) {
    for (i in 2:length(results)) {
      colnames(results[[i]])[1] <- "Est"
      out <- rbind(out, results[[i]])
    }
  }
  return(out)
}

.collapseList <- function(results, main = NULL) {
  if (is.null(main)) {
    main <- names(results)[2]
  } else {
    main <- main
  }
  out <- list(.deList(results))
  names(out) <- main
  return(out)
}

### Print

print.bsm <- print.wsm <- function(x, ...) {
  m <- list(unclass(x))
  names(m) <- "Summary Statistics for the Data"
  print(.formatList(m, ...))
  invisible(x)
}

print.cor <- function(x, ...) {
  m <- list(unclass(x))
  names(m) <- "Correlations for the Data"
  print(.formatList(m, ...))
  invisible(x)
}

print.easi <- function(x, ...) {
  print(.formatList(x, ...))
}
