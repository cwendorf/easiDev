# Estimation Approach to Statistical Inference
## Formatting

### Tables

print.easi <- function (x, digits = 3, quote = FALSE, right = TRUE, width = NULL, trim = TRUE) {
  if (is.null(width)) width <- digits + 4
  m <- format(as.matrix(x),digits = digits, na.encode = FALSE, width = width, trim = trim, nsmall = digits)
  print(m, quote = quote, right = right)
}

print.bss <- print.wss <- function(x, ..., digits = 3, quote = FALSE, right = TRUE, width = NULL, trim = TRUE) {
  m <- unclass(x)
  if (is.null(width)) width <- digits + 4
  m <- format(as.matrix(m), digits = digits, na.encode = FALSE, width = width, trim = trim, nsmall = digits)
  m <- list(m)
  names(m) <- "Summary Statistics"
  print(m, ..., quote = quote, right = right)
  invisible(x)
}

### Frames

.formatFrame <- function(results, digits = 3, width = NULL) {
  if (is.null(width)) width <- digits + 4
  format(as.data.frame(round(results, digits = digits)), width = width, trim = TRUE, nsmall = digits)
}

.unformatFrame <- function(results) {
  apply(results, c(1, 2), FUN = as.numeric)
}

### Lists

.formatList <- function(results, main = NULL, digits = 3) {
  results <- lapply(results, .formatFrame, digits)
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
