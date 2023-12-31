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

### Lists

.formatList <- function(results, main = NULL, digits = 3, width = NULL, ...) {
  results <- lapply(results, .formatFrame, digits, width, ...)
  if (!is.null(main)) names(results) <- main
  return(results)
}

.unformatList <- function(results) {
  lapply(results, .unformatFrame)
}

.deList <- function(results) {
  output <- results[[1]]
  colnames(output)[1] <- "Est"
  if (length(results) > 1) {
    for (i in 2:length(results)) {
      colnames(results[[i]])[1] <- "Est"
      output <- rbind(output, results[[i]])
    }
  }
  return(output)
}

.collapseList <- function(results, main = NULL) {
  if (is.null(main)) {
    main <- names(results)[2]
  } else {
    main <- main
  }
  output <- list(.deList(results))
  names(output) <- main
  return(output)
}

### Print

print.bsm <- print.wsm <- function(x, main = "Statistics for the Data", ...) {
  m <- list(unclass(x))
  names(m) <- main
  print(.formatList(m, ...))
  invisible(x)
}

print.cor <- function(x, main = "Correlations for the Data", ...) {
  m <- list(unclass(x))
  names(m) <- main
  print(.formatList(m, ...))
  invisible(x)
}

print.easi <- function(x, main = NULL, ...) {
  print(.formatList(x, main = main, ...))
}

view <- function(x) {
  print.default(x)
}

### Construction

construct <- function(..., class = "data") {
  if (class == "bsm" || class == "wsm") {
    output <- rbind(...)
    class(output) <- class
    comment(output) <- "Moments"
  } else if (class == "data") {
    output <- data.frame(...)
    comment(output) <- "Data"
  } else if (class == "cor") {
    output <- rbind(...)
    colnames(output) <- rownames(output)
    class(output) <- class
    comment(output) <- "Correlations"
  }
  return(output)
}

### Focusing

focus <- function(x, ...) {
  UseMethod("focus")
}

focus.bsm <- focus.wsm <- function(DescStats, ...) {
  chosen <- as.character(match.call(expand.dots = FALSE)$...)
  if (typeof(DescStats) == "list") {
    for (i in seq_along(DescStats)) {
      newclass <- class(DescStats[[i]])
      DescStats[[i]] <- DescStats[[i]][chosen, ]
      class(DescStats[[i]]) <- newclass
    }
    return(DescStats)
  }
  else {
    results <- DescStats[chosen, ]
    class(results) <- class(DescStats)
    return(results)
  }
}

focus.data.frame <- function(frame, ...) {
  filts <- (match.call(expand.dots = FALSE)$...)
  chosen <- NULL
  for (i in seq_along(filts)) {
    type <- typeof(do.call(with, list(frame, filts[[i]])))
    if (type == "logical") {frame <- subset(frame, eval(filts[[i]]))}
    else if (type == "language") {return(do.call(with, list(frame, filts[[i]])))}
    else {
      chose <- as.character(filts[[i]])
      chosen <- c(chosen, chose)
    }
  }
  if (is.null(chosen)) {
    return(frame)
  } else {
    return(subset(frame, select = chosen))
  }
}

focus.formula <- function(formula, ...) {
  chosen <- as.character(match.call(expand.dots = FALSE)$...)
  update <- paste("~ factor(.,", paste(deparse(chosen), collapse = ","), ")")
  update(formula, update)
}

focus.cor <- function(CorrStats, ...) {
  chosen <- as.character(match.call(expand.dots = FALSE)$...)
  if (typeof(CorrStats) == "list") {
    for (i in seq_along(CorrStats)) {
      newclass <- class(CorrStats[[i]])
      CorrStats[[i]] <- CorrStats[[i]][chosen, chosen]
      class(CorrStats[[i]]) <- newclass
    }
    return(CorrStats)
  }
  else {
    results <- CorrStats[chosen, chosen]
    class(results) <- class(CorrStats)
    return(results)
  }
}
