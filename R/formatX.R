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


### Construction

construct <- function(..., class = "data") {
  if (class == "bsm" || class == "wsm") {
    out <- rbind(...)
    class(out) <- class
    comment(out) <- "Moments"
  } else if (class == "data") {
    out <- data.frame(...)
    comment(out) <- "Data"
  } else if (class == "cor") {
    out <- rbind(...)
    colnames(out) <- rownames(out)
    class(out) <- class
    comment(out) <- "Correlations"
  }
  return(out)
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
