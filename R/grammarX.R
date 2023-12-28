# Estimation Approach to Statistical Inference
## Grammar

### Construction

construct <- function(..., class = "data") {
  if (class == "bss" || class == "wss") {
    out <- rbind(...)
    class(out) <- class
    comment(out) <- "Summary Statistics"
  } else if (class == "data") {
    out <- data.frame(...)
    comment(out) <- "Data"
  } else if (class == "corr") {
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

focus.bss <- focus.wss <- function(DescStats, ...) {
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

focus.corr <- function(CorrStats, ...) {
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
