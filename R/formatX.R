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

print.easi <- function(x, ...) {
  cat("\n", comment(x), "\n\n", sep = "")
  m <- unclass(x)
  print(.formatFrame(m, ...))
  cat("\n", sep = "")
  invisible(x)
}

### Split Data

splitData <- function(x, ...) {
  UseMethod("splitData")
}

splitData.data.frame <- function(frame, by, ...) {
  dropped <- deparse(substitute(by))
  results <- split(subset(frame, select = -match(dropped, names(frame))), by)
  return(results)
}

splitData.formula <- function(formula, by, ...) {
  FactorialData <- data.frame(by, eval(formula[[3]]), eval(formula[[2]]))
  temp <- all.vars(formula)
  names(FactorialData) <- c("by", temp[2], temp[1])
  SplitData <- split(FactorialData[-1], by)
  return(SplitData)
}

### Construction

construct <- function(..., class = "data") {
  if (class == "bsm" || class == "wsm") {
    results <- rbind(...)
    class(results) <- c(class, "easi")
    comment(results) <- "Statistics for the Data"
  } else if (class == "data") {
    results <- data.frame(...)
    comment(results) <- "Data"
  } else if (class == "cor") {
    results <- rbind(...)
    colnames(results) <- rownames(results)
    class(results) <- c(class, "easi")
    comment(results) <- "Correlations for the Data"
  }
  return(results)
}

combine <- function(..., class = NULL) {
  results <- list(...)
  class(results) <- c(class, "list", "easi")
  return(results)
}

### Focusing

focus <- function(x, ...) {
  UseMethod("focus")
}

focus.default <- function(...) {
  data.frame(...)
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
  chosen <- (match.call(expand.dots = FALSE)$...)
  if (is.null(chosen)) update <- formula
  else {
    chosen <- as.character(chosen)
    update <- paste("~ factor(.,", paste(deparse(chosen), collapse = ","), ")")}
  update(formula, update)
}

focus.bsm <- focus.wsm <- function(moments, ...) {
  chosen <- as.character(match.call(expand.dots = FALSE)$...)
  results <- moments[chosen, , drop = FALSE]
  class(results) <- class(moments)
  return(results)
}

focus.cor <- function(corrs, ...) {
  chosen <- as.character(match.call(expand.dots = FALSE)$...)
  results <- corrs[chosen, chosen]
  class(results) <- class(corrs)
  return(results)
}

focus.bsl <- focus.wsl <- focus.list <- function(list, ...) {
  chosen <- as.character(match.call(expand.dots = FALSE)$...)
  for (i in seq_along(list)) {
    newclass <- class(list[[i]])
    newcomment <- comment(list[[i]])
    if (class(list[[i]][1]) == "cor") (list[[i]] <- list[[i]][chosen, chosen])
    else (list[[i]] <- list[[i]][chosen, ])
    class(list[[i]]) <- newclass
    comment(list[[i]]) <- newcomment
    }
  return(list)
}
