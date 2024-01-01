# Estimation Approach to Statistical Inference
## Plot

### Color Transformations

.colorTransparent <- function(someColor, alpha = 100) {
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(x) {
    rgb(red = x[1], green = x[2], blue = x[3], alpha = alpha, maxColorValue = 255)
  })
}

.colorIntensity <- function(someColor, increase = .0) {
  newColor <- rgb2hsv(col2rgb(someColor))
  newColor[3, ] <- newColor[3, ] + increase
  newColor[newColor > 1] <- 1
  apply(newColor, 2, function(x) {
    hsv(h = x[1], s = x[2], v = x[3])
  })
}

### Initialize Plots

.plotMain <- function(results, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, ...) {
  if (is.null(main)) {
    main <- names(results)
  }
  main <- paste(strwrap(main, width = 0.7 * getOption("width")), collapse = "\n")
  results <- .unformatFrame(results[[1]])
  if (is.null(ylim)) {
    ylim <- range(pretty(c(floor(min(results) - .5), ceiling(max(results) + .5))))
  }
  par(mar = c(5, 5, 5, 3))
  plot(NULL, xaxs = "i", yaxs = "i", xaxt = "n", xlim = c(.4, nrow(results) + .6), ylim = ylim, xlab = xlab, cex.lab = 1.15, ylab = ylab, main = main, las = 1, bty = "l")
  axis(1, seq_len(nrow(results)), row.names(results))
}

.plotComp <- function(results, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, slab = "Difference", ...) {
  if (is.null(main)) main <- names(results[1])
  results <- .unformatFrame(results[[1]])
  main <- paste(strwrap(main, width = 0.7 * getOption("width")), collapse = "\n")
  graph <- results
  graph[3, ] <- results[3, ] + results[1, 1]
  if (is.null(ylim)) {
    ylim <- range(pretty(c(floor(min(graph[, 2]) - .5), ceiling(max(graph[, 3]) + .5))))
  }
  par(mar = c(5, 5, 5, 5))
  plot(NULL, xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlim = c(.4, 3.6), ylim = ylim, xlab = xlab, ylab = ylab, main = main, las = 1, cex.lab = 1.15, bty = "n")
  axis(1, .4:2.5, labels = FALSE, lwd.tick = 0)
  axis(1, 2.6:3.6, labels = FALSE, lwd.tick = 0)
  axis(1, at = c(1, 2), labels = rownames(graph)[1:2])
  axis(1, at = 3, labels = rownames(graph)[3])
  axis(2)
  axis(2, at = ylim, labels = FALSE, lwd.tick = 0)
  if (results[1, 1] < results[2, 1]) {
    td <- graph[1, 1] - axTicks(4)[max(which(axTicks(4) < graph[1, 1]))]
  }
  if (results[1, 1] >= results[2, 1]) {
    td <- graph[1, 1] - axTicks(4)[min(which(axTicks(4) > graph[1, 1]))]
  }
  val <- axTicks(4) - graph[1, 1] + td
  loc <- axTicks(4) + td
  axis(4, at = ylim, labels = FALSE, lwd.tick = 0)
  axis(4, at = loc, labels = val, las = 1)
  mtext(slab, side = 4, las = 3, cex = 1.15, line = 3)
}

### Interval Plots

.intervalsMain <- function(results, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, line = NULL, rope = NULL, values = TRUE, digits = 3, connect = FALSE, pos = 2, pch = 16, col = "black", offset = 0, points = TRUE, intervals = TRUE, ...) {
  results <- .unformatFrame(results[[1]])
  if (points) points(seq_len(nrow(results)) + offset, results[, 1], pch = pch, cex = 1.5, col = col, lwd = 2, bg = .colorIntensity(col, .6))
  if (intervals) arrows(seq_len(nrow(results)) + offset, results[, 2], seq_len(nrow(results)) + offset, results[, 3], col = col, lwd = 2, length = 0)
  if (connect) {
    if (nrow(results) > 1) {
      for (i in 1:(nrow(results) - 1)) arrows(i + offset, results[i, 1], i + 1 + offset, results[i + 1, 1], code = 3, length = 0, lty = 1, col = "black")
    }
  }
  if (!is.null(line)) {
    abline(h = line, lty = 2, col = "black")
  }
  if (!is.null(rope)) {
    rect(0, rope[1], nrow(results) + 1, rope[2], col = .colorTransparent("black", 15), border = NA)
  }
  if (values) {
    results <- .formatFrame(results, digits = digits)
    text(seq_len(nrow(results)) + offset, as.numeric(results[, 1]), results[, 1], cex = .8, pos = pos, offset = .5, font = 2, col = col)
    text(seq_len(nrow(results)) + offset, as.numeric(results[, 2]), results[, 2], cex = .8, pos = pos, offset = .5, col = col)
    text(seq_len(nrow(results)) + offset, as.numeric(results[, 3]), results[, 3], cex = .8, pos = pos, offset = .5, col = col)
  }
}

.intervalsComp <- function(results, add = FALSE, main = NULL, ylab = "Outcome", xlab = "", ylim = NULL, slab = NULL, rope = NULL, values = TRUE, digits = 3, connect = FALSE, pos = c(2, 2, 4), pch = c(15, 15, 17), col = "black", offset = 0, points = TRUE, intervals = TRUE, lines = TRUE, ...) {
  results <- .unformatFrame(results[[1]])
  graph <- results
  graph[3, ] <- results[3, ] + results[1, 1]
  if (points) points(1:3 + offset, graph[, 1], pch = pch, cex = 1.5, col = col, lwd = 2, bg = .colorIntensity(col, .6))
  if (intervals) arrows(1:3 + offset, graph[, 2], 1:3 + offset, graph[, 3], col = col, lwd = 2, length = 0)
  if (lines) arrows(c(1, 2, 4) + offset, graph[1:2, 1], 4, graph[1:2, 1], code = 3, length = 0, lty = 2, col = col)
  if (connect) {
    arrows(1, results[1, 1], 2, results[2, 1], code = 3, length = 0, lty = 1, col = "black")
  }
  if (!is.null(rope)) {
    graphrope <- rope + as.vector(results[1, 1])
    rect(2.6, graphrope[1], 3.6, graphrope[2], col = .colorTransparent("black", 15), border = NA)
  }
  if (values) {
    results <- .formatFrame(results, digits = digits)
    text(1:3 + offset, graph[, 1], results[, 1], cex = .8, pos = pos, offset = .5, font = 2, col = col)
    text(1:3 + offset, graph[, 2], results[, 2], cex = .8, pos = pos, offset = .5, col = col)
    text(1:3 + offset, graph[, 3], results[, 3], cex = .8, pos = pos, offset = .5, col = col)
  }
}

### Confidence Interval Plot

plotIntervals <- function(x, ...) {
  UseMethod("plotIntervals")
}

plotIntervals.easi <- plot.easi <- function(results, add = FALSE, ...) {
  output <- results
  if (length(results) == 1) {
    results[[1]] <- results[[1]][, c(1, (ncol(results[[1]]) - 1):ncol(results[[1]]))]
    if (!add) .plotMain(results, ...)
    .intervalsMain(results, ...)
  }
  if (length(results) == 2 && nrow(results[[1]] != nrow(results[[2]]))) {
    results <- .collapseList(results)
    results[[1]] <- results[[1]][, c(1, (ncol(results[[1]]) - 1):ncol(results[[1]]))]
    if (!add) .plotComp(results, ...)
    .intervalsComp(results, ...)
  }
  invisible(output)
}

addIntervals <- function(...) {
  plotIntervals(..., add = TRUE)
}
