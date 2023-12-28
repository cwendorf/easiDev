# Estimation Approach to Statistical Inference
## Intervals

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

plotIntervals.list <- function(results, add = FALSE, ...) {
  out <- results
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
  invisible(out)
}

addIntervals <- function(...) {
  plotIntervals(..., add = TRUE)
}
