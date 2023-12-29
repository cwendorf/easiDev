# Estimation Approach to Statistical Inference
## Plot

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