# ------------------------------------------------------------------------------
# Function 'conprof'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# ------------------------------------------------------------------------------
conprof <- function(data, grpID = 1, n = 999, graph = TRUE, add = FALSE, ...) {

  data <- as.matrix(data)
  if (ncol(data) < 2 || !is.numeric(data))
    stop("'data' must be a numeric matrix with at least two columns",
         call. = FALSE)
  if (length(grpID) > 1) {
    warning("'grpID' has more than one value", call. = FALSE)
    grpID <- grpID[1]
  }
  val <- conprof.calc(data, grpID, n)

  if (graph) {
    if (!add) {
      graphics::plot(NA, xlim = c(0, 1.05), ylim = c(0, 1.05), xaxt = "n",
                     yaxt = "n", ...)
      at <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
      graphics::axis(side = 1, at = at, labels = at * 100, ...)
      graphics::axis(side = 2, at = at, labels = at * 100, ...)
    }
    graphics::lines(val$x, val$y, ...)
  }

  p <- sum(data[, grpID]) / sum(data)
  above <- which(val$x >= p)
  below <- which(val$x < p)
  partA <- sum(val$y[above]) / n
  partB <- p - (sum(val$y[below]) / n)
  d <- (partB + partA) / (1 - p)
  list(x = val$x, y = val$y, d = d)
}

conprof.calc <- function(data, grpID, n = 999) {
  grp <- data[, grpID]
  total <- rowSums(data)
  xval <- seq(0, 1, length.out = n)
  # proportion of the group living in units where its share is >= threshold
  yval <- vapply(xval, function(v) sum(grp[grp >= total * v]), numeric(1)) /
    sum(grp)
  list(x = c(xval, 1), y = c(yval, 0))
}
