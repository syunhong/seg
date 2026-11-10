# ------------------------------------------------------------------------------
# Function 'deseg'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# ------------------------------------------------------------------------------
deseg <- function(x, data, smoothing = "kernel", nrow = 100, ncol = 100,
                  window, sigma, verbose = FALSE) {

  tmp <- chksegdata(x, data)
  coords <- tmp$coords; data <- tmp$data
  smoothing <- match.arg(smoothing, "kernel")

  if (missing(window))
    window <- .bbox.window(coords)
  if (missing(sigma))
    sigma <- min(stats::bw.nrd(coords[, 1]), stats::bw.nrd(coords[, 2]))
  if (verbose) message("deseg: kernel smoothing (sigma = ", sigma, ") ...")
  s <- surface.kernel(coords, data, sigma, nrow, ncol, window)

  v <- .decomp(s$data)
  SL <- v - .decompL(s$data)
  SQ <- 1 / ncol(s$data)
  SC <- v - (SL + SQ)

  SegDecomp(d = c(SL, SC, SQ), s$coords, s$data, tmp$crs)
}

.decomp <- function(data) {
  dx <- rowSums(data)
  empty <- which(dx == 0)
  if (length(empty) > 0) {
    warning("remove ", length(empty), " rows with no population",
            call. = FALSE)
    dx <- dx[-empty]
    data <- data[-empty, , drop = FALSE]
  }
  sx <- rowSums((data / dx)^2)
  sum(dx * sx) / sum(dx)
}

.decompL <- function(data) {
  n <- nrow(data)
  dataL <- matrix(colSums(data) / n, nrow = n, ncol = ncol(data), byrow = TRUE)
  .decomp(dataL)
}
