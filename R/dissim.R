# ------------------------------------------------------------------------------
# Functions 'dissim' and 'seg'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# ------------------------------------------------------------------------------
dissim <- function(x, data, nb, adjust = FALSE, verbose = FALSE) {

  if (!missing(x)) {
    tmp <- chksegdata(x, data)
    if (!tmp$polygons)
      stop("'x' must be an sf object with polygon geometries", call. = FALSE)
    data <- tmp$data
  }

  data <- as.matrix(data)
  if (ncol(data) > 2) {
    warning("'data' has more than two columns; only the first two are used",
            call. = FALSE)
    data <- data[, 1:2]
  }
  if (any(data < 0))
    stop("negative value(s) in 'data'", call. = FALSE)
  if (any(colSums(data) <= 0))
    stop("the sum of each column in 'data' must be > 0", call. = FALSE)

  out <- list(d = NA, dm = NA, dw = NA, ds = NA, user = NA)

  # Duncan and Duncan's index of dissimilarity
  b <- data[, 1] / sum(data[, 1])
  w <- data[, 2] / sum(data[, 2])
  out$d <- as.vector(sum(abs(b - w)) / 2)

  if (!missing(x) && adjust) {
    if (verbose) message("dissim: finding shared boundaries ...")
    sb <- .shared.boundaries(tmp$geom)
    if (nrow(sb) == 0) {
      warning("no shared boundaries found; adjusted indices not computed",
              call. = FALSE)
    } else {
      # Morrill (1991): rook contiguity, standardised by the number of joins
      out$dm <- out$d - .d.adjust.pairs(data, sb$i, sb$j, 1 / nrow(sb))
      # Wong (1993): length of shared boundaries
      wl <- sb$length / sum(sb$length)
      out$dw <- out$d - .d.adjust.pairs(data, sb$i, sb$j, wl)
      # Wong (1993): ... and perimeter/area ratio
      A <- as.numeric(sf::st_area(tmp$geom))
      P <- as.vector(tapply(sb$length, factor(sb$i, seq_along(A)), sum))
      P[is.na(P)] <- 0
      PAR <- P / A
      ws <- wl * ((PAR[sb$i] + PAR[sb$j]) / 2) / max(PAR)
      out$ds <- out$d - .d.adjust.pairs(data, sb$i, sb$j, ws)
    }
  }

  if (!missing(nb))
    out$user <- out$d - .d.adjust(data, nb)
  out
}

seg <- function(data, nb) {
  .Deprecated("dissim", package = "seg")
  if (missing(nb)) dissim(data = data)$d else dissim(data = data, nb = nb)$user
}

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

# Pairs of polygons that share a boundary (both directions) and its length
.shared.boundaries <- function(geom) {
  bnd <- sf::st_boundary(geom)
  ii <- sf::st_intersection(bnd, bnd)
  idx <- attr(ii, "idx")
  len <- as.numeric(sf::st_length(ii))
  keep <- idx[, 1] != idx[, 2] & len > 0
  data.frame(i = idx[keep, 1], j = idx[keep, 2], length = len[keep])
}

# Spatial adjustment: sum of w_ij * |z_i - z_j| over pairs (i, j)
.d.adjust.pairs <- function(data, i, j, w) {
  total <- rowSums(data)
  empty <- which(total == 0)
  if (length(empty) > 0) {
    warning("remove ", length(empty), " rows with no population",
            call. = FALSE)
    ok <- !(i %in% empty | j %in% empty)
    i <- i[ok]; j <- j[ok]; w <- rep(w, length.out = length(ok))[ok]
  }
  z <- data[, 1] / total
  sum(abs(z[j] - z[i]) * w)
}

.d.adjust <- function(data, nb) {
  if (!is.matrix(nb))
    stop("'nb' must be a matrix", call. = FALSE)
  else if (nrow(nb) != ncol(nb))
    stop("'nb' must be a square matrix", call. = FALSE)
  else if (nrow(nb) != nrow(data))
    stop("nrow(nb) must match nrow(data)", call. = FALSE)
  if (!isTRUE(all.equal(sum(nb), 1)))
    warning("the sum of all elements in 'nb' does not equal 1", call. = FALSE)
  idx <- which(nb != 0, arr.ind = TRUE)
  .d.adjust.pairs(data, idx[, 1], idx[, 2], nb[idx])
}
