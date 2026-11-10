# ------------------------------------------------------------------------------
# Internal functions 'surface.equal' and 'surface.kernel'
#
# Construct a population surface on a regular grid.
# ------------------------------------------------------------------------------

# Population is assumed to be uniformly distributed within each polygon
surface.equal <- function(geom, data, nrow, ncol) {
  bb <- sf::st_bbox(geom)
  xx <- seq(bb[["xmin"]], bb[["xmax"]], length.out = ncol)
  yy <- seq(bb[["ymin"]], bb[["ymax"]], length.out = nrow)
  coords <- as.matrix(expand.grid(x = xx, y = yy))

  id <- .which.polygon(coords, geom)
  inside <- !is.na(id)
  coords <- coords[inside, , drop = FALSE]
  id <- id[inside]

  ncell <- tabulate(id, nbins = length(geom))
  empty <- sum(ncell == 0 & rowSums(data) > 0)
  if (empty > 0)
    warning(empty, " polygon(s) contain no grid points and their population ",
            "is not included; consider increasing 'nrow' and 'ncol'",
            call. = FALSE)

  values <- data[id, , drop = FALSE] / ncell[id]
  dimnames(values) <- list(NULL, colnames(data))
  rownames(coords) <- NULL
  list(coords = coords, data = values, id = id)
}

# Quartic kernel estimate with splancs::kernel2d(). Each person is a point,
# so the population counts are expanded with rep(), as in earlier versions.
surface.kernel <- function(coords, data, sigma, nrow, ncol, window) {
  if (inherits(window, c("sf", "sfc", "sfg"))) {
    window <- sf::st_coordinates(sf::st_geometry(window)[[1]])
    first <- window[, "L1"] == 1 & if ("L2" %in% colnames(window))
      window[, "L2"] == 1 else TRUE
    window <- window[first, c("X", "Y"), drop = FALSE]
    window <- window[-nrow(window), , drop = FALSE]
  }
  window <- unname(as.matrix(window))
  if (ncol(window) != 2 || nrow(window) < 3 || !is.numeric(window))
    stop("'window' must be a polygon given as a two-column matrix",
         call. = FALSE)
  if (any(data != floor(data)))
    warning("non-integer population counts are truncated for kernel ",
            "smoothing", call. = FALSE)

  x <- coords[, 1]; y <- coords[, 2]
  values <- NULL
  for (i in seq_len(ncol(data))) {
    pts <- cbind(rep(x, data[, i]), rep(y, data[, i]))
    k <- splancs::kernel2d(pts, window, h0 = sigma, nx = ncol, ny = nrow,
                           quiet = TRUE)
    values <- cbind(values, as.numeric(k$z))
  }
  pixels <- as.matrix(expand.grid(x = k$x, y = k$y))

  inside <- !is.na(values[, 1])
  pixels <- pixels[inside, , drop = FALSE]
  values <- values[inside, , drop = FALSE]
  dimnames(values) <- list(NULL, colnames(data))
  rownames(pixels) <- NULL
  list(coords = pixels, data = values)
}
