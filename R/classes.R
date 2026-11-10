# ------------------------------------------------------------------------------
# S3 classes 'SegLocal', 'SegSpatial' and 'SegDecomp'
#
# These replace the S4 classes used up to version 0.5-7. Components are
# accessed with '$' (e.g., x$d) instead of '@'.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Constructors
# ------------------------------------------------------------------------------
SegLocal <- function(coords, data, env, crs = NA) {
  coords <- as.matrix(coords); data <- as.matrix(data)
  if (missing(env)) env <- data else env <- as.matrix(env)
  if (ncol(coords) != 2 || !is.numeric(coords))
    stop("'coords' must be a numeric matrix with two columns", call. = FALSE)
  if (ncol(data) < 2 || !is.numeric(data))
    stop("'data' must be a numeric matrix with at least two columns",
         call. = FALSE)
  if (nrow(data) != nrow(coords))
    stop("'data' must have the same number of rows as 'coords'", call. = FALSE)
  if (!is.numeric(env) || any(dim(env) != dim(data)))
    stop("'env' must be a numeric matrix with the same dimensions as 'data'",
         call. = FALSE)
  colnames(coords) <- c("x", "y")
  structure(list(coords = coords, data = data, env = env,
                 crs = sf::st_crs(crs)),
            class = "SegLocal")
}

SegSpatial <- function(d, r, h, p, coords, data, env, crs = NA) {
  out <- SegLocal(coords, data, env, crs)
  if (missing(d) || is.null(d)) d <- numeric(0)
  if (missing(r) || is.null(r)) r <- numeric(0)
  if (missing(h) || is.null(h)) h <- numeric(0)
  if (missing(p) || is.null(p)) p <- matrix(0, 0, 0)
  if (nrow(p) != ncol(p))
    stop("'p' must be a square matrix", call. = FALSE)
  out <- c(list(d = as.vector(d), r = as.vector(r), h = as.vector(h), p = p),
           unclass(out))
  structure(out, class = c("SegSpatial", "SegLocal"))
}

SegDecomp <- function(d, coords, data, crs = NA) {
  coords <- as.matrix(coords); data <- as.matrix(data)
  if (length(d) != 3)
    stop("'d' must be of length three", call. = FALSE)
  if (nrow(data) != nrow(coords))
    stop("'data' must have the same number of rows as 'coords'", call. = FALSE)
  colnames(coords) <- c("x", "y")
  structure(list(d = as.vector(d), coords = coords, data = data,
                 crs = sf::st_crs(crs)),
            class = "SegDecomp")
}

.crs.label <- function(crs) {
  if (is.na(crs)) "NA" else crs$input
}

# ------------------------------------------------------------------------------
# Methods for 'SegLocal'
# ------------------------------------------------------------------------------
print.SegLocal <- function(x, ...) {
  cat("Class                 :", class(x)[1], "\n")
  cat("Number of data points :", nrow(x$coords), "\n")
  cat("Number of data columns:", ncol(x$data), "\n")
  cat("CRS                   :", .crs.label(x$crs), "\n")
  cat("Components            :", names(x), "\n")
  invisible(x)
}

summary.SegLocal <- function(object, ...) {
  cat("An object of class \"", class(object)[1], "\"\n", sep = "")
  cat("Coordinates:\n")
  tmp <- t(apply(object$coords, 2, range))
  dimnames(tmp) <- list(c("x", "y"), c("min", "max"))
  print(tmp)
  cat("CRS:", .crs.label(object$crs), "\n")

  cat("\nData values (%):\n")
  tmp <- object$data / rowSums(object$data) * 100
  print(apply(tmp, 2, function(z) summary(z, ...)))

  cat("\nLocal environment composition (%):\n")
  tmp <- object$env / rowSums(object$env) * 100
  print(apply(tmp, 2, function(z) summary(z, ...)))
  invisible(object)
}

update.SegLocal <- function(object, coords, data, env, crs, ...) {
  if (missing(coords)) coords <- object$coords
  if (missing(data)) data <- object$data
  if (missing(env)) env <- object$env
  if (missing(crs)) crs <- object$crs
  object[c("coords", "data", "env", "crs")] <-
    unclass(SegLocal(coords, data, env, crs))[c("coords", "data", "env", "crs")]
  object
}

.sizeclass <- function(v) {
  qq <- stats::quantile(v, probs = c(0.2, 0.4, 0.6, 0.8))
  findInterval(v, qq, left.open = TRUE) + 1
}

plot.SegLocal <- function(x, which.col, main, ...) {
  env <- x$env
  if (missing(which.col))
    which.col <- seq_len(ncol(env))
  if (missing(main))
    main <- if (is.null(colnames(env))) paste("Data", which.col)
            else colnames(env)[which.col]
  main <- rep(main, length.out = length(which.col))
  for (i in seq_along(which.col)) {
    graphics::plot(x$coords[, 1], x$coords[, 2],
                   cex = .sizeclass(env[, which.col[i]]), main = main[i], ...)
  }
  invisible(NULL)
}

points.SegLocal <- function(x, which.col = 1, ...) {
  if (length(which.col) > 1)
    warning("'which.col' has a length of > 1", call. = FALSE)
  graphics::points(x$coords[, 1], x$coords[, 2],
                   cex = .sizeclass(x$env[, which.col[1]]), ...)
}

st_as_sf.SegLocal <- function(x, ...) {
  sf::st_as_sf(data.frame(x$coords, x$env), coords = c("x", "y"),
               crs = x$crs)
}

# ------------------------------------------------------------------------------
# Methods for 'SegSpatial'
# ------------------------------------------------------------------------------
print.SegSpatial <- function(x, digits = getOption("digits"), ...) {
  cat("\n\tReardon and O'Sullivan's spatial segregation measures\n\n")
  show1 <- function(lab, v) {
    cat(lab)
    if (length(v) > 0) cat(round(v, digits), "\n") else cat("-\n")
  }
  show1("Dissimilarity (D)     : ", x$d)
  show1("Relative diversity (R): ", x$r)
  show1("Information theory (H): ", x$h)
  cat("Exposure/Isolation (P): ")
  if (length(x$p) > 0) {
    p <- x$p
    if (is.null(colnames(p))) colnames(p) <- paste("Group", seq_len(ncol(p)))
    if (is.null(rownames(p))) rownames(p) <- paste("Group", seq_len(nrow(p)))
    cat("\n")
    print(p, digits = digits, ...)
    cat("--\n")
    cat("The exposure/isolation matrix should be read horizontally.\n")
    cat("Read 'help(spseg)' for more details.\n")
  } else {
    cat("-\n")
  }
  invisible(x)
}

as.list.SegSpatial <- function(x, ...) {
  list(d = x$d, r = x$r, h = x$h, p = x$p)
}

st_as_sf.SegSpatial <- function(x, ...) {
  loc <- x$local
  if (is.null(loc))
    loc <- matrix(NA_real_, nrow(x$coords), 3,
                  dimnames = list(NULL, c("d", "r", "h")))
  geom <- x$geometry
  id <- x$id

  if (!is.null(geom) && !is.null(id) && .is.polygons(geom)) {
    # population-weighted mean of the local values within each polygon
    w <- rowSums(x$data)
    ok <- !is.na(id) & stats::complete.cases(loc)
    vals <- matrix(NA_real_, length(geom), ncol(loc),
                   dimnames = list(NULL, colnames(loc)))
    if (any(ok)) {
      num <- rowsum(loc[ok, , drop = FALSE] * w[ok], id[ok])
      den <- rowsum(w[ok], id[ok])
      vals[as.integer(rownames(num)), ] <- num / as.vector(den)
    }
    return(sf::st_sf(as.data.frame(vals), geometry = geom))
  }

  if (!is.null(geom) && !is.null(id) && all(!is.na(id)) &&
      length(id) == nrow(loc))
    return(sf::st_sf(as.data.frame(loc), geometry = geom[id]))

  sf::st_as_sf(data.frame(x$coords, loc), coords = c("x", "y"), crs = x$crs)
}

plot.SegSpatial <- function(x, index = c("dissimilarity", "diversity",
                                         "information"), ...) {
  index <- match.arg(index)
  col <- c(dissimilarity = "d", diversity = "r", information = "h")[index]
  label <- c(d = "D", r = "R", h = "H")[col]
  v <- st_as_sf.SegSpatial(x)[col]

  dots <- list(...)
  if (is.null(dots$main))
    dots$main <- paste0("Local segregation (", label, ")")
  if (is.null(dots$pal) && is.null(dots$col))
    dots$pal <- function(n) grDevices::hcl.colors(n, "YlOrRd", rev = TRUE)
  if (is.null(dots$mar) && (!is.null(dots$xlab) || !is.null(dots$ylab))) {
    mar <- if (isTRUE(dots$axes)) c(2.1, 2.1, 1.2, 1) else c(1, 1, 1.2, 1)
    if (!is.null(dots$xlab)) mar[1] <- 4.1
    if (!is.null(dots$ylab)) mar[2] <- 4.1
    dots$mar <- mar
  }
  if (is.null(dots$pch) && all(sf::st_geometry_type(v) == "POINT"))
    dots$pch <- 16
  do.call(graphics::plot, c(list(v), dots))
  invisible(v)
}

# ------------------------------------------------------------------------------
# Methods for 'SegDecomp'
# ------------------------------------------------------------------------------
print.SegDecomp <- function(x, digits = getOption("digits"), ...) {
  print(sum(x$d), digits = digits)
  cat("\nSubcomponents:\n")
  tmp <- matrix(x$d, nrow = 1,
                dimnames = list("", c("Location", "Composition",
                                      "Qualitative")))
  print(tmp, digits = digits)
  invisible(x)
}

as.vector.SegDecomp <- function(x, mode = "any") {
  as.vector(x$d, mode = mode)
}

st_as_sf.SegDecomp <- function(x, ...) {
  sf::st_as_sf(data.frame(x$coords, x$data), coords = c("x", "y"),
               crs = x$crs)
}
