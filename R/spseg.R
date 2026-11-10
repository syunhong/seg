# ------------------------------------------------------------------------------
# Functions 'spseg' and 'spatseg'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# ------------------------------------------------------------------------------
spseg <- function(x, data, method = "all", smoothing = "none",
                  nrow = 100, ncol = 100, window, sigma, useC = TRUE,
                  negative.rm = FALSE, tol = .Machine$double.eps,
                  verbose = FALSE, ...) {

  # STEP 1 Data preparation ----------------------------------------------------
  tmp <- chksegdata(x, data)
  coords <- tmp$coords; pdata <- tmp$data; geom <- tmp$geom
  smoothing <- match.arg(smoothing, c("none", "kernel", "equal"))
  id <- if (is.null(geom)) NULL else seq_along(geom)

  # STEP 2 Estimate the data surface -------------------------------------------
  if (smoothing == "equal") {
    if (!tmp$polygons)
      stop("'x' must be polygons to use the \"equal\" smoothing option",
           call. = FALSE)
    if (verbose) message("spseg: redistributing the population ...")
    s <- surface.equal(geom, pdata, nrow, ncol)
    coords <- s$coords; pdata <- s$data; id <- s$id
  }

  else if (smoothing == "kernel") {
    if (missing(window))
      window <- .bbox.window(coords)
    if (missing(sigma))
      sigma <- min(stats::bw.nrd(coords[, 1]), stats::bw.nrd(coords[, 2]))
    if (verbose) message("spseg: kernel smoothing (sigma = ", sigma, ") ...")
    s <- surface.kernel(coords, pdata, sigma, nrow, ncol, window)
    coords <- s$coords; pdata <- s$data
    id <- if (tmp$polygons) .which.polygon(coords, geom) else NULL
    if (!tmp$polygons) geom <- NULL
  }

  # STEP 3 Local environments --------------------------------------------------
  if (verbose) message("spseg: computing the local environments ...")
  env <- localenv(coords, pdata, ...)
  env$crs <- tmp$crs

  # STEP 4 Segregation indices -------------------------------------------------
  if (verbose) message("spseg: computing the segregation indices ...")
  out <- spatseg(env, method, useC, negative.rm, tol)
  out$geometry <- geom
  out$id <- id
  out
}

spatseg <- function(env, method = "all", useC = TRUE, negative.rm = FALSE,
                    tol = .Machine$double.eps) {

  if (!inherits(env, "SegLocal"))
    stop("'env' must be an object of class \"SegLocal\"", call. = FALSE)

  dd <- env$data + tol; ee <- env$env + tol
  keep <- seq_len(nrow(dd))

  # Non-positive values (often as a result of kernel smoothing)
  fix <- function(test, dd, ee, keep, msg) {
    bad <- apply(test, 1, function(z) any(z <= 0))
    if (any(bad) && negative.rm) {
      warning("rows with negative values ", msg[1], call. = FALSE)
      ok <- which(!bad)
      list(dd = dd[ok, , drop = FALSE], ee = ee[ok, , drop = FALSE],
           keep = keep[ok])
    } else if (any(bad)) {
      warning("negative values ", msg[2], call. = FALSE)
      index <- (test <= 0)
      list(dd = replace(dd, index, tol), ee = replace(ee, index, tol),
           keep = keep)
    } else {
      list(dd = dd, ee = ee, keep = keep)
    }
  }
  tmp <- fix(dd, dd, ee, keep, c("have been removed",
                                 "have been replaced with 'tol'"))
  tmp <- fix(tmp$ee, tmp$dd, tmp$ee, tmp$keep, c("removed",
                                                 "replaced with 'tol'"))
  dd <- tmp$dd; ee <- tmp$ee; keep <- tmp$keep

  method <- match.arg(method, c("exposure", "information", "diversity",
                                "dissimilarity", "all"), several.ok = TRUE)
  if ("all" %in% method)
    method <- c("exposure", "information", "diversity", "dissimilarity")
  flag <- c("exposure", "information", "diversity", "dissimilarity") %in% method

  m <- ncol(dd)
  if (useC) {
    tmp <- .Call(C_spsegIDX, as.vector(dd), as.vector(ee), as.integer(m),
                 as.integer(flag))
    n <- m^2
    P <- if (flag[1]) matrix(tmp[1:n], ncol = m, byrow = TRUE) else NULL
    H <- if (flag[2]) tmp[n + 1] else NULL
    R <- if (flag[3]) tmp[n + 2] else NULL
    D <- if (flag[4]) tmp[n + 3] else NULL
  } else {
    g <- .global.index(dd, ee)
    P <- if (flag[1]) g$p else NULL
    H <- if (flag[2]) g$h else NULL
    R <- if (flag[3]) g$r else NULL
    D <- if (flag[4]) g$d else NULL
  }
  if (!is.null(P))
    rownames(P) <- colnames(P) <- colnames(dd)

  # Local values, whose population-weighted mean equals the global index
  local <- matrix(NA_real_, nrow(env$data), 3,
                  dimnames = list(NULL, c("d", "r", "h")))
  local[keep, ] <- .local.index(dd, ee)
  local[, !flag[c(4, 3, 2)]] <- NA

  out <- SegSpatial(D, R, H, P, env$coords, env$data, env$env, env$crs)
  out$local <- local
  out
}

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------
.xlogx <- function(z, base) ifelse(z > 0, z * log(z, base = base), 0)

.local.index <- function(dd, ee) {
  m <- ncol(dd)
  pim <- colSums(dd) / sum(dd)
  prop <- ee / rowSums(ee)
  E <- -sum(.xlogx(pim, m))
  I <- sum(pim * (1 - pim))
  cbind(d = rowSums(abs(sweep(prop, 2, pim))) / (2 * I),
        r = 1 - rowSums(prop * (1 - prop)) / I,
        h = 1 - (-rowSums(.xlogx(prop, m))) / E)
}

.global.index <- function(dd, ee) {
  w <- rowSums(dd) / sum(dd)
  loc <- .local.index(dd, ee)
  prop <- ee / rowSums(ee)
  list(p = crossprod(sweep(dd, 2, colSums(dd), "/"), prop),
       d = sum(w * loc[, "d"]), r = sum(w * loc[, "r"]),
       h = sum(w * loc[, "h"]))
}

.bbox.window <- function(coords) {
  x <- range(coords[, 1]); y <- range(coords[, 2])
  matrix(c(x[1], y[1], x[1], y[2], x[2], y[2], x[2], y[1]),
         ncol = 2, byrow = TRUE)
}
