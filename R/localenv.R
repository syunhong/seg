# ------------------------------------------------------------------------------
# Function 'localenv'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# ------------------------------------------------------------------------------
localenv <- function(x, data, power = 2, useExp = TRUE, scale = FALSE,
                     maxdist, sprel, tol = .Machine$double.eps) {

  tmp <- chksegdata(x, data)
  coords <- tmp$coords; data <- tmp$data

  if (missing(maxdist)) {
    if (scale)
      stop("'maxdist' must be specified when 'scale' is set to TRUE",
           call. = FALSE)
    maxdist <- -1
  } else if (!is.numeric(maxdist)) {
    stop("'maxdist' must be numeric", call. = FALSE)
  } else if (maxdist < 0) {
    stop("'maxdist' must be greater than or equal to 0", call. = FALSE)
  }

  if (missing(sprel))
    sprel <- coords
  else if (!inherits(sprel, "nb") && !inherits(sprel, "dist"))
    stop("invalid object 'sprel'", call. = FALSE)

  env <- localenv.get(sprel, data, power, useExp, scale, maxdist, tol)
  SegLocal(coords, data, env, tmp$crs)
}

localenv.get <- function(sprel, data, power, useExp, scale, maxdist, tol) {

  if (inherits(sprel, "nb")) {
    # row-standardised neighbours ('W' style), e.g., from spdep::poly2nb()
    if (length(sprel) != nrow(data))
      stop("'data' must have the same number of rows as 'sprel'",
           call. = FALSE)
    nbs <- lapply(sprel, function(z) z[z > 0])
    if (any(lengths(nbs) == 0))
      stop("some units in 'sprel' have no neighbours", call. = FALSE)
    env <- t(vapply(nbs, function(z) colMeans(data[z, , drop = FALSE]),
                    numeric(ncol(data))))
  }

  else if (inherits(sprel, "dist")) {
    d <- as.matrix(sprel)
    if (nrow(d) != nrow(data))
      stop("'data' must have the same number of rows as 'sprel'",
           call. = FALSE)
    w <- if (useExp) exp(power * d * -1) else 1 / (d + tol)^power
    if (maxdist >= 0)
      w[d > maxdist] <- 0
    env <- (w %*% data) / rowSums(w)
  }

  else {
    if (nrow(sprel) != nrow(data))
      stop("'data' must have the same number of rows as 'sprel'",
           call. = FALSE)
    env <- .Call(C_envconstruct, as.double(sprel[, 1]), as.double(sprel[, 2]),
                 as.double(data), as.integer(ncol(data)), as.double(power),
                 as.integer(useExp), as.integer(scale), as.double(maxdist),
                 as.double(tol))
  }

  dimnames(env) <- dimnames(data)
  env
}
