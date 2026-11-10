# ------------------------------------------------------------------------------
# Functions 'isp' and 'whiteseg'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# ------------------------------------------------------------------------------
isp <- function(x, data, nb, fun, verbose = FALSE, ...) {

  tmp <- chksegdata(x, data)
  coords <- tmp$coords; pdf <- tmp$data

  if (missing(nb))
    nb <- as.matrix(stats::dist(x = coords, ...))
  if (missing(fun))
    fun <- function(z) exp(-z)

  # spatial interaction between pairs of units at non-zero distances
  sym <- isSymmetric(unname(nb))
  f <- matrix(0, nrow(nb), ncol(nb))
  nz <- which(nb != 0)
  f[nz] <- fun(nb[nz])
  if (sym)
    diag(f) <- 0

  pRow <- rowSums(pdf)
  pCol <- colSums(pdf)
  pA <- colSums(pdf * (f %*% pdf)) / pCol
  pB <- sum(pRow * (f %*% pRow)) / sum(pCol)
  as.vector(sum(pA) / pB)
}

whiteseg <- function(x, data, nb, fun, verbose = FALSE, ...) {
  .Deprecated("isp", package = "seg")
  isp(x, data, nb, fun, verbose, ...)
}
