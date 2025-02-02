# ------------------------------------------------------------------------------
# Function isp()
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2025-02-02
# ------------------------------------------------------------------------------
isp <- function(x, data, nb, fun, verbose = FALSE, ...) {

  # The input object 'x' can be either a class of 'sf' or 'data.frame'. 
  # Depending on the class of 'x', 'data' may not be required. The internal 
  # function 'chksegdata()' processes the information as required by the 
  # current function.

  # Process input data using chksegdata()
  if (verbose)
    tmp <- chksegdata(x, data)
  else
    tmp <- suppressMessages(chksegdata(x, data))
  
  coords <- tmp$coords
  pdf <- tmp$data

  # Verify 'coords' and 'data'
  if (ncol(pdf) < 2)
    stop("'data' must be a matrix with at least two columns", call. = FALSE)
  else if (!is.numeric(pdf))
    stop("'data' must be a numeric matrix", call. = FALSE)
  else if (nrow(pdf) != nrow(coords))
    stop("'data' must have the same number of rows as 'x'", call. = FALSE)
  
  # Generate distance matrix if 'nb' is missing
  if (missing(nb))
    nb <- as.matrix(dist(coords, ...))
  
  # Use default negative exponential function if 'fun' is missing
  if (missing(fun))
    fun <- function(z) exp(-z)
  
  # Process distance matrix
  if (isSymmetric(nb)) {
    pairID <- t(combn(1:nrow(nb), 2))  # Unique pairs
    pairDist <- as.numeric(as.dist(nb))
  } else {
    pairID <- expand.grid(1:nrow(nb), 1:nrow(nb))
    pairDist <- as.numeric(nb)
  }
  
  VALID <- which(pairDist != 0)
  pairID <- pairID[VALID,]
  pairDist <- pairDist[VALID]
  
  # Compute spatial interaction effect
  speffect <- fun(pairDist)
  
  pRow <- rowSums(pdf)  # Total population by census tracts
  pCol <- colSums(pdf)  # Total population by groups
  
  pA <- sapply(1:ncol(pdf), function(i) {
    sum(pdf[pairID[,1], i] * pdf[pairID[,2], i] * speffect) / pCol[i]
  })
  
  sum(pA) / (sum(pRow[pairID[,1]] * pRow[pairID[,2]] * speffect) / sum(pCol))
}
