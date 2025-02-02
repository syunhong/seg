# ------------------------------------------------------------------------------
# Internal function 'localenv.get'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
localenv.get <- function(sprel, data, power, useExp, scale, maxdist, tol) {
  
  grps <- colnames(data)
  rows <- rownames(data)
  
  # Case 1: If 'sprel' is an 'nb' (neighbor object)
  if (inherits(sprel, "nb")) {
    xmat <- spdep::nb2mat(sprel, style = "W")  # Convert nb to weight matrix
    if (nrow(xmat) != nrow(data)) 
      stop("'data' must have the same number of rows as 'sprel'", call. = FALSE)
    env <- xmat %*% data  # Matrix multiplication for spatial weights
  }
  
  # Case 2: If 'sprel' is a distance matrix
  else if (inherits(sprel, "dist")) {
    sprel <- as.matrix(sprel)  # Convert 'dist' object to matrix
    if (nrow(sprel) != nrow(data)) 
      stop("'data' must have the same number of rows as 'sprel'", call. = FALSE)
    
    env <- matrix(nrow = nrow(data), ncol = ncol(data))
    
    for (i in seq_len(nrow(data))) {
      if (useExp)
        weight <- exp(power * sprel[i, ] * -1)  # Exponential decay weighting
      else
        weight <- 1 / (sprel[i, ] + tol)^power  # Power-law weighting
      
      if (maxdist >= 0)
        weight[sprel[i, ] > maxdist] <- 0  # Zero out weights beyond max distance
      
      env[i, ] <- colSums(data * weight) / sum(weight)  # Use colSums() for efficiency
    }
  } 
  
  # Case 3: If 'sprel' is a coordinate matrix
  else {
    if (nrow(sprel) != nrow(data))
      stop("'data' must have the same number of rows as 'sprel'", call. = FALSE)
    
    xval <- sprel[, 1]
    yval <- sprel[, 2]
    dim <- ncol(data)
    data <- as.vector(data)
    
    env <- .Call("envconstruct", xval, yval, data, as.integer(dim), power, 
                 as.integer(useExp), as.integer(scale), maxdist, tol)
    
    # --------------------------------------------------------------------------
    # R version 'envconstruct()'
    # --------------------------------------------------------------------------
    # envconstruct <- function(x, y, v, d) {
    #   n <- length(v)
    #   env <- rep(0, n)
    #   
    #   for (i in 1:n) {
    #     m <- 0
    #     for (j in 1:n) {
    #       dx <- x[i] - x[j]
    #       dy <- y[i] - y[j]
    #       if (dx <= d && dy <= d) {
    #         if (dx^2 + dy^2 <= d^2) {
    #           env[i] <- env[i] + v[j]
    #           m <- m + 1
    #         }
    #       }
    #     }
    #     if (m > 1)
    #       env[i] <- env[i] / m
    #   }
    #   return(env)
    # }
    # --------------------------------------------------------------------------
    
  }
  
  colnames(env) <- grps
  rownames(env) <- rows
  env
}
