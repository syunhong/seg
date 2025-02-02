# ------------------------------------------------------------------------------
# Function 'spatseg'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
spatseg <- function(env, method = "all", useC = TRUE, negative.rm = FALSE, 
                    tol = .Machine$double.eps) {
  
  # Ensure data consistency
  dd <- env@data + tol
  ee <- env@env + tol
  
  # Check and handle negative values (often from kernel smoothing)
  negIDs <- rowSums(dd <= 0) > 0
  if (sum(negIDs) > 0) {
    if (negative.rm) {
      warning("Rows with negative values have been removed", call. = FALSE)
      dd <- dd[!negIDs, ]
      ee <- ee[!negIDs, ]
    } else {
      warning("Negative values replaced with 'tol'", call. = FALSE)
      dd[negIDs, ] <- tol
      ee[negIDs, ] <- tol
    }
  }
  
  negIDs <- rowSums(ee <= 0) > 0
  if (sum(negIDs) > 0) {
    if (negative.rm) {
      warning("Rows with negative values removed", call. = FALSE)
      dd <- dd[!negIDs, ]
      ee <- ee[!negIDs, ]
    } else {
      warning("Negative values replaced with 'tol'", call. = FALSE)
      ee[negIDs, ] <- tol
      dd[negIDs, ] <- tol
    }
  }
  
  # Select methods to compute
  method <- match.arg(method, c("exposure", "information", "diversity", 
                                "dissimilarity", "all"), several.ok = TRUE)
  if ("all" %in% method)
    method <- c("exposure", "information", "diversity", "dissimilarity")
  
  P <- matrix(0, nrow = 0, ncol = 0)
  H <- numeric(); R <- numeric(); D <- numeric()
  
  if (useC) {
    # Use C-based implementation for performance
    m <- ncol(dd)
    method_flags <- c("exposure" %in% method, "information" %in% method,
                      "diversity" %in% method, "dissimilarity" %in% method)
    tmp <- .Call("spsegIDX", as.vector(dd), as.vector(ee), 
                 as.integer(m), as.integer(method_flags))
    results <- list()
    n <- m^2
    if (!is.na(tmp[1])) {
      results$p <- matrix(tmp[1:n], ncol = m, byrow = TRUE)
      rownames(results$p) <- colnames(results$p) <- colnames(dd)
    }
    if (!is.na(tmp[n+1])) results$h <- tmp[n+1] 
    if (!is.na(tmp[n+2])) results$r <- tmp[n+2]
    if (!is.na(tmp[n+3])) results$d <- tmp[n+3]
  } else {
    # Compute indices manually
    m <- ncol(dd)
    ptsSum <- sum(dd)  # Total population
    ptsRowSum <- rowSums(dd)  # Population per location
    ptsColSum <- colSums(dd)  # Population per group
    ptsProp <- ptsColSum / ptsSum  # Group proportions
    envProp <- ee / rowSums(ee)  # Proportion of each group per local environment
    
    if ("exposure" %in% method) {
      P <- matrix(0, nrow = m, ncol = m)
      rownames(P) <- colnames(P) <- colnames(dd)
      for (i in 1:m) {
        A <- dd[, i] / ptsColSum[i]
        for (j in 1:m) {
          P[i, j] <- sum(A * envProp[, j])
        }
      }
    }
    
    if ("information" %in% method) {
      Ep <- -rowSums(envProp * log(envProp, base = m))
      E <- -sum(ptsProp * log(ptsProp, base = m))
      H <- 1 - (sum(ptsRowSum * Ep) / (ptsSum * E))
    }
    
    if ("diversity" %in% method) {
      Ip <- rowSums(envProp * (1 - envProp))
      I <- sum(ptsProp * (1 - ptsProp))
      R <- 1 - sum((ptsRowSum * Ip) / (ptsSum * I))
    }
    
    if ("dissimilarity" %in% method) {
      I <- sum(ptsProp * (1 - ptsProp))
      constant <- ptsRowSum / (2 * ptsSum * I)
      Dp <- abs(envProp - ptsProp)
      D <- sum(colSums(Dp * constant))
    }
    
    results <- list(p = P, h = H, r = R, d = D)
  }
  
  SegSpatial(results$d, results$r, results$h, results$p, 
             env@coords, env@data, env@env, env@proj4string)
}