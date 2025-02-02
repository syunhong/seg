# ------------------------------------------------------------------------------
# Function 'conprof'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2025-02-02
# ------------------------------------------------------------------------------
conprof <- function(data, grpID = 1, n = 999, graph = TRUE, add = FALSE, ...) {
  
  if (inherits(data, "sf"))
    data <- st_drop_geometry(data)
  
  if (ncol(data) < 2 || !is.numeric(as.matrix(data)))
    stop("'data' must be a numeric matrix with at least two columns", call. = FALSE)
  
  if (length(grpID) > 1) {
    warning("'grpID' has more than one value, using the first value", call. = FALSE)
    grpID <- grpID[1]
  }
  
  colsum <- sum(data[,grpID])        
  rowsum <- apply(data, 1, function(z) sum(z))
  
  xval <- rbind(seq(0, 1, length.out = n))
  yval <- numeric(n)
  threshold <- rowsum %*% xval
  
  for (i in 1:n) {
    INDEX <- (data[,grpID] >= threshold[,i])
    yval[i] <- sum(data[INDEX,grpID]) / colsum
  }
  
  val <- list("x" = c(xval, 1), "y" = c(yval, 0))
  
  if (graph) {
    if (!add) {
      plot(NA, xlim = c(0, 1.05), ylim = c(0, 1.05), xaxt = "n", yaxt = "n", ...)
      intrval <- seq(0, 1, by = 0.2)
      axistxt <- intrval * 100
      axis(side = 1, at = intrval, labels = axistxt, ...)
      axis(side = 2, at = intrval, labels = axistxt, ...)      
    }
    lines(val$x, val$y, ...)
  }
  
  # Proportion of the group 
  p <- sum(data[[grpID]]) / sum(data)
  
  above <- which(val$x >= p)
  below <- which(val$x < p)
  partA <- sum(val$y[above]) / n
  partB <- p - (sum(val$y[below]) / n)
  
  d <- (partB + partA) / (1 - p)
  
  return(list(x = val$x, y = val$y, d = d))
}
