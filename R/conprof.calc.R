# ------------------------------------------------------------------------------
# Internal function 'conprof.calc'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2025-02-02
# ------------------------------------------------------------------------------
conprof.calc <- function(data, grpID, n = 999) {

  colsum <- sum(data[[grpID]])
  rowsum <- rowSums(data)
  
  xval <- rbind(seq(0, 1, length.out = n))
  yval <- numeric(n)
  threshold <- rowsum %*% xval
  
  for (i in 1:n) {
    INDEX <- (data[[grpID]] >= threshold[, i])
    yval[i] <- sum(data[[grpID]][INDEX]) / colsum
  }
  
  list("x" = c(xval, 1), "y" = c(yval, 0))
}