# ------------------------------------------------------------------------------
# Internal function 'surface.kernel'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
surface.kernel <- function(coords, data, sigma, nrow, ncol, window, verbose) {
  
  if (verbose){
    begTime <- Sys.time(); fn <- match.call()[[1]]
    message(fn, ": kernel smoothing of the population data ...")
  }
  
  x <- coords[, 1]
  y <- coords[, 2]
  results <- vector("list", ncol(data))  # Store results for each column
  
  for (i in seq_len(ncol(data))) {
    if (verbose)
      message(fn, ": processing column ", i)
    
    # Expand coordinates by data values
    wgtXY <- cbind(rep(x, data[, i]), rep(y, data[, i]))
    
    # Perform kernel smoothing
    tmp1 <- splancs::kernel2d(wgtXY, window, h0 = sigma, nx = ncol, ny = nrow, quiet = TRUE)
    
    # Transform result into required format
    results[[i]] <- as.numeric(tmp1$z)
  }
  
  # Create coordinate grid
  pixels <- expand.grid(tmp1$x, tmp1$y)
  colnames(pixels) <- c("x", "y")
  
  # Combine smoothed values
  values <- do.call(cbind, results)
  colnames(values) <- colnames(data)
  
  if (verbose){
    tt <- as.numeric(difftime(Sys.time(), begTime, units = "sec"))
    message(fn, ": done! [", tt, " seconds]")
  }
  
  list(coords = pixels, data = values)
}
