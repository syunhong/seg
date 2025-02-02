# ------------------------------------------------------------------------------
# Function 'deseg'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2025-02-02
# ------------------------------------------------------------------------------
deseg <- function(x, data, smoothing = "kernel", nrow = 100, ncol = 100, 
                  window, sigma, verbose = FALSE) {
  
  # ----------------------------------------------------------------------------
  # STEP 1 Data preparation
  # ----------------------------------------------------------------------------
  if (verbose)
    tmp <- chksegdata(x, data)    
  else
    tmp <- suppressMessages(chksegdata(x, data))
  
  coords <- tmp$coords
  data <- tmp$data
  
  crs <- if (!is.null(st_crs(x))) st_crs(x)$wkt else as.character(NA)
  
  smoothing <- match.arg(smoothing, c("kernel"), several.ok = FALSE)
  
  # ----------------------------------------------------------------------------
  # STEP 2 Estimate the data surface
  # ----------------------------------------------------------------------------
  if (smoothing == "kernel") {
    if (missing(window)) {
      x_range <- range(coords[[1]])
      y_range <- range(coords[[2]])
      window <- matrix(c(x_range[1], y_range[1], 
                         x_range[1], y_range[2], 
                         x_range[2], y_range[2], 
                         x_range[2], y_range[1]), 
                       ncol = 2, byrow = TRUE)
    }
    
    if (missing(sigma))
      sigma <- min(bw.nrd(coords[[1]], na.rm = TRUE), bw.nrd(coords[[2]], na.rm = TRUE))   
    
    tmp <- surface.kernel(coords, data, sigma, nrow, ncol, window, verbose)
  }
  
  # ----------------------------------------------------------------------------
  # STEP 3 Calculate the index
  # ----------------------------------------------------------------------------
  v <- .decomp(tmp$data)
  SL <- v - .decompL(tmp$data)
  SQ <- 1 / ncol(tmp$data)
  SC <- v - (SL + SQ)
  
  SegDecomp(d = c(SL, SC, SQ), tmp$coords, tmp$data, st_crs(crs))
}
