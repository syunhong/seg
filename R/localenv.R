# ------------------------------------------------------------------------------
# Function 'localenv'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2025-02-02
# ------------------------------------------------------------------------------
localenv <- function(x, data, power = 2, useExp = TRUE, scale = FALSE,
                     maxdist, sprel, tol = .Machine$double.eps) {
  
  # Process data using chksegdata()
  tmp <- suppressMessages(chksegdata(x, data))
  coords <- tmp$coords
  data <- tmp$data
  proj4string <- tmp$proj4string
  
  # Handle maxdist parameter
  if (missing(maxdist)) {
    # --------------------------------------------------------------------------
    # This part of code is updated on 10 May 2019 to address an issue in the
    # envconstruct.c file.
    #
    # The scaled and normalized exponential decay part (i.e., contribution from
    # Benjamin Jarvis) didn't work properly when the maxdist parameter was not 
    # given by users. 
    #
    # The following if-else statement solves the problem - Thanks CCL!
    # --------------------------------------------------------------------------    
    if (!scale)
      maxdist <- -1
    else
      stop("'maxdist' must be specified when 'scale' is set to TRUE", call. = FALSE)
  } else if (!is.numeric(maxdist) || maxdist < 0) {
    stop("'maxdist' must be a non-negative numeric value", call. = FALSE)
  }
  
  # Handle sprel parameter
  if (missing(sprel)) {
    sprel <- coords
  } else if (!inherits(sprel, "nb") && !inherits(sprel, "dist")) {
    stop("Invalid object 'sprel': must be of class 'nb' or 'dist'", call. = FALSE)
  }
  
  # Compute local environment
  env <- localenv.get(sprel, data, power, useExp, scale, maxdist, tol)
  
  # Return processed data
  SegLocal(coords, data, env, CRS(proj4string))
}
