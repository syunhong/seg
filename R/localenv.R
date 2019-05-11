# ------------------------------------------------------------------------------
# Function 'localenv'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
localenv <- function(x, data, power = 2, useExp = TRUE, scale = FALSE,
                     maxdist, sprel, tol = .Machine$double.eps) {
  
  tmp <- suppressMessages(chksegdata(x, data))
  coords <- tmp$coords; data <- tmp$data; proj4string <- tmp$proj4string
  
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
      stop("'maxdist' must be specified when 'scale' is set to TRUE", 
           call. = FALSE)
  }
    
  else if (!is.numeric(maxdist))
    stop("'maxdist' must be numeric", call. = FALSE)
  else if (maxdist < 0)
    stop("'maxdist' must be greater than or equal to 0", call. = FALSE)
  
  if (missing(sprel)) 
    sprel <- coords
  else if (class(sprel) != "nb" && class(sprel) != "dist")
    stop("invalid object 'sprel'", call. = FALSE)

  env <- localenv.get(sprel, data, power, useExp, scale, maxdist, tol)

  SegLocal(coords, data, env, CRS(proj4string))
}
