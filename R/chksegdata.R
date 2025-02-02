# ------------------------------------------------------------------------------
# Internal function 'chksegdata'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2025-02-02
# ------------------------------------------------------------------------------
chksegdata <- function(x, data) {
  
  begTime <- Sys.time(); fn <- match.call()[[1]]
  proj4string <- as.character(NA)
  
  # ----------------------------------------------------------------------------
  # STEP 1. CHECK 'x'
  # ----------------------------------------------------------------------------
  if (inherits(x, "sf")) {   
    message(fn, ": 'x' is an object of class \"sf\"")
    
    # Convert MULTIPOLYGON/invalid geometries before extracting centroids
    coords <- try(st_coordinates(st_centroid(st_make_valid(st_geometry(x)))), silent = TRUE)
    if (inherits(coords, "try-error"))
      stop("failed to extract coordinates from 'x'", call. = FALSE)
    
    message(fn, ": ", nrow(coords), " coordinates extracted from 'x'")
    
    if (missing(data)) {
      data <- try(st_drop_geometry(x), silent = TRUE) 
      if (inherits(data, "try-error"))
        stop("'data' is missing, with no default", call. = FALSE)
      message(fn, ": 'data' is missing, using attached data in 'x'")
    }
    
    data <- try(as.matrix(data), silent = TRUE)
    if (inherits(data, "try-error"))
      stop("failed to coerce 'data' to matrix", call. = FALSE)
    
    # Handle NA values in 'data'
    removeNA <- which(apply(data, 1, function(z) any(is.na(z))))
    if (length(removeNA) > 0) {
      warning(fn, ": ", length(removeNA), " row(s) contain NA values.")
      data[removeNA,] <- 0  # Replace NA values with 0 instead of removing
    }
    
    message(fn, ": retrieving projection information from 'x'")
    proj4string <- st_crs(x)$wkt  # Use WKT format
  }
  
  # ----------------------------------------------------------------------------
  # If 'x' is a spatial point pattern object (ppp)
  # ----------------------------------------------------------------------------
  else if (inherits(x, "ppp")) {
    message(fn, ": 'x' is an object of class \"ppp\"")
    coords <- cbind(x = x$x, y = x$y)
    
    if (missing(data)) {
      if (is.null(x$marks)) {
        stop("'data' is missing, with no default", call. = FALSE)
      } else {
        data <- as.matrix(x$marks)
        message(fn, ": 'data' is missing, using marks attached to 'x'")
      }
    } else {
      data <- as.matrix(data)
    }
  }
  
  # ----------------------------------------------------------------------------
  # If 'x' is a numeric matrix or data frame with coordinates
  # ----------------------------------------------------------------------------
  else if (is.matrix(x) || is.data.frame(x)) {
    message(fn, ": 'x' is an object of class \"matrix\" or \"data.frame\"")      
    coords <- as.matrix(x)
    if (ncol(coords) != 2 || !is.numeric(coords))
      stop("'x' must be a numeric matrix with two columns (x, y)", call. = FALSE)
    if (missing(data))
      stop("'data' is missing, with no default", call. = FALSE)
    else
      data <- as.matrix(data)
  }
  
  # ----------------------------------------------------------------------------
  # If 'x' is not one of the supported classes
  # ----------------------------------------------------------------------------
  else {
    stop("invalid object 'x'", call. = FALSE)
  }
  
  # ----------------------------------------------------------------------------
  # STEP 2. CHECK 'data'
  # ----------------------------------------------------------------------------
  if (ncol(data) < 2 || !is.numeric(data))
    stop("'data' must be a numeric matrix with at least two columns", call. = FALSE)
  else if (nrow(data) != nrow(coords))
    stop("'data' must have the same number of rows as 'x'", call. = FALSE)
  
  tt <- as.numeric(difftime(Sys.time(), begTime, units = "sec"))
  message(fn, ": done! [", tt, " seconds]")
  
  colnames(coords) <- c("x", "y")
  list(coords = coords, data = data, proj4string = proj4string)
}