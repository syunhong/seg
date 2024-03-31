# ------------------------------------------------------------------------------
# Internal function 'chksegdata'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2024-03-30
# ------------------------------------------------------------------------------
chksegdata <- function(x, data) {

  begTime <- Sys.time(); fn <- match.call()[[1]]
  proj4string <- as.character(NA)
  
  # ----------------------------------------------------------------------------
  #
  # STEP 1. CHECK 'x'
  #
  # ----------------------------------------------------------------------------
  # (1) If 'x' is an object of class "sf", then do the following:
  # ----------------------------------------------------------------------------
  if (inherits(x, "sf")) {   
    message(fn, ": 'x' is an object of class \"sf\"")
    coords <- try(x |> st_geometry() |> st_centroid(), silent = TRUE)
    if (inherits(coords, "try-error"))
      stop("failed to extract coordinates from 'x'", call. = FALSE)
    coords <- do.call(rbind, coords)
    message(fn, ": ", nrow(coords), " coordinates extracted from 'x'")
    
    if (missing(data)) {
      data <- try(st_drop_geometry(x), silent = TRUE) 
      if (inherits(data, "try-error"))
        stop("'data' is missing, with no default", call. = FALSE)
      message(fn, ": 'data' is missing, use the one attached to 'x'")
    }
    
    data <- try(as.matrix(data), silent = TRUE)
    if (inherits(data, "try-error"))
      stop("failed to coerce 'data' to matrix", call. = FALSE)

    message(fn, ": check if 'data' has any NA values")
    removeNA <- which(apply(data, 1, function(z) any(is.na(z))))
    if (length(removeNA) > 0) {
      data <- data[-removeNA,]
      coords <- coords[-removeNA,]
      message(fn, ": ", length(removeNA), "NA(s) removed")
    }
    
    message(fn, ": retrieves projection information from 'x'")
    proj4string <- st_crs(x)$proj4string
  }
  
  # ----------------------------------------------------------------------------
  # (2) If 'x' is an object of class "ppp":
  # ----------------------------------------------------------------------------
  else if (is(x, "ppp")) {
    message(fn, ": 'x' is an object of class \"ppp\"")
    coords <- cbind(x = x$x, y = x$y)
    if (missing(data)) {
      if (is.null(x$marks)) {
        stop("'data' is missing, with no default", call. = FALSE)
      } else {
        data <- as.matrix(x$marks)
        message(fn, ": 'data' is missing, use the one attached to 'x'")
      }
    } else {
      data <- as.matrix(data)
    }
  }
  
  # ----------------------------------------------------------------------------
  # (3) If 'x' is a n * 2 matrix or data frame object:
  # ----------------------------------------------------------------------------
  else if (is.matrix(x) || is.data.frame(x)) {
    message(fn, ": 'x' is an object of class \"matrix\" or \"data.frame\"")      
    coords <- as.matrix(x)
    if (ncol(coords) != 2 || !is.numeric(coords))
      stop("'x' must be a numeric matrix with two columns", call. = FALSE)
    if (missing(data))
      stop("'data' is missing, with no default", call. = FALSE)
    else
      data <- as.matrix(data)
  }
  
  # ----------------------------------------------------------------------------
  # (4) If 'x' is not one of the supporting classes:
  # ----------------------------------------------------------------------------
  else
    stop("invalid object 'x'", call. = FALSE)
  
  # ----------------------------------------------------------------------------
  #
  # STEP 2. CHECK 'data'
  #
  # ----------------------------------------------------------------------------
  if (ncol(data) < 2 || !is.numeric(data))
    stop("'data' must be a numeric matrix with at least two columns", 
         call. = FALSE)
  else if (nrow(data) != nrow(coords))
    stop("'data' must have the same number of rows as 'x'", call. = FALSE)

  tt <- as.numeric(difftime(Sys.time(), begTime, units = "sec"))
  message(fn, ": done! [", tt, " seconds]")
    
  colnames(coords) <- c("x", "y")
  list(coords = coords, data = data, proj4string = proj4string)
}
