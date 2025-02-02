# ------------------------------------------------------------------------------
# Internal function 'surface.equal'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
surface.equal <- function(x, data, nrow, ncol, verbose) {
  
  if (verbose){
    begTime <- Sys.time(); fn <- match.call()[[1]]
    message(fn, ": redistributing population data ...")
  }
  
  # Ensure x is an 'sf' object
  if (!inherits(x, "sf"))
    stop("'x' must be an 'sf' object to use the \"equal\" smoothing option", call. = FALSE)
  
  # Get bounding box
  xy <- st_bbox(x)
  xmn <- xy$xmin; xmx <- xy$xmax
  ymn <- xy$ymin; ymx <- xy$ymax
  
  # Create a nrow * ncol grid
  xx <- seq(xmn, xmx, length.out = ncol)
  yy <- seq(ymn, ymx, length.out = nrow)
  coords <- expand.grid(xx, yy)
  colnames(coords) <- c("x", "y")
  
  # Convert grid points to sf format
  spcoords <- st_as_sf(coords, coords = c("x", "y"), crs = st_crs(x))
  
  # Identify which polygons the points belong to
  polygonIDs <- st_intersects(spcoords, x, sparse = FALSE)
  polygonIDs <- apply(polygonIDs, 1, function(z) ifelse(any(z), which(z)[1], NA))
  
  # Remove points that are outside the polygons
  valid <- !is.na(polygonIDs)
  coords <- coords[valid, ]
  polygonIDs <- polygonIDs[valid]
  
  # Redistribute population within each polygon
  cellPerPolygon <- table(polygonIDs)
  values <- matrix(NA, nrow = nrow(coords), ncol = ncol(data))
  
  for (i in seq_len(ncol(data))) {
    counts <- data[polygonIDs, i]
    counts <- counts / cellPerPolygon[as.character(polygonIDs)]
    values[, i] <- counts
  }
  
  if (verbose){
    tt <- as.numeric(difftime(Sys.time(), begTime, units = "sec"))
    message(fn, ": done! [", tt, " seconds]")
  }
  
  colnames(coords) <- c("x", "y")
  colnames(values) <- colnames(data)
  list(coords = coords, data = values, id = polygonIDs)
}
