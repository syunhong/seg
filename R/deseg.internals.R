# ------------------------------------------------------------------------------
# Internal functions used by 'deseg'
#
# Author: Seong-Yun Hong <syhong@khu.ac.kr>
# Last update: 2025-02-02
# ------------------------------------------------------------------------------
.decomp <- function(data) {
  
  if (inherits(data, "sf")) {
    data <- st_drop_geometry(data)
  }
  
  dx <- rowSums(data)
  removeID <- which(dx == 0)
  removeL <- length(removeID)
  if (removeL > 0) {
    warning("remove ", removeL, " rows with no population", call. = FALSE)
    dx <- dx[-removeID]
    data <- data[-removeID, ]
  }  
  
  sx <- data / dx
  sx <- rowSums(sx * sx)
  
  sum(dx * sx) / sum(dx)
}

.decompL <- function(data) {
  
  if (inherits(data, "sf")) {
    data <- st_drop_geometry(data)
  }
  
  groupsize <- colSums(data)
  numpoints <- nrow(data)
  
  dataL <- matrix(groupsize / numpoints, nrow = numpoints, ncol = length(groupsize), byrow = TRUE)
  
  .decomp(dataL)
}

.decompC <- function(data) {
  
  if (inherits(data, "sf")) {
    data <- st_drop_geometry(data)
  }
  
  tmp <- sum(data, na.rm = TRUE) / (nrow(data) * ncol(data))
  dataC <- matrix(tmp, nrow = nrow(data), ncol = ncol(data))
  
  .decomp(dataC)
}
