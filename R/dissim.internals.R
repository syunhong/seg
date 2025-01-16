.spatial_adj <- function(data, nb) {
  
  if (!is.matrix(nb))
    stop("'nb' must be a matrix", call. = FALSE)
  else if (nrow(nb) != ncol(nb))
    stop("'nb' must be a square matrix", call. = FALSE)
  else if (nrow(nb) != nrow(data))
    stop("nrow(nb) must match nrow(data)", call. = FALSE)
  
  if (sum(nb) != 1)
    warning("the sum of all elements in 'nb' does not equal 1", call. = FALSE)
  
  rowsum <- apply(data, 1, sum)
  removeID <- which(rowsum == 0)
  removeL <- length(removeID)
  if (removeL > 0) {
    warning("remove ", removeL, " rows with no population", call. = FALSE)
    rowsum <- rowsum[-removeID]
    data <- data[-removeID,]
    nb <- nb[-removeID, -removeID]
  }
  
  # Black proportions in census tracts
  z <- data[,1] / rowsum
  # Additional spatial component value
  spstr <- 0
  nbvec <- as.vector(nb)
  INDEX <- which(nbvec != 0)
  for (i in 1:length(INDEX)) {
    rowID <- INDEX[i] %% nrow(nb)
    colID <- INDEX[i] %/% nrow(nb)
    if (rowID == 0)
      rowID <- nrow(nb)
    else
      colID <- colID + 1
    spstr <- spstr + (abs(z[colID] - z[rowID]) * nbvec[INDEX[i]])
  }
  as.vector(spstr)
}


# ******************************************************************************
# 
# .use_contiguity()
#
# ******************************************************************************
.use_contiguity <- function(x, data, queen, verbose) {
  speffect <- NA
  if (requireNamespace("spdep", quietly = TRUE)) {
    if (verbose) {
      message("library 'spdep' appears to be available")
      message("attempting to calculate Morrill's D(adj)")
    }
    
    grd.nb <- spdep::poly2nb(x, queen = queen) |> spdep::nb2mat(style = "B")
    grd.nb <- grd.nb / sum(grd.nb)
    speffect <- .spatial_adj(data, grd.nb)
  } 
  
  else if (verbose) {
    message("failed to load 'spdep'")
  }
  
  speffect
}

# ******************************************************************************
# 
# .use_common_boundary()
#
# ******************************************************************************
.use_common_boundary <- function(x, data, verbose) {
  
  # Initialise the output vector
  result <- rep(NA, 2)
  
  # Check if 'terra' is available on user's computer
  if (requireNamespace("terra", quietly = TRUE)) {
    if (verbose) {
      message("'terra' available")
      message("calculate Wong's D(w) and D(s)...")
    }
  } else {
    if (verbose) {
      message("'terra' not available")
      message("cannot calculate Wong's D(w) and D(s)...")
    }
    
    return(result)
  }
  
  common_borders <- try(x |> terra::vect() |> terra::sharedPaths() |> st_as_sf(), 
                        silent = TRUE)
  if (inherits(common_borders, "try-error"))
    stop("failed to find common boundaries in 'x'", call. = FALSE)
  common_lengths <- st_length(common_borders)
  
  len_mat <- matrix(0, nrow = nrow(x), ncol = nrow(x))
  
  rowID <- unique(common_borders$id1)
  for (i in 1:length(rowID)) {
    INDEX <- (common_borders$id1 == rowID[i])
    colID <- common_borders$id2[INDEX]
    sl <- common_lengths[INDEX]
    len_mat[rowID[i], colID] <- sl
    len_mat[colID, rowID[i]] <- sl
  }
  
  len_mat <- len_mat / sum(len_mat)  
  result[1] <- .spatial_adj(data, len_mat)
    
  
  A <- try(st_area(x), silent = TRUE)
  if (inherits(A, "try-error"))
    stop("failed to compute areas of the polygons in 'x'", call. = FALSE)

  P <- apply(len_mat, 1, sum)
  PAR <- P/A
  maxPAR <- max(PAR)
    
  PAR.mat <- matrix(NA, nrow = length(PAR), ncol = length(PAR))
  for (i in 1:length(PAR)) {
    for (j in 1:length(PAR))
      PAR.mat[i,j] <- ((PAR[i] + PAR[j])/2) / maxPAR
  }
    
  result[2] <- .spatial_adj(data, len_mat * PAR.mat)
  result
}



# .use.spgrass6 <- function(x, data, wVECT.args, v2n.args, verbose) {
#   speffect <- rep(NA, 2)
#   if (requireNamespace("spgrass6", quietly = TRUE) & 
#       requireNamespace("rgdal", quietly = TRUE) & 
#       requireNamespace("spdep", quietly = TRUE)) {
#     if (verbose) {
#       message("library 'spgrass6' and 'rgdal' appear to be available")
#       message("attempting to calculate Wong's D(w) and D(s)")
#     }
#     
#     if (!("SpatialPolygonsDataFrame" %in% is(x)))
#       x <- SpatialPolygonsDataFrame(x, as.data.frame(data))
# 
#     if (missing(wVECT.args)) {
#       wVECT.args <- list(SDF = x, vname = "tmp")
#     } else {
#       if (is.null(wVECT.args$SDF))
#         wVECT.args$SDF <- x
#       if (is.null(wVECT.args$vname))
#         wVECT.args$vname <- "tmp"
#     }
#     do.call(spgrass6::writeVECT6, wVECT.args)
#     
#     if (missing(v2n.args)) {
#       v2n.args <- list(vname = wVECT.args$vname)
#     } else {
#       if (is.null(v2n.args$vname))
#       v2n.args$vname <- wVECT.args$vname
#     }
#     sl <- do.call(spgrass6::vect2neigh, v2n.args)
#     sl.mat <- spdep::listw2mat(spdep::sn2listw(sl))
#     sl.mat <- sl.mat / sum(sl.mat)  
#     speffect[1] <- .d.adjust(data, sl.mat)
#     
#     A <- unlist(lapply(slot(x, "polygons"), function(z) slot(z, "area")))
#     P <- attr(sl, "total") - attr(sl, "external")
#     PAR <- P/A
#     maxPAR <- max(PAR)
#     
#     PAR.mat <- matrix(NA, nrow = length(PAR), ncol = length(PAR))
#     for (i in 1:length(PAR)) {
#       for (j in 1:length(PAR))
#         PAR.mat[i,j] <- ((PAR[i] + PAR[j])/2) / maxPAR
#     }
#     
#     speffect[2] <- .d.adjust(data, sl.mat * PAR.mat)   
#   } else if (verbose) {
#     message("failed to load 'spgrass6' or 'rgdal'")
#   }
#   
#   speffect
# }

