# ------------------------------------------------------------------------------
# Methods for class 'SegSpatial'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Coercion methods
# ------------------------------------------------------------------------------
setAs("SegSpatial", "sf", 
      function(from) {
        validObject(from)
        st_as_sf(data.frame(from@data, geometry = st_sfc(st_multipoint(from@coords))), crs = st_crs(from@proj4string))
      })

setAs("SegSpatial", "SpatialPointsDataFrame", 
      function(from) {
        validObject(from)
        st_as_sf(data.frame(from@data, geometry = st_sfc(st_multipoint(from@coords))), crs = st_crs(from@proj4string))
      })

setAs("SegSpatial", "SpatialPixelsDataFrame", 
      function(from) {
        validObject(from)
        st_as_sf(data.frame(from@data, geometry = st_sfc(st_multipoint(from@coords))), crs = st_crs(from@proj4string))
      })

setAs("SegSpatial", "list", 
      function(from) {
        validObject(from)
        list(d = from@d, r = from@r, h = from@h, p = from@p)
      })

as.list.SegSpatial <- function(x, ...) {
  validObject(x)
  list(d = x@d, r = x@r, h = x@h, p = x@p)
}

# ------------------------------------------------------------------------------
# Printing
# ------------------------------------------------------------------------------
setMethod("show", signature(object = "SegSpatial"), function(object) {
  validObject(object)
  cat("\n\tReardon and O'Sullivan's spatial segregation measures\n\n")
  
  cat("Dissimilarity (D)     :", ifelse(length(object@d) > 0, round(object@d, 4), "-"), "\n")
  cat("Relative diversity (R):", ifelse(length(object@r) > 0, round(object@r, 4), "-"), "\n")
  cat("Information theory (H):", ifelse(length(object@h) > 0, round(object@h, 4), "-"), "\n")
  
  cat("Exposure/Isolation (P):\n")
  if (length(object@p) > 0) {
    if (is.null(colnames(object@p)))
      colnames(object@p) <- paste("Group", 1:ncol(object@p))
    if (is.null(rownames(object@p)))
      rownames(object@p) <- paste("Group", 1:nrow(object@p))
    print(object@p)
    cat("--\nThe exposure/isolation matrix should be read horizontally.\nRead 'help(spseg)' for more details.\n")
  } else {
    cat("-\n")
  }
})

print.SegSpatial <- function(x, digits = getOption("digits"), ...) {
  validObject(x)
  cat("\n\tReardon and O'Sullivan's spatial segregation measures\n\n")
  
  cat("Dissimilarity (D)     :", ifelse(length(x@d) > 0, round(x@d, digits), "-"), "\n")
  cat("Relative diversity (R):", ifelse(length(x@r) > 0, round(x@r, digits), "-"), "\n")
  cat("Information theory (H):", ifelse(length(x@h) > 0, round(x@h, digits), "-"), "\n")
  
  cat("Exposure/Isolation (P):\n")
  if (length(x@p) > 0) {
    if (is.null(colnames(x@p)))
      colnames(x@p) <- paste("Group", 1:ncol(x@p))
    if (is.null(rownames(x@p)))
      rownames(x@p) <- paste("Group", 1:nrow(x@p))
    print(x@p, digits, ...)
    cat("--\nThe exposure/isolation matrix should be read horizontally.\nRead 'help(spseg)' for more details.\n")
  } else {
    cat("-\n")
  }
}

# ------------------------------------------------------------------------------
# Plotting (using ggplot2 instead of spplot)
# ------------------------------------------------------------------------------
plot.SegSpatial <- function(x, which.col = 1, main = NULL, ...) {
  validObject(x)
  library(ggplot2)
  
  df <- as.data.frame(cbind(x@coords, x@data))
  colnames(df) <- c("x", "y", colnames(x@data))
  
  if (is.null(main)) main <- paste("Variable:", colnames(x@data)[which.col])
  
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = df[, which.col]), color = "blue", alpha = 0.6) +
    scale_size_continuous(range = c(1, 6)) +
    theme_minimal() +
    labs(title = main, size = "Value")
}

# ------------------------------------------------------------------------------
# Utilities
# ------------------------------------------------------------------------------
summary.SegSpatial <- function(object, ...) {
  validObject(object)
  cat("\n\tSummary of Spatial Segregation Measures\n\n")
  
  cat("Dissimilarity Index (D):", round(object@d, 4), "\n")
  cat("Relative Diversity (R) :", round(object@r, 4), "\n")
  cat("Information Theory (H):", round(object@h, 4), "\n")
  
  cat("\nExposure/Isolation Matrix:\n")
  print(object@p)
  
  cat("\nCoordinate Range:\n")
  print(apply(object@coords, 2, range))
  
  cat("\nData Summary (%):\n")
  tmp <- t(apply(object@data, 1, function(z) z / sum(z))) * 100
  print(apply(tmp, 2, summary, ...))
}

update.SegSpatial <- function(object, d, r, h, p, coords, data, env, proj4string, ...) {
  validObject(object)
  
  if (missing(d)) d <- object@d
  if (missing(r)) r <- object@r
  if (missing(h)) h <- object@h
  if (missing(p)) p <- object@p
  if (missing(coords)) coords <- object@coords
  if (missing(data)) data <- object@data
  if (missing(env)) env <- object@env
  if (missing(proj4string)) proj4string <- object@proj4string
  
  SegSpatial(d, r, h, p, coords, data, env, proj4string)
}

# ------------------------------------------------------------------------------
# Utilities
# ------------------------------------------------------------------------------
