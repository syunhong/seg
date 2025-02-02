# ------------------------------------------------------------------------------
# Methods for class 'SegDecomp'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------

setAs("SegDecomp", "sf", 
      function(from) {
        validObject(from)
        st_as_sf(data.frame(from@data, geometry = st_sfc(st_multipoint(from@coords))), crs = st_crs(from@proj4string))
      })

setAs("SegDecomp", "SpatialPointsDataFrame", 
      function(from) {
        validObject(from)
        st_as_sf(data.frame(from@data, geometry = st_sfc(st_multipoint(from@coords))), crs = st_crs(from@proj4string))
      })

setAs("SegDecomp", "SpatialPixelsDataFrame", 
      function(from) {
        validObject(from)
        st_as_sf(data.frame(from@data, geometry = st_sfc(st_multipoint(from@coords))), crs = st_crs(from@proj4string))
      })

setAs("SegDecomp", "vector", 
      function(from) {
        validObject(from)
        as.vector(from@d)
      })

as.vector.SegDecomp <- function(x, ...) {
  validObject(x)
  as.vector(x@d)
}

# ------------------------------------------------------------------------------
# Printing
# ------------------------------------------------------------------------------
setMethod("show", signature(object = "SegDecomp"), function(object) {
  validObject(object)
  print(sum(object@d))
  cat("\nSubcomponents:\n")
  tmp <- t(data.frame(object@d))
  rownames(tmp) <- ""
  colnames(tmp) <- c("Location", "Composition", "Qualitative")
  print(tmp)
})

print.SegDecomp <- function(x, digits = getOption("digits"), ...) {
  validObject(x)
  print(sum(x@d), digits = digits)
  cat("\nSubcomponents:\n")
  tmp <- t(data.frame(x@d))
  rownames(tmp) <- ""
  colnames(tmp) <- c("Location", "Composition", "Qualitative")
  print(tmp, digits = digits)
}

# ------------------------------------------------------------------------------
# Plotting (using ggplot2 instead of spplot)
# ------------------------------------------------------------------------------
setMethod("spplot", signature(obj = "SegDecomp"), function(obj, ...) {
  validObject(obj)
  
  # Convert to sf object
  sfObj <- try(as(obj, "sf"), silent = TRUE)
  if (inherits(sfObj, "try-error")) {
    stop("failed to convert 'obj' to sf object", call. = FALSE)
  }
  
  # Use ggplot2 to plot (alternative: tmap)
  library(ggplot2)
  ggplot(sfObj) +
    geom_sf(aes(fill = obj@d), color = "black") +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(title = "Segregation Decomposition", fill = "D Values")
})