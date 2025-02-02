# ------------------------------------------------------------------------------
# Methods for class 'SegLocal'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Coercion methods
# ------------------------------------------------------------------------------
setAs("SegLocal", "sf", 
      function(from) {
        validObject(from)
        st_as_sf(data.frame(from@data, geometry = st_sfc(st_multipoint(from@coords))), crs = st_crs(from@proj4string))
      })

setAs("SegLocal", "SpatialPointsDataFrame", 
      function(from) {
        validObject(from)
        st_as_sf(data.frame(from@data, geometry = st_sfc(st_multipoint(from@coords))), crs = st_crs(from@proj4string))
      })

setAs("SegLocal", "SpatialPixelsDataFrame", 
      function(from) {
        validObject(from)
        st_as_sf(data.frame(from@data, geometry = st_sfc(st_multipoint(from@coords))), crs = st_crs(from@proj4string))
      })

setAs("SpatialPointsDataFrame", "SegLocal", 
      function(from) {
        SegLocal(coords = st_coordinates(from), data = from@data, env = from@data, 
                 proj4string = st_crs(from))
      })

setAs("SpatialPolygonsDataFrame", "SegLocal", 
      function(from) {
        SegLocal(coords = st_coordinates(from), data = from@data, 
                 env = from@data, proj4string = st_crs(from))
      })

# ------------------------------------------------------------------------------
# Printing
# ------------------------------------------------------------------------------
setMethod("show", signature(object = "SegLocal"), function(object) {
  validObject(object)
  cat("Class                 :", class(object), "\n")
  cat("Number of data points :", nrow(object@coords), "\n")
  cat("Number of data columns:", ncol(object@data), "\n")
  cat("Projection            :", st_crs(object@proj4string)$wkt, "\n")
  cat("Slot names            :", slotNames(object), "\n")
})

print.SegLocal <- function(x, ...) {
  validObject(x)
  cat("Class                 :", class(x), "\n")
  cat("Number of data points :", nrow(x@coords), "\n")
  cat("Number of data columns:", ncol(x@data), "\n")
  cat("Projection            :", st_crs(x@proj4string)$wkt, "\n")
  cat("Slot names            :", slotNames(x), "\n")
}

# ------------------------------------------------------------------------------
# Plotting (using ggplot2 instead of spplot)
# ------------------------------------------------------------------------------
plot.SegLocal <- function(x, which.col = 1:ncol(x@env), main = NULL, ...) {
  validObject(x)
  library(ggplot2)
  
  df <- as.data.frame(cbind(x@coords, x@env))
  colnames(df) <- c("x", "y", colnames(x@env))
  
  if (is.null(main)) main <- paste("Data", which.col)
  
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = df[, which.col]), color = "blue", alpha = 0.6) +
    scale_size_continuous(range = c(1, 6)) +
    theme_minimal() +
    labs(title = main, size = "Value")
}

points.SegLocal <- function(x, which.col = 1, ...) {
  validObject(x)
  library(ggplot2)
  
  df <- as.data.frame(cbind(x@coords, x@env))
  colnames(df) <- c("x", "y", colnames(x@env))
  
  if (length(which.col) > 1) warning("'which.col' has a length > 1", call. = FALSE)
  
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = df[, which.col]), color = "red", alpha = 0.6) +
    scale_size_continuous(range = c(1, 6)) +
    theme_minimal() +
    labs(title = paste("Variable:", colnames(x@env)[which.col]), size = "Value")
}

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
summary.SegLocal <- function(object, ...) {
  validObject(object)
  cat("An object of class \"", class(object), "\"\n", sep = "")
  cat("Coordinates:\n")
  tmp <- apply(object@coords, 2, range)
  print(tmp)
  
  if (is.na(st_crs(object@proj4string)$wkt)) {
    cat("Is projected: FALSE\n")
  } else {
    cat("Is projected: TRUE\n")
    cat("Projection  : ", st_crs(object@proj4string)$wkt, "\n")
  }
  
  cat("\nData values (%):\n")
  tmp <- t(apply(object@data, 1, function(z) z / sum(z))) * 100
  print(apply(tmp, 2, summary, ...))
  
  cat("\nLocal environment composition (%):\n")
  tmp <- t(apply(object@env, 1, function(z) z / sum(z))) * 100
  print(apply(tmp, 2, summary, ...))
}

# ------------------------------------------------------------------------------
# Updating
# ------------------------------------------------------------------------------
update.SegLocal <- function(object, coords, data, env, proj4string, ...) {
  validObject(object)
  
  if (missing(coords)) coords <- object@coords
  if (missing(data)) data <- object@data
  if (missing(env)) env <- object@env
  if (missing(proj4string)) proj4string <- object@proj4string
  
  SegLocal(coords, data, env, proj4string)
}

# Methods that are not so useful (removed on 23 December 2013) ...
#
# as.list.SegLocal <- function(x, ...) {
#   validObject(x)
#   list(coords = x@coords, data = x@data, env = x@env, 
#        proj4string = x@proj4string)
# }
# 
# setAs("list", "SegLocal",
#       function(from) {
#         if (is.null(from$proj4string))
#           SegLocal(coords = from$coords, data = from$data, env = from$env)
#         else
#           SegLocal(coords = from$coords, data = from$data, env = from$env, 
#                    proj4string = from$proj4string)
#       })
# setAs("SegLocal", "list", 
#       function(from) {
#         validObject(from)
#         list(coords = from@coords, data = from@data, env = from@env, 
#              proj4string = from@proj4string)
#       })
#
# "[[.SegLocal" <- function(i, ...) {
#   validObject(x)
#   slotnames <- slotNames(x)
# 
#   if (is.numeric(i)) {
#     i <- as.integer(i)
#     if (i > length(slotnames))
#       chosen <- NULL
#     else {
#       chosen <- slotnames[i]
#       chosen <- paste("x@", chosen, sep = "")
#       chosen <- eval(parse(text = chosen))
#     }
#   }
#   
#   else if (is.character(i)) {
#     chosen <- paste("x@", i, sep = "")
#     chosen <- eval(parse(text = chosen))
#   }
#   
#   else {
#     chosen <- NULL
#   }
# }
