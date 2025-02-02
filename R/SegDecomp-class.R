# ------------------------------------------------------------------------------
# Class 'SegDecomp'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setClass(Class = "SegDecomp", 
         slots = c(d = "numeric", coords = "matrix", data = "matrix",
                   proj4string = "CRS"))

setValidity(Class = "SegDecomp", 
            method = function(object) { 
              if (length(object@d) != 3)
                return("Error: length of 'd' must be three.")
              if (any(object@d < 0))
                return("Error: 'd' values must be positive.")
              if (sum(object@d) > 1)
                return("Error: sum of 'd' must be between 0 and 1.")
              if (!is.matrix(object@coords) || ncol(object@coords) != 2 || !is.numeric(object@coords))
                return("Error: 'coords' must be a numeric matrix with two columns (x, y).")
              if (!is.matrix(object@data) || ncol(object@data) < 2 || !is.numeric(object@data))
                return("Error: 'data' must be a numeric matrix with at least two columns.")
              if (nrow(object@data) != nrow(object@coords))
                return("Error: 'data' must have the same number of rows as 'coords'.")
              if (!inherits(object@proj4string, "CRS"))
                return("Error: 'proj4string' must be a valid CRS object.")
              TRUE
            })

SegDecomp <- function(d, coords, data, proj4string = CRS(as.character(NA))) {
  if (!is.numeric(d) || length(d) != 3)
    stop("Error: 'd' must be a numeric vector of length 3.")
  if (!is.matrix(coords) || ncol(coords) != 2 || !is.numeric(coords))
    stop("Error: 'coords' must be a numeric matrix with two columns.")
  if (!is.matrix(data) || ncol(data) < 2 || !is.numeric(data))
    stop("Error: 'data' must be a numeric matrix with at least two columns.")
  if (nrow(data) != nrow(coords))
    stop("Error: 'data' and 'coords' must have the same number of rows.")
  if (!inherits(proj4string, "CRS"))
    stop("Error: 'proj4string' must be a valid CRS object.")
  
  new("SegDecomp", d = d, coords = coords, data = data, proj4string = proj4string)
}