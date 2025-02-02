# ------------------------------------------------------------------------------
# Class 'SegSpatial'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setClass(Class = "SegSpatial",
         slots = c(d = "numeric", r = "numeric", h = "numeric", p = "matrix"),
         contains = "SegLocal")

setValidity(Class = "SegSpatial", 
            method = function(object) {
              if (!is.matrix(object@p) || nrow(object@p) != ncol(object@p))
                return("Error: 'p' must be a square numeric matrix.")
              TRUE
            })

SegSpatial <- function(d, r, h, p, coords, data, env, proj4string = CRS(as.character(NA))) {
  
  # Validate 'd', 'r', and 'h'
  if (!is.numeric(d) || length(d) != 1)
    stop("Error: 'd' must be a numeric scalar.", call. = FALSE)
  if (!is.numeric(r) || length(r) != 1)
    stop("Error: 'r' must be a numeric scalar.", call. = FALSE)
  if (!is.numeric(h) || length(h) != 1)
    stop("Error: 'h' must be a numeric scalar.", call. = FALSE)
  
  # Validate 'p'
  if (!is.matrix(p) || nrow(p) != ncol(p))
    stop("Error: 'p' must be a square numeric matrix.", call. = FALSE)
  
  # Validate 'coords'
  if (!is.matrix(coords) || ncol(coords) != 2 || !is.numeric(coords))
    stop("Error: 'coords' must be a numeric matrix with two columns (x, y).", call. = FALSE)
  
  # Validate 'data'
  if (!is.matrix(data) || ncol(data) < 2 || !is.numeric(data))
    stop("Error: 'data' must be a numeric matrix with at least two columns.", call. = FALSE)
  
  # Validate 'env'
  if (!is.matrix(env) || any(dim(env) != dim(data)))
    stop("Error: 'env' must have the same dimensions as 'data'.", call. = FALSE)
  
  # Validate 'proj4string'
  if (!inherits(proj4string, "CRS"))
    stop("Error: 'proj4string' must be a valid CRS object.", call. = FALSE)
  
  new("SegSpatial", d = d, r = r, h = h, p = p, coords = coords, data = data, 
      env = env, proj4string = proj4string)
}