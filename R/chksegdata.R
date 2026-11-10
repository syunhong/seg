# ------------------------------------------------------------------------------
# Internal function 'chksegdata'
#
# Standardises the input of the user-level functions. Returns the point
# coordinates, the population data, the CRS, and (if 'x' was an sf object) the
# geometry, all with incomplete rows removed.
# ------------------------------------------------------------------------------
chksegdata <- function(x, data) {

  geom <- NULL
  crs <- sf::NA_crs_

  # 'sp' objects are still accepted if the user has them, via sf
  if (inherits(x, "Spatial"))
    x <- sf::st_as_sf(x)

  if (inherits(x, c("sf", "sfc"))) {
    geom <- sf::st_geometry(x)
    crs <- sf::st_crs(geom)
    if (missing(data)) {
      if (!inherits(x, "sf"))
        stop("'data' is missing, with no default", call. = FALSE)
      data <- sf::st_drop_geometry(x)
    }
    coords <- .sfc2coords(geom)
  }

  else if (inherits(x, "ppp")) {
    coords <- cbind(x$x, x$y)
    if (missing(data)) {
      if (is.null(x$marks))
        stop("'data' is missing, with no default", call. = FALSE)
      data <- x$marks
    }
  }

  else if (is.matrix(x) || is.data.frame(x)) {
    coords <- as.matrix(x)
    if (ncol(coords) != 2 || !is.numeric(coords))
      stop("'x' must be a numeric matrix with two columns", call. = FALSE)
    if (missing(data))
      stop("'data' is missing, with no default", call. = FALSE)
  }

  else {
    stop("invalid object 'x'", call. = FALSE)
  }

  data <- as.matrix(data)
  if (ncol(data) < 2 || !is.numeric(data))
    stop("'data' must be a numeric matrix with at least two columns",
         call. = FALSE)
  if (nrow(data) != nrow(coords))
    stop("'data' must have the same number of rows as 'x'", call. = FALSE)

  keep <- which(stats::complete.cases(data))
  if (length(keep) < nrow(data)) {
    message(nrow(data) - length(keep), " row(s) with NA removed")
    data <- data[keep, , drop = FALSE]
    coords <- coords[keep, , drop = FALSE]
    if (!is.null(geom))
      geom <- geom[keep]
  }

  storage.mode(coords) <- "double"
  colnames(coords) <- c("x", "y")
  rownames(coords) <- NULL

  list(coords = coords, data = data, crs = crs, geom = geom,
       polygons = .is.polygons(geom))
}

# Coordinates of points, or centroids of polygons (of the largest part, as
# the label points used by 'sp' in earlier versions)
.sfc2coords <- function(geom) {
  type <- as.character(sf::st_geometry_type(geom, by_geometry = TRUE))
  if (isTRUE(sf::st_is_longlat(geom)))
    warning("'x' has geographic coordinates; distances are computed as ",
            "if they were planar. Consider sf::st_transform().", call. = FALSE)
  if (all(type == "POINT"))
    xy <- sf::st_coordinates(geom)
  else if (all(type %in% c("POLYGON", "MULTIPOLYGON")))
    xy <- sf::st_coordinates(suppressWarnings(
      sf::st_centroid(geom, of_largest_polygon = TRUE)))
  else
    stop("'x' must contain either points or polygons", call. = FALSE)
  xy[, 1:2, drop = FALSE]
}

.is.polygons <- function(geom) {
  if (is.null(geom))
    return(FALSE)
  type <- as.character(sf::st_geometry_type(geom, by_geometry = TRUE))
  all(type %in% c("POLYGON", "MULTIPOLYGON"))
}

# Index of the polygon that contains each point (first match), or NA
.which.polygon <- function(coords, geom) {
  pts <- sf::st_as_sf(as.data.frame(coords), coords = 1:2,
                      crs = sf::st_crs(geom))
  hits <- sf::st_intersects(pts, geom)
  vapply(hits, function(z) if (length(z)) z[1L] else NA_integer_, 1L)
}
