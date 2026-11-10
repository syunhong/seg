\name{localenv}
\alias{localenv}
\title{Population Composition of the Local Environment}
\description{
Computes the population composition of the local environment of each data point, as a distance-weighted average of the population at all points. The result is used by \code{\link{spatseg}} and \code{\link{spseg}}.
}
\usage{
localenv(x, data, power = 2, useExp = TRUE, scale = FALSE, maxdist, sprel,
         tol = .Machine$double.eps)
}
\arguments{
  \item{x}{a numeric matrix or data frame with coordinates (each row is a point), an \code{sf} or \code{sfc} object with point or polygon geometries, or an object of class \code{ppp}. For polygons, the centroids (of the largest part) are used.}
  \item{data}{a numeric matrix or data frame with at least two columns, one for each population group. This can be missing if \sQuote{x} has the data attached.}
  \item{power}{a numeric value that determines how quickly the weight declines with distance. See \sQuote{Details}.}
  \item{useExp}{logical. If TRUE (default), a negative exponential function of distance is used; otherwise an inverse distance function.}
  \item{scale}{logical. If TRUE, the weights are scaled by \sQuote{maxdist}. See \sQuote{Details}.}
  \item{maxdist}{an optional numeric value giving the maximum search radius. Points further away get a weight of zero. Required if \sQuote{scale} is TRUE.}
  \item{sprel}{an optional object of class \code{dist} or \code{nb} describing the spatial relationships between the points. See \sQuote{Details}.}
  \item{tol}{a small positive value added to distances in the inverse distance function to avoid division by zero.}
}
\details{
For points \eqn{i} and \eqn{j} at distance \eqn{d}, the weight is \eqn{\exp(-p d)}{exp(-p * d)} if \sQuote{useExp} is TRUE and \eqn{1/(d + tol)^p}{1/(d + tol)^p} otherwise, where \eqn{p} is \sQuote{power}. The coordinates are treated as planar, so data with geographic coordinates should be projected first (e.g., with \code{sf::st_transform}). Note that the appropriate value of \sQuote{power} depends on the units of the coordinates.

If \sQuote{scale} is TRUE, the weight is \eqn{(1 - (d/r)^p)^p}{(1 - (d/r)^p)^p} (a biweight kernel) when \sQuote{useExp} is FALSE, and \eqn{(\exp(-p d/r) - \exp(-p)) / (1 - \exp(-p))}{(exp(-p d/r) - exp(-p)) / (1 - exp(-p))} (a scaled and normalised exponential decay) when \sQuote{useExp} is TRUE, where \eqn{r} is \sQuote{maxdist}.

If \sQuote{sprel} is a \code{dist} object, its distances are used in place of the Euclidean distances between the coordinates (\sQuote{scale} is ignored). If it is an \code{nb} object (e.g., from \code{spdep::poly2nb}), the local environment of a unit is the average population of its neighbours, i.e., row-standardised weights. Every unit must have at least one neighbour.
}
\value{
An object of class \code{\link{SegLocal}}.
}
\author{
Seong-Yun Hong; the scaled kernels were contributed by Benjamin Jarvis.
}
\references{
Reardon, S. F. and O'Sullivan, D. (2004) Measures of spatial segregation. \emph{Sociological Methodology}, \bold{34}, 121-162.
}
\seealso{
\code{\link{SegLocal}}, \code{\link{spseg}}
}
\examples{
library(sf)
data(segdata)
grd <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10)),
                    n = 10)
grd <- grd[as.vector(matrix(1:100, 10)[, 10:1])]

# negative exponential function of distance
env1 <- localenv(grd, segdata[, 1:2])
env1

# inverse distance, and equal weights (power = 0)
env2 <- localenv(grd, segdata[, 1:2], useExp = FALSE)
env3 <- localenv(grd, segdata[, 1:2], useExp = FALSE, power = 0)

# map the composition of the local environments
plot(st_sf(prop = env1$env[, 1] / rowSums(env1$env), geometry = grd),
     main = "Group 1 share of the local environment")
}
