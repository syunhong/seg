\name{isp}
\alias{isp}
\alias{whiteseg}
\title{White's Spatial Proximity Index}
\description{
Calculates the index of spatial proximity proposed by White (1983).
}
\usage{
isp(x, data, nb, fun, verbose = FALSE, \dots)
whiteseg(x, data, nb, fun, verbose = FALSE, \dots)
}
\arguments{
  \item{x}{a numeric matrix or data frame with coordinates (each row is a point), an \code{sf} or \code{sfc} object with point or polygon geometries, or an object of class \code{ppp}. For polygons, the centroids are used.}
  \item{data}{a numeric matrix or data frame with at least two columns, one for each population group. The number of rows must equal the number of points in \sQuote{x}. This can be missing if \sQuote{x} is an \code{sf} or \code{ppp} object with the data attached.}
  \item{nb}{an optional matrix of distances between the points. If missing, Euclidean distances between the coordinates are used.}
  \item{fun}{an optional function of distance that gives the degree of spatial interaction. The default is \code{function(z) exp(-z)}.}
  \item{verbose}{logical. Currently unused; kept for compatibility.}
  \item{\dots}{optional arguments passed to \code{\link{dist}} when \sQuote{nb} is missing.}
}
\details{
Pairs of points with a distance of zero, including each point paired with itself, are excluded. \code{whiteseg} is deprecated; use \code{isp} instead.
}
\value{
A single numeric value. A value of one indicates no spatial clustering of the groups; values greater than one indicate that members of each group live closer to one another than to members of the other groups.
}
\author{
Seong-Yun Hong
}
\references{
White, M. J. (1983) The measurement of spatial segregation. \emph{American Journal of Sociology}, \bold{88}, 1008-1018.
}
\seealso{
\code{\link{dissim}}, \code{\link{spseg}}
}
\examples{
library(sf)
data(segdata)
grd <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10)),
                    n = 10)
grd <- grd[as.vector(matrix(1:100, 10)[, 10:1])]

isp(grd, segdata[, 1:2])
isp(grd, segdata[, 5:6])

# a different distance-decay function
isp(grd, segdata[, 1:2], fun = function(z) 1 / (1 + z^2))
}
