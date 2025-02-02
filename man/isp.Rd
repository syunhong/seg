\name{isp}
\alias{isp}
\alias{whiteseg}
\title{Index of Spatial Proximity}
\description{
Computes the index of spatial proximity developed by White (1983). This measure estimates the level of clustering by comparing the average distance between members of the same group with that between all individuals (regardless of the groups to which they belong). The results may change drastically depending on the definition of distance.
}
\usage{
isp(x, data, nb, fun, verbose = FALSE, \dots)
whiteseg(x, data, nb, fun, verbose = FALSE, \dots)
}
\arguments{
  \item{x}{a numeric matrix or data frame with coordinates (each row is a point), or an object of class \code{Spatial}.}
  \item{data}{an object of class \code{matrix}, or one that can be coerced to that class. The number of rows in \sQuote{data} should equal the number of geographic units in \sQuote{x}, and the number of columns should be greater than one (i.e., at least two population groups are required). This can be missing if \sQuote{x} has a data frame attached to it.}
  \item{nb}{an optional \code{matrix} object indicating the distances between the geographic units.}
  \item{fun}{a function for the calculation of proximity. The function should take a numeric vector as an argument (distance) and return a vector of the same length (proximity). If this is not specified, a negative exponential function is used by default.}
  \item{verbose}{logical. If TRUE, print the current stage of the computation and time spent on each job to the screen.}
  \item{\dots}{optional arguments to be passed to \code{dist} when calculating the distances between the geographic units in \sQuote{x}. Ignored if \sQuote{nb} is given. See \code{help(dist)} for available options.}
}
\details{
\sQuote{nb} must be a square matrix (i.e., one that has the same number of rows and columns) but does not have to be symmetric. If \sQuote{nb} is not given, \code{whiteseg} attempts to create a distance matrix of \sQuote{x} using the function \code{dist} in \pkg{stats} and use it as \sQuote{nb}. The optional arguments in \sQuote{\dots} will be passed to \code{dist}.
}
\value{
A single numeric value indicating the degree of segregation; a value of 1 indicates absence of segregation, and values greater than 1.0 indicate clustering. If the index value is less than one, it indicates an unusual form of segregation (i.e., people live closer to other population groups).
}
\note{
The function name \code{whiteseg()} was changed in version 0.4-3 to \code{isp()}. The old function name will be deprecated from 0.6-1.}
\author{
Seong-Yun Hong
}
\references{
White, M. J. (1983). The measurement of spatial segregation. \emph{The American Journal of Sociology}, \bold{88}, 1008-1018.
}
\seealso{
\code{\link{seg}}, \code{dist}
}
\examples{
# uses the idealised landscapes in 'segdata'
data(segdata)
grd <- GridTopology(cellcentre.offset=c(0.5,0.5),
                    cellsize=c(1,1), cells.dim=c(10,10))
grd.sp <- as.SpatialPolygons.GridTopology(grd) |> sf::st_as_sf()

d <- rep(NA, 8) # index of dissimilarity
p <- rep(NA, 8) # index of spatial proximity
for (i in 1:8) {
  idx <- 2 * i
  d[i] <- seg(segdata[,(idx-1):idx])
  p[i] <- whiteseg(grd.sp, data = segdata[,(idx-1):idx])
  full <- segdata[,(idx-1)] == 100
  half <- segdata[,(idx-1)] == 50
  plot(grd.sp)
  plot(grd.sp[full,], col = "Black", add = TRUE)
  if (any(half))
    plot(grd.sp[half,], col = "Grey", add = TRUE)
  text(5, 11.5, labels = 
       paste("D = ", round(d[i], 2), ", P = ", round(p[i], 2), sep = ""))
}
}
