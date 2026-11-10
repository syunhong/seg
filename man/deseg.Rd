\name{deseg}
\alias{deseg}
\title{Decomposable Segregation Measure}
\description{
Computes the decomposable segregation measure developed by Sadahiro and Hong (2013).
}
\usage{
deseg(x, data, smoothing = "kernel", nrow = 100, ncol = 100, window, sigma,
      verbose = FALSE)
}
\arguments{
  \item{x}{a numeric matrix or data frame with coordinates (each row is a point), an \code{sf} or \code{sfc} object with point or polygon geometries, or an object of class \code{ppp}. For polygons, the centroids are used.}
  \item{data}{a numeric matrix or data frame with at least two columns, one for each population group. This can be missing if \sQuote{x} has the data attached.}
  \item{smoothing}{a character string indicating how to construct the population surface. Currently only \dQuote{kernel} is available.}
  \item{nrow}{the number of rows of the grid used for the population surface.}
  \item{ncol}{the number of columns of the grid.}
  \item{window}{an optional polygon, given as a two-column matrix of vertices or as an \code{sf}/\code{sfc} polygon. See \sQuote{Details}.}
  \item{sigma}{an optional kernel bandwidth. See \sQuote{Details}.}
  \item{verbose}{logical. If TRUE, report the progress of the computation.}
}
\details{
The decomposable segregation measure is a surface-based, non-spatial method. The index calculation does not take into account the spatial arrangement of the population; it is the spatial smoothing that deals with the spatial aspect of segregation. If the smoothing is not done properly, the measure may suffer from the same checkerboard problem as the traditional index of dissimilarity.

\code{deseg} uses \code{kernel2d} in \pkg{splancs}, a quartic kernel estimator with edge correction. Each person is treated as a point, so the population counts should be integers; non-integer counts are truncated, with a warning. Grid points outside \sQuote{window} are not used. If \sQuote{window} is missing, the rectangle that covers all points in \sQuote{x} is used. If \sQuote{sigma} is missing, the smaller of \code{bw.nrd} of the x and y coordinates is used.
}
\value{
An object of class \code{\link{SegDecomp}}.
}
\author{
Seong-Yun Hong
}
\references{
Sadahiro, Y. and Hong, S.-Y. (2013) Decomposition approach to the measurement of spatial segregation. \emph{CSIS Discussion Paper Series}, \bold{119}, 1-33.
}
\seealso{
\code{\link{SegDecomp}}, \code{\link{spseg}}
}
\examples{
data(segdata)
xy <- expand.grid(x = 1:10 - 0.5, y = 10:1 - 0.5)

d <- sapply(1:8, function(i)
  as.vector(deseg(xy, segdata[, (2 * i - 1):(2 * i)], nrow = 30, ncol = 30)))
dimnames(d) <- list(c("Location", "Composition", "Qualitative"),
                    LETTERS[1:8])
round(d, 3)
}
