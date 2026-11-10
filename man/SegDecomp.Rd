\name{SegDecomp}
\alias{SegDecomp}
\alias{SegDecomp-class}
\alias{print.SegDecomp}
\alias{as.vector.SegDecomp}
\alias{st_as_sf.SegDecomp}
\title{Decomposable Segregation Measure: Results}
\description{
The class of objects returned by \code{\link{deseg}}, and its methods.
}
\usage{
SegDecomp(d, coords, data, crs = NA)

\method{print}{SegDecomp}(x, digits = getOption("digits"), \dots)
\method{as.vector}{SegDecomp}(x, mode = "any")
\method{st_as_sf}{SegDecomp}(x, \dots)
}
\arguments{
  \item{d}{a numeric vector of length three: the location, composition and qualitative components.}
  \item{coords, data, crs}{see \code{\link{SegLocal}}.}
  \item{x}{an object of class \code{SegDecomp}.}
  \item{digits}{the number of digits to print.}
  \item{mode}{passed to \code{\link{as.vector}}.}
  \item{\dots}{ignored.}
}
\details{
A \code{SegDecomp} object is a list with the components \code{d}, \code{coords}, \code{data} and \code{crs}, where \code{coords} and \code{data} describe the smoothed population surface.
}
\value{
\code{SegDecomp} returns an object of class \code{SegDecomp}, \code{as.vector} the three components, and \code{st_as_sf} an \code{sf} object of the surface points.
}
\note{
Up to version 0.5-7, \code{SegDecomp} was an S4 class. Use \code{$} instead of \code{@} to extract components.
}
\author{
Seong-Yun Hong
}
\seealso{
\code{\link{deseg}}
}
\examples{
data(segdata)
xy <- expand.grid(x = 1:10 - 0.5, y = 10:1 - 0.5)
d <- deseg(xy, segdata[, 1:2], nrow = 30, ncol = 30)
d
as.vector(d)
}
