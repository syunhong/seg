\name{SegDecomp}
\alias{SegDecomp}
\title{Create an Object of Class SegDecomp}
\description{
Creates a new object of \code{\link{SegDecomp-class}}.
}
\usage{
SegDecomp(d, coords, data, proj4string = CRS(as.character(NA)))
}
\arguments{
  \item{d}{a numeric vector of length three, representing the locational, compositional, and qualitative segregation, respectively.}
  \item{coords}{a numeric matrix or data frame with coordinates (each row is a point).}
  \item{data}{an object of class \code{matrix} containing the population data at each data point. The number of rows in \sQuote{data} should equal the number of points in \sQuote{coords}, and the number of columns should be greater than one (i.e., at least two population groups are required).}
  \item{proj4string}{an optional projection string of class \code{CRS}.}
}
\value{
An object of \code{\link{SegDecomp-class}}.
}
\author{
Seong-Yun Hong
}
\seealso{
\code{\link{SegDecomp-class}}, \code{\link{deseg}}
}
\examples{
# creates a random data set with 50 data points and 3 population groups
xy <- matrix(runif(100), ncol = 2)
colnames(xy) <- c("x", "y")
pop <- matrix(runif(150), ncol = 3)
colnames(pop) <- LETTERS[1:3]

# constructs an object of class 'SegDecomp'
v <- SegDecomp(d = numeric(3), coords = xy, data = pop)
is(v)
}
