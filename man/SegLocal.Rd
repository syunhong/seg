\name{SegLocal}
\alias{SegLocal}
\title{Create an Object of Class SegLocal}
\description{
Create a new object of \code{\link{SegLocal-class}} from a matrix of coordinates and population data.
}
\usage{
SegLocal(coords, data, env, proj4string = CRS(as.character(NA)))
}
\arguments{
  \item{coords}{a numeric matrix or data frame with coordinates (each row is a point).}
  \item{data}{an object of class \code{matrix} containing the population data at each data point. The number of rows in \sQuote{data} should equal the number of points in \sQuote{coords}, and the number of columns should be greater than one (i.e., at least two population groups are required).}
  \item{env}{an object of class \code{matrix} containing the local environment parameters. Must be the same dimensions as \sQuote{data}.}
  \item{proj4string}{an optional projection string of class \code{CRS}.}
}
\value{
An object of \code{\link{SegLocal-class}}.
}
\author{
Seong-Yun Hong
}
\seealso{
\code{\link{SegLocal-class}}, \code{\link{localenv}}
}
\examples{
# creates a random data set with 50 data points and 3 population groups
xy <- matrix(runif(100), ncol = 2)
colnames(xy) <- c("x", "y")
pop <- matrix(runif(150), ncol = 3)
colnames(pop) <- LETTERS[1:3]

# constructs an object of class 'SegLocal'
v <- SegLocal(coords = xy, data = pop, env = pop)
is(v)
}
