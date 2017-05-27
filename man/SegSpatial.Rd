\name{SegSpatial}
\alias{SegSpatial}
\title{Create an Object of Class SegSpatial}
\description{
Creates a new object of \code{\link{SegSpatial-class}}.
}
\usage{
SegSpatial(d, r, h, p, coords, data, env, proj4string = CRS(as.character(NA)))
}
\arguments{
  \item{d}{an object of class \code{numeric} containing the spatial dissimilarity index value.}
  \item{r}{an object of class \code{numeric} containing the spatial diversity index value.}
  \item{h}{an object of class \code{numeric} containing the spatial information theory index value.}
  \item{p}{an object of class \code{matrix} that has the spatial exposure/isolation of all population groups.}
  \item{coords, data, env, proj4string}{see \code{\link{SegLocal-class}}.}
}
\value{
An object of \code{\link{SegSpatial-class}}.
}
\author{
Seong-Yun Hong
}
\seealso{
\code{\link{SegSpatial-class}}, \code{\link{spseg}}
}
\examples{
# creates a random data set with 50 data points and 3 population groups
xy <- matrix(runif(100), ncol = 2)
colnames(xy) <- c("x", "y")
pop <- matrix(runif(150), ncol = 3)
colnames(pop) <- LETTERS[1:3]

# constructs an object of class 'SegSpatial'
v <- SegSpatial(d = numeric(), r = numeric(), h = numeric(), 
                p = matrix(0, 0, 0), coords = xy, data = pop, env = pop)
is(v)
}
