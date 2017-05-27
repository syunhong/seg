\name{SegDecomp-class}
\docType{class}
\alias{SegDecomp-class}
\alias{coerce,SegDecomp,SpatialPoints-method}
\alias{coerce,SegDecomp,SpatialPointsDataFrame-method}
\alias{coerce,SegDecomp,SpatialPixelsDataFrame-method}
\alias{coerce,SegDecomp,vector-method}
\alias{as.vector.SegDecomp-method}
\alias{show,SegDecomp-method}
\alias{print.SegDecomp-method}
\alias{spplot,SegDecomp-method}

\title{Class Decomp}
\description{A class to hold results from \code{\link{deseg}}.}
\section{Objects from the Class}{
Objects can be created by calls to \code{\link{deseg}}, or the constructor \code{\link{SegDecomp}}.
}
\section{Slots}{
  \describe{
    \item{d}{a numeric vector of length three, representing the locational, compositional, and qualitative segregation, respectively.}
    \item{coords}{a numeric matrix or data frame with coordinates (each row is a point).}
    \item{data}{an object of class \code{matrix} containing the population data at each data point. The number of rows in \sQuote{data} should equal the number of points in \sQuote{coords}, and the number of columns should be greater than one (i.e., at least two population groups are required).}
    \item{proj4string}{an optional projection string of class \code{CRS}.}
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "SegDecomp", to = "SpatialPoints")}: coerce an object of class \code{SegDecomp} to an object of class \code{SpatialPoints}. The points have no attribute data.}
    \item{coerce}{\code{signature(from = "SegDecomp", to = "SpatialPointsDataFrame")}: coerce an object of class \code{SegDecomp} to an object of class \code{SpatialPointsDataFrame}. The values in the slot \sQuote{data} will be used as the attribute data.}
    \item{coerce}{\code{signature(from = "SegDecomp", to = "SpatialPixelsDataFrame")}: coerce an object of class \code{SegDecomp} to an object of class \code{SpatialPixelsDataFrame}. This method is to retrieve the kernel density estimates and save it as a \code{SpatialPixelsDataFrame} object, with a proj4string.}
    \item{coerce}{\code{signature(from = "SegDecomp", to = "vector")}: coerce an object of class \code{SegDecomp} to an object of class \code{vector}. Use this to extract the segregation index values from the specified object.}
    \item{as.list}{\code{signature(x = "SegDecomp")}: same as the above.}
    \item{show}{\code{signature(object = "SegDecomp")}: print the segregation index values.}
    \item{print}{\code{signature(x = "SegDecomp")}: same as \code{show}.}
    \item{spplot}{\code{signature(obj = "SegDecomp")}: coerce an object of class \code{SegDecomp} to an object of class \code{SpatialPixelsDataFrame} or \code{SpatialPointsDataFrame} and display it. See \code{help(spplot)} for more details about the graphical parameter arguments.}
  }
}
\author{Seong-Yun Hong}
\seealso{
\code{\link{SegDecomp}}, \code{\link{deseg}}
}
\examples{
# creates 100 regularly-spaced data points and 3 population groups
xy <- expand.grid(1:10, 1:10)
colnames(xy) <- c("x", "y")
pop <- matrix(runif(300), ncol = 3)
colnames(pop) <- LETTERS[1:3]

# randomly-generated index values
d <- runif(2, 0, 0.4)
d <- c(d, 0.9 - sum(d))

# constructs an object of class 'SegDecomp'
v <- SegDecomp(d = d, coords = as.matrix(xy), data = pop)
print(v)

# retrieves the index values
as.vector(v) # same as: as(v, "vector")

# displays the kernel estimates in the slot 'data'
spplot(v, col.regions = heat.colors(20))
}
\keyword{classes}
