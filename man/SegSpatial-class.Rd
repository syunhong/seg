\name{SegSpatial-class}
\docType{class}
\alias{SegSpatial-class}
\alias{coerce,SegSpatial,SpatialPoints-method}
\alias{coerce,SegSpatial,SpatialPointsDataFrame-method}
\alias{coerce,SegSpatial,SpatialPixelsDataFrame-method}
\alias{coerce,SegSpatial,list-method}
\alias{as.list.SegSpatial-method}
\alias{show,SegSpatial-method}
\alias{print.SegSpatial-method}
\alias{spplot,SegSpatial-method}

\title{Class SegSpatial}
\description{A class to hold results from \code{\link{spatseg}}.}
\section{Objects from the Class}{
Objects can be created by calls to \code{\link{spseg}}, or the constructor \code{\link{SegSpatial}}.
}
\section{Slots}{
  \describe{
    \item{d}{an object of class \code{numeric} containing the spatial dissimilarity index value.}
    \item{r}{an object of class \code{numeric} containing the spatial diversity index value.}
    \item{h}{an object of class \code{numeric} containing the spatial information theory index value.}
    \item{p}{an object of class \code{matrix} that has the spatial exposure/isolation of all population groups.}
    \item{coords, data, env, proj4string}{see \code{\link{SegLocal-class}}.}
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "SegSpatial", to = "SpatialPoints")}: coerce an object of class \code{SegSpatial} to an object of class \code{SpatialPoints}. The points have no attribute data.}
    \item{coerce}{\code{signature(from = "SegSpatial", to = "SpatialPointsDataFrame")}: coerce an object of class \code{SegSpatial} to an object of class \code{SpatialPointsDataFrame}. The values in the slot \sQuote{data} will be used as the attribute data.}
    \item{coerce}{\code{signature(from = "SegSpatial", to = "SpatialPixelsDataFrame")}: coerce an object of class \code{SegSpatial} to an object of class \code{SpatialPixelsDataFrame}. The values in the slot \sQuote{data} will be used as the attribute data. May not work when the points are irregularly spaced.}
    \item{coerce}{\code{signature(from = "SegSpatial", to = "list")}: retrieve the segregation index values and return it as a \code{list} object.}
    \item{as.list}{\code{signature(x = "SegSpatial")}: same as the above.}
    \item{show}{\code{signature(object = "SegSpatial")}: show the segregation index values.}
    \item{print}{\code{signature(x = "SegSpatial")}: same as \code{show}.}
    \item{spplot}{\code{signature(obj = "SegSpatial")}: coerce an object of class \code{SegSpatial} to an object of class \code{SpatialPixelsDataFrame} or \code{SpatialPointsDataFrame} and display it. See \code{help(spplot)} for more details about the graphical parameter arguments.}
  }
}
\section{Extends}{
\code{\link{SegLocal-class}}.
}
\author{Seong-Yun Hong}
\seealso{
\code{\link{SegSpatial}}, \code{\link{spseg}}
}
\examples{
# creates 100 regularly-spaced data points and 3 population groups
xy <- expand.grid(1:10, 1:10)
colnames(xy) <- c("x", "y")
pop <- matrix(runif(300), ncol = 3)
env <- matrix(runif(300), ncol = 3)
colnames(pop) <- LETTERS[1:3]
colnames(env) <- LETTERS[4:6]

# constructs an object of class 'SegSpatial'
v <- SegSpatial(d = numeric(), r = numeric(), h = numeric(), 
                p = matrix(0, 0, 0), 
                coords = as.matrix(xy), data = pop, env = env)
print(v)

# changes the spatial dissimilarity index value
slot(v, "d") <- runif(1)

# retrieves the index values
as.list(v)

# displays the values in the slot 'data'
# spplot(v, col.regions = heat.colors(20))

# displays the values in the slot 'env'
# w <- as(v, "SegLocal")
# spplot(w, col.regions = heat.colors(20))
}
\keyword{classes}
