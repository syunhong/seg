\name{SegLocal-class}
\docType{class}
\alias{SegLocal-class}
\alias{coerce,SegLocal,SpatialPoints-method}
\alias{coerce,SegLocal,SpatialPointsDataFrame-method}
\alias{coerce,SegLocal,SpatialPixelsDataFrame-method}
\alias{coerce,SpatialPointsDataFrame,SegLocal-method}
\alias{coerce,SpatialPolygonsDataFrame,SegLocal-method}
\alias{show,SegLocal-method}
\alias{print.SegLocal-method}
\alias{plot.SegLocal-method}
\alias{points.SegLocal-method}
\alias{spplot,SegLocal-method}
\alias{summary.SegLocal-method}
\alias{update.SegLocal-method}

\title{Class SegLocal}
\description{A class to hold the local population composition data.}
\section{Objects from the Class}{
Objects can be created by calls to \code{\link{localenv}}, or the constructor \code{\link{SegLocal}}.
}
\section{Slots}{
  \describe{
    \item{coords}{a numeric matrix or data frame with coordinates (each row is a point).}
    \item{data}{an object of class \code{matrix} containing the population data at each data point. The number of rows in \sQuote{data} should equal the number of points in \sQuote{coords}, and the number of columns should be greater than one (i.e., at least two population groups are required).}
    \item{env}{an object of class \code{matrix} containing the local environment parameters. Must be the same dimensions as \sQuote{data}.}
    \item{proj4string}{an optional projection string of class \code{CRS}.}
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "SegLocal", to = "SpatialPoints")}: coerce an object of class \code{SegLocal} to an object of class \code{SpatialPoints}. The points have no attribute data.}
    \item{coerce}{\code{signature(from = "SegLocal", to = "SpatialPointsDataFrame")}: coerce an object of class \code{SegLocal} to an object of class \code{SpatialPointsDataFrame}. The values in the slot \sQuote{env} will be used as the attribute data.}
    \item{coerce}{\code{signature(from = "SegLocal", to = "SpatialPixelsDataFrame")}: coerce an object of class \code{SegLocal} to an object of class \code{SpatialPixelsDataFrame}. The values in the slot \sQuote{env} will be used as the attribute data. May not work when the points are irregularly spaced.}
    \item{coerce}{\code{signature(from = "SpatialPointsDataFrame", to = "SegLocal")}: coerce an object of class \code{SpatialPointsDataFrame} to an object of class \code{SegLocal}.}
    \item{coerce}{\code{signature(from = "SpatialPolygonsDataFrame", to = "SegLocal")}: coerce an object of class \code{SpatialPolygonsDataFrame} to an object of class \code{SegLocal}.}
    \item{show}{\code{signature(object = "SegLocal")}: show the number of points and data columns in an object of class \code{SegLocal}.}
    \item{print}{\code{signature(x = "SegLocal")}: same as \code{show}.}
    \item{plot}{\code{signature(x = "SegLocal")}: draw a plot, or plots, of points in an object of class \code{SegLocal}. Use an optional argument \sQuote{which.col} to specify a column of the data that determines the points' sizes. See the examples below for demonstration.}
    \item{points}{\code{signature(x = "SegLocal")}: draw points in an object of class \code{SegLocal} on an active graphic device.}
    \item{spplot}{\code{signature(obj = "SegLocal")}: coerce an object of class \code{SegLocal} to an object of class \code{SpatialPixelsDataFrame} or \code{SpatialPointsDataFrame} and display it. See \code{help(spplot)} for more details about the graphical parameter arguments.}
    \item{summary}{\code{signature(object = "SegLocal")}: summarise the population compositions of points and local environments in an object of class \code{SegLocal}.}
    \item{update}{\code{signature(object = "SegLocal")}: update an existing object of class \code{SegLocal}.}
  }
}
\author{Seong-Yun Hong}
\seealso{
\code{\link{SegLocal}}, \code{\link{localenv}}
}
\examples{
# creates 100 regularly-spaced data points and 3 population groups
xy <- expand.grid(1:10, 1:10)
colnames(xy) <- c("x", "y")
pop <- matrix(runif(300), ncol = 3)
colnames(pop) <- LETTERS[1:3]

# constructs an object of class 'SegLocal'
v <- SegLocal(coords = as.matrix(xy), data = pop, env = pop)
summary(v)

# updates the map projection information
v <- update(v, proj4string = sp::CRS("+proj=nzmg +datum=nzgd49"))
summary(v)

# displays the (randomly-generated) local population data
par(mfrow = c(1, 3))
plot(v, main = paste("Data", colnames(pop)), xlab = "x", ylab = "y")
par(mfrow = c(1, 1))

# coverts the object to class 'Spatial'
if (require(sp)) {
  plot(as(v, "SpatialPoints"))
  spplot(v, col.regions = heat.colors(20))
  v.sp <- as(v, "SpatialPixelsDataFrame")
  is(v.sp)
}
}
\keyword{classes}
