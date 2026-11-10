\name{SegSpatial}
\alias{SegSpatial}
\alias{SegSpatial-class}
\alias{print.SegSpatial}
\alias{as.list.SegSpatial}
\alias{plot.SegSpatial}
\alias{st_as_sf.SegSpatial}
\title{Spatial Segregation Measures: Results and Maps}
\description{
The class of objects returned by \code{\link{spseg}} and \code{\link{spatseg}}, and its methods. \code{plot} maps where segregation is strong or weak.
}
\usage{
SegSpatial(d, r, h, p, coords, data, env, crs = NA)

\method{print}{SegSpatial}(x, digits = getOption("digits"), \dots)
\method{as.list}{SegSpatial}(x, \dots)
\method{plot}{SegSpatial}(x, index = c("dissimilarity", "diversity",
     "information"), \dots)
\method{st_as_sf}{SegSpatial}(x, \dots)
}
\arguments{
  \item{d, r, h}{numeric values of the dissimilarity, relative diversity and information theory indices (\code{numeric(0)} or \code{NULL} if not computed).}
  \item{p}{the exposure/isolation matrix (a 0 by 0 matrix or \code{NULL} if not computed).}
  \item{coords, data, env, crs}{see \code{\link{SegLocal}}.}
  \item{x}{an object of class \code{SegSpatial}.}
  \item{digits}{the number of digits to print.}
  \item{index}{the local index to map: \dQuote{dissimilarity} (default), \dQuote{diversity} or \dQuote{information}.}
  \item{\dots}{for \code{plot}, arguments passed to \code{plot.sf}, such as \sQuote{main}, \sQuote{xlab}, \sQuote{ylab}, \sQuote{axes}, \sQuote{breaks}, \sQuote{nbreaks}, \sQuote{pal}, \sQuote{border}, \sQuote{key.pos} and \sQuote{reset}. Ignored by the other methods.}
}
\details{
A \code{SegSpatial} object is a list that extends \code{\link{SegLocal}} with the components \code{d}, \code{r}, \code{h} and \code{p} (the global indices) and \code{local}, a matrix with the local values \code{d}, \code{r} and \code{h} for each data point (see \code{\link{spseg}}). When created by \code{spseg} from an \code{sf} object, it also holds the input geometry (\code{geometry}) and, for each data point, the index of the geometry it belongs to (\code{id}).

\code{plot} draws a map of the local values. If the input was polygons, the map uses the original polygons: without smoothing, each polygon shows its own local value; with smoothing, it shows the population-weighted mean of the local values of the grid points inside it, so that the population-weighted mean over all polygons still equals the global index. Polygons without grid points are left blank. Otherwise, the data points are drawn. The default title is \dQuote{Local segregation (D)} (or R, H), and the default palette runs from light yellow (weak) to dark red (strong). Room for \sQuote{xlab} and \sQuote{ylab} is made automatically.

\code{st_as_sf} returns the data being mapped as an \code{sf} object with columns \code{d}, \code{r} and \code{h}, for use with other mapping tools.
}
\value{
\code{SegSpatial} returns an object of class \code{c("SegSpatial", "SegLocal")}. \code{as.list} returns a list with the elements \code{d}, \code{r}, \code{h} and \code{p}. \code{plot} invisibly returns the \code{sf} object that was mapped.
}
\note{
Up to version 0.5-7, \code{SegSpatial} was an S4 class whose slots were accessed with \code{@}. Use \code{$} instead (e.g., \code{x$d}). The \code{spplot} methods have been replaced by \code{plot} and \code{st_as_sf}.
}
\author{
Seong-Yun Hong
}
\seealso{
\code{\link{spseg}}, \code{\link{SegLocal}}
}
\examples{
library(sf)
data(segdata)
grd <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10)),
                    n = 10)
grd <- grd[as.vector(matrix(1:100, 10)[, 10:1])]
a <- spseg(st_sf(segdata[, 3:4], geometry = grd))

a$d
head(a$local)

# the population-weighted mean of the local values is the global index
w <- rowSums(a$data) / sum(a$data)
c(sum(w * a$local[, "d"]), a$d)

plot(a, main = "Pattern B", breaks = seq(0, 1, 0.1))
plot(a, index = "diversity", xlab = "x", ylab = "y", axes = TRUE)

st_as_sf(a)
}
