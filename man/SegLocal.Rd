\name{SegLocal}
\alias{SegLocal}
\alias{SegLocal-class}
\alias{print.SegLocal}
\alias{summary.SegLocal}
\alias{update.SegLocal}
\alias{plot.SegLocal}
\alias{points.SegLocal}
\alias{st_as_sf.SegLocal}
\title{Local Environments}
\description{
The class of objects returned by \code{\link{localenv}}, and its methods.
}
\usage{
SegLocal(coords, data, env, crs = NA)

\method{print}{SegLocal}(x, \dots)
\method{summary}{SegLocal}(object, \dots)
\method{update}{SegLocal}(object, coords, data, env, crs, \dots)
\method{plot}{SegLocal}(x, which.col, main, \dots)
\method{points}{SegLocal}(x, which.col = 1, \dots)
\method{st_as_sf}{SegLocal}(x, \dots)
}
\arguments{
  \item{coords}{a numeric matrix with two columns, x and y.}
  \item{data}{a numeric matrix with at least two columns, giving the population of each group at each point.}
  \item{env}{a numeric matrix with the same dimensions as \sQuote{data}, giving the population composition of the local environment of each point. If missing, \sQuote{data} is used.}
  \item{crs}{a coordinate reference system, as accepted by \code{sf::st_crs}.}
  \item{x, object}{an object of class \code{SegLocal}.}
  \item{which.col}{the column(s) of \code{env} to plot.}
  \item{main}{plot title(s).}
  \item{\dots}{further arguments passed to \code{plot} and \code{points} (for \code{plot} and \code{points}) or to \code{summary} (for \code{summary}). Ignored otherwise.}
}
\details{
A \code{SegLocal} object is a list with the components \code{coords}, \code{data}, \code{env} and \code{crs}. \code{plot} and \code{points} draw the points with symbol sizes proportional to the quintile of the local environment values. \code{st_as_sf} converts the object to \code{sf} points with the columns of \code{env} as attributes.
}
\value{
\code{SegLocal} and \code{update} return an object of class \code{SegLocal}. \code{st_as_sf} returns an \code{sf} object.
}
\note{
Up to version 0.5-7, \code{SegLocal} was an S4 class whose slots were accessed with \code{@}, and the projection was stored as an \pkg{sp} \code{CRS} object in the slot \code{proj4string}. Use \code{$} instead (e.g., \code{x$env}); the projection is now an \pkg{sf} \code{crs} object in \code{x$crs}.
}
\author{
Seong-Yun Hong
}
\seealso{
\code{\link{localenv}}, \code{\link{spatseg}}
}
\examples{
data(segdata)
xy <- expand.grid(x = 1:10 - 0.5, y = 10:1 - 0.5)
env <- localenv(xy, segdata[, 1:2])
env
summary(env)
plot(env, which.col = 1)

# a SegLocal object built by hand, then the segregation measures
v <- SegLocal(xy, segdata[, 1:2], env$env)
spatseg(v)
}
