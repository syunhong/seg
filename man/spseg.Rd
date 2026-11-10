\name{spseg}
\alias{spseg}
\alias{spatseg}
\title{Spatial Segregation Measures}
\description{
Calculates the set of spatial segregation measures developed by Reardon and O'Sullivan (2004), together with the local values that show where segregation is strong or weak.
}
\usage{
spseg(x, data, method = "all", smoothing = "none", nrow = 100, ncol = 100,
      window, sigma, useC = TRUE, negative.rm = FALSE,
      tol = .Machine$double.eps, verbose = FALSE, \dots)
spatseg(env, method = "all", useC = TRUE, negative.rm = FALSE,
        tol = .Machine$double.eps)
}
\arguments{
  \item{x}{a numeric matrix or data frame with coordinates (each row is a point), an \code{sf} or \code{sfc} object with point or polygon geometries, or an object of class \code{ppp}. For polygons, the centroids (of the largest part) are used as data points.}
  \item{env}{an object of class \code{\link{SegLocal}}.}
  \item{data}{a numeric matrix or data frame with at least two columns, one for each population group. The number of rows must equal the number of points or polygons in \sQuote{x}. This can be missing if \sQuote{x} is an \code{sf} or \code{ppp} object with the data attached.}
  \item{method}{a vector of character strings indicating the measures to be computed: one or more of \dQuote{all} (default), \dQuote{exposure}, \dQuote{information}, \dQuote{diversity} and \dQuote{dissimilarity}. Abbreviations are accepted.}
  \item{smoothing}{a character string indicating how to construct the population surface: \dQuote{none} (default), \dQuote{kernel} or \dQuote{equal}. See \sQuote{Details}.}
  \item{nrow}{the number of rows of the grid used for the population surface. Ignored if \sQuote{smoothing} is \dQuote{none}.}
  \item{ncol}{the number of columns of the grid.}
  \item{window}{an optional polygon, given as a two-column matrix of vertices or as an \code{sf}/\code{sfc} polygon, within which the kernel surface is estimated. See \code{\link{deseg}}.}
  \item{sigma}{an optional kernel bandwidth. See \code{\link{deseg}}.}
  \item{useC}{logical. If TRUE, the global indices are calculated in C.}
  \item{negative.rm}{logical. If TRUE, data points where at least one group has a population of zero or less are removed, to prevent \code{-Inf} or \code{NaN} in the information theory index. If FALSE, such values are replaced with \sQuote{tol}.}
  \item{tol}{a small positive value. See \sQuote{Details}.}
  \item{verbose}{logical. If TRUE, report the progress of the computation.}
  \item{\dots}{optional arguments passed to \code{\link{localenv}}, such as \sQuote{power}, \sQuote{useExp} and \sQuote{maxdist}.}
}
\details{
\code{spatseg} computes the measures from the local environments in a \code{\link{SegLocal}} object. \code{spseg} is a wrapper that first constructs the population surface and its local environments.

With \code{smoothing = "none"}, the data points are the input points or polygon centroids. With \code{"equal"}, the population of each polygon is spread evenly over the points of an \sQuote{nrow} by \sQuote{ncol} grid that fall inside it; polygons that contain no grid point are dropped with a warning, so \sQuote{nrow} and \sQuote{ncol} should be large enough for the smallest polygon. With \code{"kernel"}, a quartic kernel surface is estimated with \code{splancs::kernel2d} (see \code{\link{deseg}}).

\code{tol} is added to all population values, so that \code{v * log(v)} is computed as \code{(v + tol) * log(v + tol)} and does not become \code{NaN} when \code{v = 0}.

Each of the dissimilarity (D), relative diversity (R) and information theory (H) indices is a population-weighted average of local values,
\deqn{D = \sum_p \frac{t_p}{T} d_p, \quad d_p = \frac{\sum_m |\tilde\pi_{pm} - \pi_m|}{2I},}{D = sum_p (t_p/T) d_p,  d_p = sum_m |pi~_pm - pi_m| / (2 I),}
\deqn{R = \sum_p \frac{t_p}{T} r_p, \quad r_p = 1 - \frac{\tilde I_p}{I},}{R = sum_p (t_p/T) r_p,  r_p = 1 - I~_p / I,}
\deqn{H = \sum_p \frac{t_p}{T} h_p, \quad h_p = 1 - \frac{\tilde E_p}{E},}{H = sum_p (t_p/T) h_p,  h_p = 1 - E~_p / E,}
where \eqn{t_p} is the population at point \eqn{p}, \eqn{T} the total population, \eqn{\pi_m}{pi_m} the overall proportion of group \eqn{m}, \eqn{\tilde\pi_{pm}}{pi~_pm} its proportion in the local environment of \eqn{p}, and \eqn{I}, \eqn{\tilde I_p}{I~_p}, \eqn{E} and \eqn{\tilde E_p}{E~_p} the interaction (Simpson) and entropy indices of the region and of the local environment of \eqn{p}. The local values are returned in the component \code{local} and mapped by \code{\link{plot.SegSpatial}}. Larger values indicate places that contribute more to segregation. \eqn{r_p} and \eqn{h_p} are negative where the local environment is more diverse than the region as a whole.
}
\value{
An object of class \code{\link{SegSpatial}}.
}
\note{
The exposure/isolation index, P, is a matrix. The spatial exposure of group \sQuote{m} to group \sQuote{n} is in row \sQuote{m} and column \sQuote{n}. The matrix is rarely symmetric, so it should be interpreted with care. The diagonal cells are the spatial isolation indices.
}
\author{
Seong-Yun Hong
}
\references{
Reardon, S. F. and O'Sullivan, D. (2004) Measures of spatial segregation. \emph{Sociological Methodology}, \bold{34}, 121-162.

Reardon, S. F., Farrell, C. R., Matthews, S. A., O'Sullivan, D., Bischoff, K. and Firebaugh, G. (2009) Race and space in the 1990s: Changes in the geographic scale of racial residential segregation, 1990-2000. \emph{Social Science Research}, \bold{38}, 55-70.
}
\seealso{
\code{\link{SegSpatial}}, \code{\link{plot.SegSpatial}}, \code{\link{localenv}}, \code{\link{deseg}}
}
\examples{
library(sf)
data(segdata)
grd <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10)),
                    n = 10)
grd <- grd[as.vector(matrix(1:100, 10)[, 10:1])]
x <- st_sf(segdata[, 1:2], geometry = grd)

# no spatial smoothing
a <- spseg(x)
print(a, digits = 3)
plot(a)
plot(a, index = "information", main = "Local H",
     xlab = "Easting", ylab = "Northing", axes = TRUE)

# population spread evenly within each polygon
b <- spseg(x, smoothing = "equal", nrow = 50, ncol = 50)
print(b, digits = 3)

# kernel smoothing within a boundary polygon
w <- matrix(c(1.5, 1.5, 1.5, 8.5, 8.5, 8.5, 8.5, 4.5, 5.5, 4.5, 5.5, 1.5),
            ncol = 2, byrow = TRUE)
k <- spseg(x, smoothing = "kernel", window = w, nrow = 20, ncol = 20)
print(k, digits = 3)

# the index values as a list
as.list(k)
}
