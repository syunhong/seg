\name{spseg}
\alias{spseg}
\alias{spatseg}
\title{Spatial Segregation Measures}
\description{
Calculates the set of spatial segregation measures developed by Reardon and O'Sullivan (2004).
}
\usage{
spseg(x, data, method = "all", smoothing = "none", nrow = 100, ncol = 100, 
      window, sigma, useC = TRUE, negative.rm = FALSE, 
      tol = .Machine$double.eps, verbose = FALSE, \dots)
spatseg(env, method = "all", useC = TRUE, negative.rm = FALSE, 
        tol = .Machine$double.eps)
}
\arguments{
  \item{x}{a numeric matrix or data frame with coordinates (each row is a point), or an object of class \code{Spatial} or \code{ppp}.}
  \item{env}{an object of \code{\link{SegLocal-class}}.}
  \item{data}{an object of class \code{matrix}, or one that can be coerced to that class. The number of rows in \sQuote{data} should equal the number of points in \sQuote{x}, and the number of columns should be greater than one (i.e., at least two population groups are required). This can be missing if \sQuote{x} has a data frame attached to it.}
  \item{method}{a vector of character strings indicating an measure or measures to be computed. This must be one or more of the strings \dQuote{all} (default), \dQuote{exposure}, \dQuote{information}, \dQuote{diversity}, and \dQuote{dissimilarity}. Abbreviations are accepted, as long as it is clear which method is meant.}
  \item{smoothing}{a character string indicating how to perform spatial smoothing of the population data. This must be (an abbreviation of) one of the strings \dQuote{none} (default), \dQuote{kernel}, or \dQuote{equal}.}
  \item{nrow}{a numeric value indicating the number of row cells in the rasterised data surface. Ignored if \sQuote{smoothing} is \dQuote{none}.}
  \item{ncol}{a numeric value indicating the number of column cells.}
  \item{window}{an optional object of class \code{matrix} to be passed to \code{kernel2d}. See \sQuote{Details} in \code{\link{deseg}}.}
  \item{sigma}{an optional numeric value specifying the kernel bandwidth to be passed to \code{kernel2d}. See also \sQuote{Details} in \code{\link{deseg}}.}
  \item{useC}{logical. If TRUE, calculate the segregation values in C.}
  \item{negative.rm}{logical. If TRUE, all geographic units where at least one group (i.e., column) has a population of zero or less will be removed to prevent -Inf or NaN in the information theory index. If FALSE, the non-positive values will be replaced with \sQuote{tol}.}
  \item{tol}{a small, positive non-zero value. See \sQuote{Details}.}
  \item{verbose}{logical. If TRUE, print the current stage of the computation and time spent on each job to the screen.}
  \item{\dots}{optional arguments to be passed to \code{\link{localenv}} to compute the population composition of each local environment.}
}
\details{
\code{spatseg} computes the set of spatial segregation measures proposed by Reardon and O'Sullivan.

\code{spseg} is a wrapper function, which calls \code{spatseg} after constructing a population density surface and its local environment parameters with user-specified options. Currently the population density surface is constructed by assuming that the population density is uniform in each census tract, or by using \code{kernel2d} in the package \pkg{splancs}. In the previous version (< 0.4.1), the function \code{rasterize} in \pkg{raster} was used in the case of the former, and \code{density.ppp} in \pkg{spatstat} for the latter.

In R, \code{log(0)} is defined as \code{-Inf}, and \code{0 * log(0)} is \code{NaN} (not-a-number), in compliance with the IEEE Standard for Floating-Point Arithmetic. When computing the spatial information theory index, which is a spatial version of the entropy index, this is annoying because \code{0 * log(0)} occurs quite often, especially when one or more groups are small, and/or when the grid size is very small. To work around this problem, the argument \sQuote{tol} is added, so instead of \code{v * log(v)}, it calculates \code{(v + tol) * log(v + tol)}, where \code{v = 0}. This makes the entropy practically zero, as \sQuote{tol} goes towards 0.
}
\value{
An object of \code{\link{SegSpatial-class}}.
}
\note{
The exposure/isolation index, P, is presented in a matrix form. The spatial exposure of group \sQuote{m} to group \sQuote{n} is located in the row \sQuote{m} and column \sQuote{n} of the matrix. The matrix is rarely symmetric in practice so the spatial exposure index should be interpreted with care.

The spatial isolation index values are given in the diagonal cells of the matrix; cell value at (m, m) indicates the degree of spatial isolation for group \sQuote{m} for example.
}
\author{
Seong-Yun Hong
}
\references{
Reardon, S. F. and O'Sullivan, D. (2004) Measures of spatial segregation. \emph{Sociological Methodology}, \bold{34}, 121-162.

Reardon, S. F., Farrell, C. R., Matthews, S. A., O'Sullivan, D., Bischoff, K., and Firebaugh, G. (2009) Race and space in the 1990s: Changes in the geographic scale of racial residential segregation, 1990-2000. \emph{Social Science Research}, \bold{38}, 55-70.
}
\seealso{
\code{\link{SegSpatial-class}}, \code{\link{localenv}}, \code{kernel2d}
}
\examples{
# uses the idealised landscapes in 'segdata'
data(segdata)
grd <- GridTopology(cellcentre.offset=c(0.5,0.5),
                    cellsize=c(1,1), cells.dim=c(10,10))
grd.sp <- as.SpatialPolygons.GridTopology(grd) |> sf::st_as_sf()
test.df <- segdata[,1:2]

# no spatial smoothing
xx1 <- spseg(grd.sp, data = test.df)
print(xx1, digits = 3)

# plots the values in the slot 'data'
spplot(xx1, main = "No spatial smoothing")

\dontrun{
# smoothes the data points
xx2 <- spseg(grd.sp, data = test.df, smoothing = "equal")
print(xx2, digits = 3)
spplot(xx2, main = "Equal")
}

# uses the kernel smoothing of the data points
xx3 <- spseg(grd.sp, data = test.df, smoothing = "kernel", 
             nrow = 20, ncol = 20)
print(xx3, digits = 3)
spplot(xx3, main = "Kernel")

\dontrun{
# same as the above but with a boundary polygon
w <- matrix(c(1.5, 1.5,
              1.5, 8.5,
              8.5, 8.5,
              8.5, 4.5,
              5.5, 4.5,
              5.5, 1.5), ncol = 2, byrow = TRUE)
xx4 <- spseg(grd.sp, data = segdata[,1:2], smoothing = "kernel", 
             window = w, nrow = 20, ncol = 20)
print(xx4, digits = 3)
spplot(xx4, main = "Kernel with a boundary polygon")

# retrieves the index values
as.list(xx4)

# shows the values in the slot 'env'
spplot(as(xx4, "SegLocal"), main = "Local population composition")}
}
