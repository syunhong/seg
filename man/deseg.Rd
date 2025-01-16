\name{deseg}
\alias{deseg}
\title{Decomposable Segregation Measure}
\description{
Calculates the decomposable segregation measure developed by Sadahiro and Hong (2013).
}
\usage{
deseg(x, data, smoothing = "kernel", nrow = 100, ncol = 100, 
      window, sigma, verbose = FALSE)
}
\arguments{
  \item{x}{a numeric matrix or data frame with coordinates (each row is a point), or an object of class \code{Spatial} or \code{ppp}.}
  \item{data}{an object of class \code{matrix}, or one that can be coerced to that class. The number of rows in \sQuote{data} should equal the number of points in \sQuote{x}, and the number of columns should be greater than one (i.e., at least two population groups are required). This can be missing if \sQuote{x} has a data frame attached to it.}
  \item{smoothing}{a character string indicating how to perform spatial smoothing of the population data. Currently only \dQuote{kernel} is supported.}
  \item{nrow}{a numeric value indicating the number of row cells in the rasterised data surface.}
  \item{ncol}{a numeric value indicating the number of column cells.}
  \item{window}{an optional object of class \code{matrix} to be passed to \code{kernel2d}. See \sQuote{Details}.}
  \item{sigma}{an optional numeric value specifying the kernel bandwidth to be passed to \code{kernel2d}. See also \sQuote{Details}.}
  \item{verbose}{logical. If TRUE, print the current stage of the computation and time spent on each job to the screen.}
}
\details{
The decomposable segregation measure is a surface-based, non-spatial method. The index calculation does not take into account the spatial arrangement of the population. It is the spatial smoothing process that deals with the spatial aspect of segregation. If the spatial smoothing is not performed properly, this measure may suffer from the same checkerboard problem as the traditional index of dissimilarity.

Currently \code{deseg} uses the function \code{kernel2d} in \pkg{splancs}, which employs a quartic kernel estimator. The points outside \sQuote{window} are not considered when computing the kernel estimates. The argument \sQuote{window} must be a polygon represented in a matrix form such that each row corresponds to a vertex. This is passed to \code{kernel2d} as \sQuote{poly}. If \sQuote{window} is missing, a square that covers all points in \sQuote{x} will be used.

If \sQuote{sigma} is not given, \code{bw.nrd} in \pkg{stats} is used to find the optimal bandwidth.
}
\value{
An object of \code{\link{SegDecomp-class}}.
}
\author{
Seong-Yun Hong
}
\references{
Sadahiro, Y. and Hong, S.-Y. (2013) Decomposition approach to the measurement of spatial segregation. \emph{CSIS Discussion Paper Series}, \bold{119}, 1-33.
}
\seealso{
\code{\link{SegDecomp-class}}, \code{kernel2d}
}
\examples{
# uses the idealised landscapes in 'segdata'
data(segdata)
grd <- GridTopology(cellcentre.offset=c(0.5,0.5),
                    cellsize=c(1,1), cells.dim=c(10,10))
grd.sp <- as.SpatialPolygons.GridTopology(grd) |> sf::st_as_sf()

# displays the test data
plot(grd.sp)
plot(grd.sp[segdata[,9] == 100,], col = "Black", add = TRUE)
plot(grd.sp[segdata[,9] == 50,], col = "Grey", add = TRUE)

# tries different bandwidths for the same data
bw1 <- deseg(grd.sp, segdata[,9:10], sigma = 1, nrow = 20, ncol = 20)
print(bw1, digits = 3)
bw1.val <- sum(as.vector(bw1))
spplot(bw1, col.regions=rev(heat.colors(20)), 
       main = paste("Bandwidth = 1, S =", round(bw1.val, 2)))

bw2 <- deseg(grd.sp, segdata[,9:10], sigma = 2, nrow = 20, ncol = 20)
print(bw2, digits = 3)
bw2.val <- sum(as(bw2, "vector"))
spplot(bw2, col.regions=rev(heat.colors(20)), 
       main = paste("Bandwidth = 2, S =", round(bw2.val, 2)))

\dontrun{
# let's see how the index value changes with different bandwidths
h0 <- seq(1, 5, length.out = 10); vals <- numeric()
for (i in 1:10) {
  d <- deseg(grd.sp, segdata[,9:10], sigma = h0[i], verbose = TRUE)
  vals <- append(vals, sum(as(d, "vector")))
}
plot(h0, vals, type = "b", xlab = "Bandwidth", ylab = "S")
title(main = "segdata[,9:10]")

# calculates the index for all data sets in 'segdata'
d.segdata <- matrix(NA, nrow = 3, ncol = 8)
for (i in 1:8) {
  idx <- 2 * i
  tmp <- deseg(grd.sp, segdata[,(idx-1):idx])
  d.segdata[,i] <- as(tmp, "vector")
}

# presents the results as a bar chart
barplot(d.segdata, names.arg = LETTERS[1:8], main = "segdata",
        legend.text = c(expression(italic(paste("S"[L]))), 
                        expression(italic(paste("S"[C]))),
                        expression(italic(paste("S"[Q])))),
        args.legend = list(x = "topright", horiz = TRUE))}
}
