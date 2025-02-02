\name{dissim}
\alias{dissim}
\title{Index of Dissimilarity}
\description{
Calculates the index of dissimilarity proposed by Duncan and Duncan (1955). If \sQuote{x} or \sQuote{nb} is given, the index is adjusted to reflect the spatial distribution of population.
}
\usage{
dissim(x, data, nb, adjust = FALSE, use_queen = FALSE, verbose = FALSE)
}
\arguments{
  \item{x}{an optional object of class \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame.}}
  \item{data}{a numeric matrix or data frame with two columns that represent mutually exclusive population groups (e.g., Asians and non-Asians). If more than two columns are given, only the first two will be used for computing the index.}
  \item{adjust}{logical. If TRUE, and if \sQuote{x} is given, the index of dissimilarity is adjusted according to the suggestions of Morrill (1991) and Wong (1993), depending on packages installed on the system. See \sQuote{Details} for more information. Ignore if \sQuote{x} is not given.}
  \item{use_queen}{logical. If TRUE, the queen contiguity will be used.}
  \item{verbose}{logical. If TRUE, print the current stage of the computation and time spent on each job to the screen.}
}

\details{
\code{dissim} calculates the index of dissimilarity for \sQuote{data}. If \sQuote{data} is missing, it attempts to extract the data from \sQuote{x}. If \sQuote{x} is not given, or if it is not a valid \code{SpatialPolygonsDataFrame} object, the function stops with an error.

When \sQuote{x} is given and \sQuote{adjust} is set to TRUE, the index is adjusted according to the suggestions of Morrill (1991) and Wong (1993). However, this automatic adjustment requires a number of packages to be installed on the user's system: \pkg{spdep} for Morrill's D(adj) and \pkg{spgrass6} and \pkg{rgdal} for Morrill's D(w) and D(s). Note that, for D(w) and D(s), GRASS commands should be accessible from within R.

Alternatively, the index value can be adjusted using a user-specified weighting matrix \sQuote{nb}. \sQuote{nb} must be a square matrix (i.e., one that has the same number of rows and columns) but does not have to be symmetric. If \sQuote{nb} is not specified, the function calculates the traditional index of dissimilarity proposed by Duncan and Duncan (1955). 

If \sQuote{nb} is a rook-based contiguity matrix standardised by the total number of neighbours, the index is adjusted as described in Morrill (1991). See the example code below and \code{nb2mat} in \pkg{spdep} for more information regarding how to construct such a matrix. For Wong's D(w) and D(s), see \url{https://sites.google.com/site/hongseongyun/seg}. Note that the sum of all elements in \sQuote{nb} should equal one.
}
\value{
\code{dissim} returns a list containing the following elements:
\item{d}{index of dissimilarity.}
\item{dm}{index of dissimilarity adjusted according to Morrill (1991). NA if not calculated.}
\item{dw}{index of dissimilarity adjusted according to Wong (1993). NA if not calculated.}
\item{ds}{index of dissimilarity adjusted according to Wong (1993). NA if not calculated.}
}
\author{
Seong-Yun Hong
}
\references{
Duncan, O. D., & Duncan, B. (1955). A methodological analysis of segregation indexes. \emph{American Sociological Review}, \bold{20}, 210-217.

Morrill, R. L. (1991). On the measure of geographic segregation. \emph{Geography Research Forum}, \bold{11}, 25-36.

Wong, D. W. S. (1993). Spatial indices of segregation. \emph{Urban Studies}, \bold{30}, 559-572.
}
\seealso{
\code{\link{spseg}}}
\examples{
if (require(spdep)) { # package 'spdep' is required

  # uses the idealised landscapes in 'segdata'
  data(segdata)
  geom <- st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,0)))) |> 
    st_make_grid(cellsize = 1)
  grd_sf <- st_sf(geom)
  grd.nb <- nb2mat(poly2nb(grd_sf, queen = FALSE), style = "B")
  grd.nb <- grd.nb / sum(grd.nb)

  d <- list(); m <- list()
  for (i in 1:8) {
    idx <- 2 * i
    d[[i]] <- dissim(data = segdata[,(idx-1):idx])
    m[[i]] <- dissim(grd_sf, segdata[,(idx-1):idx], adjust = TRUE)
  }
}
}
