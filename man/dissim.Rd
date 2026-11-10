\name{dissim}
\alias{dissim}
\alias{seg}
\title{Index of Dissimilarity}
\description{
Calculates the index of dissimilarity proposed by Duncan and Duncan (1955). If polygons are given in \sQuote{x}, the index can be adjusted for the spatial arrangement of the population as suggested by Morrill (1991) and Wong (1993). The index can also be adjusted with a user-specified weighting matrix \sQuote{nb}.
}
\usage{
dissim(x, data, nb, adjust = FALSE, verbose = FALSE)
seg(data, nb)
}
\arguments{
  \item{x}{an optional \code{sf} or \code{sfc} object with polygon geometries.}
  \item{data}{a numeric matrix or data frame with two columns that represent mutually exclusive population groups (e.g., Asians and non-Asians). If more than two columns are given, only the first two are used. This can be missing if \sQuote{x} is an \code{sf} object with the data attached.}
  \item{nb}{an optional square \code{matrix} describing the intensity of interaction between geographic units. The sum of all elements should be one.}
  \item{adjust}{logical. If TRUE and \sQuote{x} is given, the index is adjusted according to Morrill (1991) and Wong (1993). See \sQuote{Details}.}
  \item{verbose}{logical. If TRUE, report the progress of the computation.}
}
\details{
When \sQuote{x} is given and \sQuote{adjust} is TRUE, the boundaries shared by neighbouring polygons are found with \pkg{sf}, and three adjusted indices are computed. Each adjusted index takes the form
\deqn{D - \sum_i \sum_j w_{ij} |z_i - z_j|,}{D - sum_i sum_j w_ij |z_i - z_j|,}
where \eqn{z_i} is the proportion of group 1 in unit \eqn{i} and the sums run over all ordered pairs of units.
\describe{
  \item{Morrill's D(adj)}{\eqn{w_{ij}}{w_ij} is one for pairs of units that share a boundary (rook contiguity) and zero otherwise, divided by the sum over all pairs.}
  \item{Wong's D(w)}{\eqn{w_{ij}}{w_ij} is the length of the boundary shared by units \eqn{i} and \eqn{j}, divided by the sum over all pairs.}
  \item{Wong's D(s)}{the weights of D(w) are multiplied by \eqn{((P_i/A_i) + (P_j/A_j)) / 2} divided by the maximum of \eqn{P/A}, where \eqn{A} is the area of a unit and \eqn{P} its perimeter.}
}
Note that the perimeter \eqn{P} used for D(s) is the length of the boundary that a unit shares with other units in the study area, not its total perimeter: boundary segments on the edge of the study area are excluded. This follows Wong (1993) and is needed to reproduce the values reported there. Earlier versions of \pkg{seg} computed the same quantity with GRASS.

Areas and lengths are computed by \pkg{sf}, so they are in metres for geographic coordinates. Units with no population are removed from the adjustment, with a warning.

If \sQuote{nb} is given, the index is adjusted with \sQuote{nb} as the weights \eqn{w_{ij}}{w_ij}. \sQuote{nb} does not have to be symmetric.

\code{seg} is deprecated; use \code{dissim} instead.
}
\value{
\code{dissim} returns a list with the following elements:
\item{d}{index of dissimilarity.}
\item{dm}{index of dissimilarity adjusted according to Morrill (1991). NA if not calculated.}
\item{dw}{index of dissimilarity adjusted according to Wong (1993) using shared boundary lengths. NA if not calculated.}
\item{ds}{index of dissimilarity adjusted according to Wong (1993) using shared boundary lengths and perimeter/area ratios. NA if not calculated.}
\item{user}{index of dissimilarity adjusted with the user-specified matrix \sQuote{nb}. NA if \sQuote{nb} is missing.}
}
\note{
Before version 0.6-0, \code{adjust = TRUE} relied on \pkg{spdep}, \pkg{spgrass6}, \pkg{rgdal} and a GRASS installation, and the arguments \code{p2n.args}, \code{n2m.args}, \code{wVECT.args} and \code{v2n.args} were passed to those packages. These arguments have been removed.
}
\author{
Seong-Yun Hong
}
\references{
Duncan, O. D. and Duncan, B. (1955) A methodological analysis of segregation indexes. \emph{American Sociological Review}, \bold{20}, 210-217.

Morrill, R. L. (1991) On the measure of geographic segregation. \emph{Geography Research Forum}, \bold{11}, 25-36.

Wong, D. W. S. (1993) Spatial indices of segregation. \emph{Urban Studies}, \bold{30}, 559-572.
}
\seealso{
\code{\link{spseg}}, \code{\link{isp}}
}
\examples{
library(sf)
data(segdata)
grd <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10)),
                    n = 10)
grd <- grd[as.vector(matrix(1:100, 10)[, 10:1])]

# D, D(adj), D(w) and D(s) for the eight patterns
out <- sapply(1:8, function(i) {
  unlist(dissim(grd, segdata[, (2 * i - 1):(2 * i)], adjust = TRUE)[1:4])
})
colnames(out) <- LETTERS[1:8]
round(out, 3)

# a user-specified weighting matrix (here, rook contiguity)
rook <- st_relate(grd, pattern = "F***1****", sparse = FALSE) * 1
dissim(data = segdata[, 1:2], nb = rook / sum(rook))$user
}
