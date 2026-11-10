\name{segdata}
\docType{data}
\alias{segdata}
\title{Hypothetical Patterns of Segregation}
\description{
This data set contains eight different spatial configurations that were used by Morrill (1991) and Wong (1993) to test their segregation measures.
}
\usage{
data(segdata)
}
\format{
A data frame with 100 rows and 16 columns, representing eight idealised spatial patterns on a 10 by 10 grid. The rows are the grid cells, ordered from the top-left cell row by row. Columns \code{A1} and \code{A2} are the populations of group 1 and group 2 in pattern A, \code{B1} and \code{B2} those in pattern B, and so on up to \code{H1} and \code{H2}.
}
\source{
Morrill, R. L. (1991) On the measure of geographic segregation. \emph{Geography Research Forum}, \bold{11}, 25-36.

Wong, D. W. S. (1993) Spatial indices of segregation. \emph{Urban Studies}, \bold{30}, 559-572.
}
\examples{
library(sf)
data(segdata)

# a 10 x 10 grid in the same order as the rows of 'segdata'
grd <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10)),
                    n = 10)
grd <- grd[as.vector(matrix(1:100, 10)[, 10:1])]

op <- par(mfrow = c(2, 4), mar = c(0, 1, 1.5, 1))
for (i in 1:8) {
  share <- segdata[, 2 * i - 1] / 100
  plot(grd, col = grey(1 - share), main = paste("Pattern", LETTERS[i]))
}
par(op)
}
\keyword{datasets}
