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
An object of class \code{data.frame}. The data set contains 16 columns, representing eight idealised spatial patterns. Each column indicates the following information:

\tabular{rll}{
  [,1] \tab A1 \tab Pattern A, Group 1 \cr
  [,2] \tab A2 \tab Pattern A, Group 2 \cr
  [,3] \tab B1 \tab Pattern B, Group 1 \cr
  [,4] \tab B2 \tab Pattern B, Group 2 \cr
  [,5] \tab C1 \tab Pattern C, Group 1 \cr
  [,6] \tab C2 \tab Pattern C, Group 2 \cr
  [,7] \tab D1 \tab Pattern D, Group 1 \cr
  [,8] \tab D2 \tab Pattern D, Group 2 \cr
  [,9] \tab E1 \tab Pattern E, Group 1 \cr
  [,10] \tab E2 \tab Pattern E, Group 2 \cr
  [,11] \tab F1 \tab Pattern F, Group 1 \cr
  [,12] \tab F2 \tab Pattern F, Group 2 \cr
  [,13] \tab G1 \tab Pattern G, Group 1 \cr
  [,14] \tab G2 \tab Pattern G, Group 2 \cr
  [,15] \tab H1 \tab Pattern H, Group 1 \cr
  [,16] \tab H2 \tab Pattern H, Group 2
}
}
\source{
Morrill, R. L. (1991). On the measure of geographic segregation. \emph{Geography Research Forum}, \bold{11}, 25-36.

Wong, D. W. S. (1993). Spatial Indices of Segregation. \emph{Urban Studies}, \bold{30}, 559-572. 
}
\examples{
data(segdata)
grd <- GridTopology(cellcentre.offset=c(0.5,0.5),
                    cellsize=c(1,1), cells.dim=c(10,10))
grd.sp <- as.SpatialPolygons.GridTopology(grd)

pd <- par()
par(mfrow = c(2, 4), mar = c(0, 1, 0, 1))
for (i in 1:8) {
  full <- segdata[,(2*i)-1] == 100
  half <- segdata[,(2*i)-1] == 50
  plot(grd.sp)
  plot(grd.sp[full,], col = "Black", add = TRUE)
  if (any(half))
    plot(grd.sp[half,], col = "Grey", add = TRUE)
  text(5, 11.5, labels = paste("Pattern", LETTERS[i]))
}
par(mfrow = pd$mfrow, mar = pd$mar)
}
\keyword{datasets}
