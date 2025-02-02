\name{conprof}
\alias{conprof}
\title{Concentration Profile}
\description{
Draws a graph that shows the pattern of residential concentration for a population group and calculates its summary statistic as suggested by Hong and Sadahiro (2013).
}
\usage{
conprof(data, grpID = 1, n = 999, graph = TRUE, add = FALSE, \dots)
}
\arguments{
  \item{data}{an object of class \code{matrix}, or one that can be coerced to that class. Each column represents a population group. The number of columns should be greater than one (i.e., at least two population groups are required).}
  \item{grpID}{a numeric value specifying the population group (i.e., column in \sQuote{data}) to be analysed. Multiple values are not allowed.}
  \item{n}{a numeric value indicating the number of thresholds to be used. A large value of \sQuote{n} creates a smoother-looking graph but slows down the calculation speed.}
  \item{graph}{logical. If TRUE, draw the concentration profile for the specified population group.}
  \item{add}{logical. If TRUE, add the graph to the current plot.}
  \item{\dots}{optional arguments to be passed to \code{plot} when \sQuote{add} is FALSE, or to \code{lines} otherwise. Ignored when \code{graph} is FALSE.}
}
\details{
For \sQuote{n} equally-spaced thresholds between 0 and 1, \code{conprof} identifies the areas where the selected population group comprises at least the given threshold proportions; computes how many of the group members live in these areas; and plots them on a 2D plane with the threshold values in the horizontal axis and the proportions of the people in the vertical axis.

The summary statistic is calculated by estimating the area between the concentration profile and a hypothetical line that represents a uniform distribution (see the examples).
}
\value{
A \code{list} object with the following three elements:

\item{x}{the threshold values.}
\item{y}{the proportions of the people who live in the areas where they comprise at least the corresponding threshold percentages in the local population composition.}
\item{d}{the summary statistic for the concentration profile.}
}
\author{
Seong-Yun Hong
}
\references{
Poulsen, M., Johnston, R., and Forrest J. (2002) Plural cities and ethnic enclaves: Introducing a measurement procedure for comparative study. \emph{International Journal of Urban and Regional Research}, \bold{26}, 229-243.

Hong, S.-Y. and Sadahiro, Y. (2013) Measuring geographic segregation: A graph-based approach. \emph{Journal of Geographical Systems}, \bold{16}, 211-231.
}
\examples{
xx <- runif(100) # random distribution
xx <- xx * (4000 / sum(xx))
yy <- rep(c(40, 60), 100) # no segregation
zz <- rep(c(100, 0), c(40, 60)) # complete segregation

set1 <- cbind(xx, 100 - xx)
set2 <- matrix(yy, ncol = 2, byrow = TRUE)
set3 <- cbind(zz, 100 - zz)

par(mar = c(5.1, 4.1, 2.1, 2.1))
out1 <- conprof(set1, grpID = 1, 
  xlab = "Threshold level (\%)", 
  ylab = "Population proportion (\%)", 
  cex.lab = 0.9, cex.axis = 0.9, lty = "dotted")
out2 <- conprof(set2, grpID = 1, add = TRUE, 
  lty = "longdash")
out3 <- conprof(set3, grpID = 1, add = TRUE)
title(main = paste("R =", round(out1$d, 2)))

# shaded areas represent the summary statistic value
if (require(graphics)) {
  polygon(c(out1$x[1:400], 0.4, 0), 
          c(out1$y[1:400], 1, 1), 
          density = 10, angle = 60, 
          border = "transparent")
  polygon(c(out1$x[401:999], 1, 0.4), 
          c(out1$y[401:999], 0, 0), 
          density = 10, angle = 60, 
          border = "transparent")
}
}
