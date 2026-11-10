\name{conprof}
\alias{conprof}
\title{Concentration Profile}
\description{
Draws the concentration profile of a population group and computes the summary index proposed by Hong and Sadahiro (2014).
}
\usage{
conprof(data, grpID = 1, n = 999, graph = TRUE, add = FALSE, \dots)
}
\arguments{
  \item{data}{a numeric matrix or data frame with at least two columns, one for each population group, and one row for each geographic unit.}
  \item{grpID}{the column of the group to analyse.}
  \item{n}{the number of threshold values at which the profile is evaluated.}
  \item{graph}{logical. If TRUE, draw the profile.}
  \item{add}{logical. If TRUE, add the profile to the current plot.}
  \item{\dots}{further graphical arguments passed to \code{plot}, \code{axis} and \code{lines}.}
}
\details{
For each threshold \eqn{t} between 0 and 1, the profile gives the proportion of the group that lives in units where the group's share of the population is at least \eqn{t}. The summary index measures the area between the profile and the profile expected without segregation, scaled to lie between 0 and 1.
}
\value{
A list with the threshold values \code{x}, the profile \code{y} and the summary index \code{d}.
}
\author{
Seong-Yun Hong
}
\references{
Poulsen, M., Johnston, R. and Forrest, J. (2002) Plural cities and ethnic enclaves: Introducing a measurement procedure for comparative study. \emph{International Journal of Urban and Regional Research}, \bold{26}, 229-243.

Hong, S.-Y. and Sadahiro, Y. (2014) Measuring geographic segregation: a graph-based approach. \emph{Journal of Geographical Systems}, \bold{16}, 211-231.
}
\seealso{
\code{\link{dissim}}
}
\examples{
data(segdata)
a <- conprof(segdata[, 1:2], main = "Concentration profiles")
b <- conprof(segdata[, 7:8], add = TRUE, lty = 2)
c(a$d, b$d)
}
