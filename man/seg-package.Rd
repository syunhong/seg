\name{seg-package}
\alias{seg-package}
\docType{package}
\title{Measuring Spatial Segregation}
\description{
Functions for measuring spatial segregation: the index of dissimilarity and its spatial adjustments (Morrill 1991; Wong 1993), White's spatial proximity index (\code{\link{isp}}), Reardon and O'Sullivan's (2004) set of spatial segregation measures (\code{\link{spseg}}), and a decomposable segregation measure (\code{\link{deseg}}). The local values behind Reardon and O'Sullivan's measures can be mapped with \code{\link{plot.SegSpatial}}.
}
\details{
Since version 0.6-0, spatial data are handled with \pkg{sf}. Polygon or point data can be given as \code{sf} or \code{sfc} objects, or as a two-column matrix of coordinates. The packages \pkg{sp}, \pkg{spdep}, \pkg{spgrass6} and \pkg{rgdal} are no longer used, and GRASS is not needed for Wong's D(w) and D(s). Objects returned by the package are S3 objects whose components are extracted with \code{$}. See \code{news(package = "seg")} for other changes.
}
\author{
Seong-Yun Hong and David O'Sullivan. Maintainer: Seong-Yun Hong <syhong@khu.ac.kr>
}
\references{
Hong, S.-Y., O'Sullivan, D. and Sadahiro, Y. (2014) Implementing spatial segregation measures in R. \emph{PLoS ONE}, \bold{9}(11), e113767.
}
\keyword{package}
