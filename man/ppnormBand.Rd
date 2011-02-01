\name{ppnormBand}
\alias{ppnormBand}
\title{Adding simultaneous confidence bands to normal probability plots...}
\usage{ppnormBand(x, e)}
\description{Adding simultaneous confidence bands to normal probability plots}
\details{This function calculates simultaneous confidence bands for the estimated cdf using the logistic-transform normal-approximation method. 

Information on how to find the approximate factors \code{e} can be found in Meeker and Escobar (1998) on page 61. They provide a table which summarizes some common choices for the factor.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{x}{a vector of probabilities}
\item{e}{factors for EP simultaneous approx. confidence bands (see details for more info.)}}
\references{
Meeker, W. Q. and Escobar, L. A. (1998), \emph{Statistical Methods for Reliability Data}, New York, NY: Wiley.
}
