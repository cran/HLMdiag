\name{ggplot_ppnorm}
\alias{ggplot_ppnorm}
\title{Constructing a normal probability plot...}
\usage{ggplot_ppnorm(x, line=NULL, ...)}
\description{Constructing a normal probability plot}
\details{This function will construct a normal probability plot within
the \code{ggplot2} framework.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{x}{a numeric vector}
\item{line}{the method used to fit a reference line. If no reference line is desired, leave the value as \code{NULL}. \code{line = "rlm"} will use robust regression to fit a reference line. \code{line = "quantile"} will fit a line through the first and third quartiles.}
\item{...}{other arguments to be passed to qplot}}
\examples{
x1 <- rnorm(200)
ggplot_ppnorm(x = x1, line = "rlm")
x2 <- rt(200, df = 5)
ggplot_ppnorm(x = x2, line = "quantile")
}