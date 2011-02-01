\name{compare_eb_ls}
\alias{compare_eb_ls}
\title{Visually comparing shrinkage and LS estimates...}
\usage{compare_eb_ls(eb, ols, identify=FALSE, ...)}
\description{Visually comparing shrinkage and LS estimates}
\details{This function creates a plot (using \code{qplot}) where the shrinkage estimate appears on the horizontal axis and the LS estimate appears on the vertical axis.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{eb}{a matrix of random effects}
\item{ols}{a matrix of the LS estimates found using \code{random_ls_coef}}
\item{identify}{identify the percentage of points to identify as unusual, \code{FALSE} if you do not want the points identified.}
\item{...}{other arguments to be passed to \code{qplot}}}
\examples{
\dontrun{
wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
rancoef.eb <- coef(wages.fm1)$id
rancoef.ols <- random_ls_coef(formula = lnw ~ exper | id, lme.model = wages.fm1)
compare_eb_ls(eb = rancoef.eb, ols = rancoef.ols, identify = 0.01)
}
}
