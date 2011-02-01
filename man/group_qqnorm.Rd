\name{group_qqnorm}
\alias{group_qqnorm}
\title{Plotting numerous normal q-q plots on same plot...}
\usage{group_qqnorm(x, group, line = NULL, alpha_point = 1, alpha_line = 1,...)}
\description{Plotting numerous normal q-q plots on same plot}
\details{This function will plot multiple normal q-q plots on the same plot. This will be particularly useful when comparing the distribution between groups. Differing slopes indicate the normal distributions for the groups
do not share a common variance parameter.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{x}{a numeric vector from which quantiles will be calculated}
\item{group}{a vector indicating group membership for each value in \code{x}.}
\item{line}{the method used to fit reference lines. If no reference lines are desired, leave the value as \code{NULL}. \code{line = "rlm"} will use robust regression to fit reference lines. \code{line = "quantile"} will fit lines through the first and third quartiles.}
\item{alpha_point}{alpha value specified for the points}
\item{alpha_line}{alpha value specified for the lines}
\item{...}{other arguments to be passed to \code{qplot}}}
\references{
Hilden-Minton, J. A. (1995), ``Mulilevel Diagnostics for Mixed and Hierarchical Linear Models,'' Ph.D. thesis, University of California Los Angeles. 
}
\examples{
data(Oxboys, package = 'mlmRev')
fm <- lmer(formula = height ~ age + I(age^2) + (age + I(age^2)| Subject), data = Oxboys)
level1Resids <- LSresids(formula = height ~ age + I(age^2) | Subject, lme.model = fm, semi.standardize = TRUE)
with(level1Resids, group_qqnorm(x = residual, group = id, line = "quantile", alpha_point = .4, alpha_line = .4))

\dontrun{
wages.fm <- lmer(lnw ~ exper + (exper | id), data = wages, line == )
level1.resid <- LSresids(formula = lnw ~ exper | id, lme.model = wages.fm)
with(level1.resid, group_qqnorm(x = residual, group = id))
with(na.omit(level1.resid), group_qqnorm(x = semi.std.resid, group = id))
}
}