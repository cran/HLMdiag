\name{random_ls_coef}
\alias{random_ls_coef}
\title{Finding LS estimates for the random coefficient model...}
\usage{random_ls_coef(formula, lme.model)}
\description{Finding LS estimates for the random coefficient model}
\details{This function calculates estimates of the random coefficients 
(intercept and slope) by fitting separate LS regression models to each group. 
This function will work only with one grouping factor.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{formula}{a linear formula that is used by the function \code{lmList} (\code{y ~ x1 + ... + xn | g} where \code{g} is a grouping factor)}
\item{lme.model}{an object contatining the original hierarchical model fit using \code{lmer}}}
\examples{
data(Oxboys, package = 'mlmRev')
fm <- lmer(formula = height ~ age + I(age^2) + (age + I(age^2)| Subject), data = Oxboys)
rancoef.ols <- random_ls_coef(formula = height ~ age + I(age^2) | Subject, lme.model = fm)

\dontrun{wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
random_ls_coef(formula = lnw ~ exper | id, lme.model = wages.fm1)}}