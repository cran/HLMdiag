\name{LSresids}
\alias{LSresids}
\title{Calculating case-level (level-1) least squares residuals...}
\usage{LSresids(formula, lme.model, semi.standardize = TRUE)}
\description{Calculating case-level (level-1) least squares residuals}
\details{This function calculates the case-level least squares residuals
found by fitting separate LS regression models to each case.}
\value{a data frame with the following parts: 
\item{id}{the grouping variable}
\item{residual}{the LS residuals}
\item{fitted}{the fitted values from the LS regression models}
\item{frame}{variables in the model frame used for the LS regression models}
\item{hat}{the hat values used to calculate the semi-standardized residuals}
\item{semi.std.resid}{the semi-standardized residuals}
}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{formula}{a linear formula that is used by the function \code{lmList} (\code{y ~ x1 + ... + xn | g} where \code{g} is a grouping factor)}
\item{lme.model}{an object contatining the original hierarchical model fit using \code{lmer}}
\item{semi.standardize}{a logical vector. If true a vector of semi-standardized residuals will also be returned.}}
\examples{
data(Oxboys, package = 'mlmRev')
fm <- lmer(formula = height ~ age + I(age^2) + (age + I(age^2)| Subject), data = Oxboys)
level1Resids <- LSresids(formula = height ~ age + I(age^2) | Subject, lme.model = fm, semi.standardize = TRUE)

\dontrun{wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
LSresids(formula = lnw ~ exper | id, lme.model = wages.fm1)}}
