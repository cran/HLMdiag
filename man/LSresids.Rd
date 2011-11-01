\name{LSresids}
\alias{LSresids}
\title{Calculating case-level (level-1) least squares residuals}
\usage{
  LSresids(formula, data, semi.standardize = TRUE)
}
\arguments{
  \item{formula}{a linear formula that is used by
  \code{adjust_lmList}' (y ~ x1 + ... + xn | g where g is a
  grouping factor)}

  \item{data}{a data frame containing the variables in the model.}

  \item{semi.standardize}{if \code{TRUE} the
  semi-standardized residuals will also be returned}
}
\value{
  a data frame with the following columns: id, residual,
  fitted
}
\description{
  This function calculates the case-level least squares
  residuals found by fitting separate LS regression models
  to each case.
}
\examples{
data(Oxboys, package = 'mlmRev')
fm <- lmer(formula = height ~ age + I(age^2) + (age + I(age^2)| Subject), data = Oxboys)
level1Resids <- LSresids(formula = height ~ age + I(age^2) | Subject, data = fm@frame, semi.standardize = TRUE)

\dontrun{wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
LSresids(formula = lnw ~ exper | id, data = wages.fm1@frame)}
}
\author{
  Adam Loy \email{aloy@istate.edu}
}

