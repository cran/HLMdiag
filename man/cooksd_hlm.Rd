\name{cooksd_hlm}
\alias{cooksd_hlm}
\title{Calculating Cook's Distance...}
\usage{cooksd_hlm(model, delete, type)}
\description{Calculating Cook's Distance}
\details{This function can be used to calculate Cook's Distance for the fixed effects
or variance components for a hierarchical linear model.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{model}{an object containing the original hierarchical model fit using \code{lmer}}
\item{delete}{an object containing the output returned by the \code{case_delete} function}
\item{type}{specifies whether Cook's Distance will be calculated for the fixed effects ("fixef")
or the covariance parameters ("vcov")}}
\value{
The function returns a numeric vector.
}
\references{
Schabenberger, O. (2004),"Mixed Model Influence Diagnostics", in \emph{Proceedings of the Twenty-Ninth SAS Users Group International Conference}, SAS Users Group International.
}
\examples{
\dontrun{
data(Oxboys, package = 'mlmRev')
fm <- lmer(formula = height ~ age + I(age^2) + (age + I(age^2)| Subject), data = Oxboys)
fmDel <- case_delete(model = fm, group = "Subject")
cooksd_hlm(model = fm, delete = fmDel, type = "fixef")

library(mlmRev)
exm1 <- lmer(normexam ~ standLRT + sex + schgend + (1 | school), data = Exam)
exm1DEL <- case_delete(model = exm1, group = "school")
cooksd_hlm(model = exm1, delete = exm1DEL, type = "fixef")
}
}