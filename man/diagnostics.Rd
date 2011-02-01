\name{diagnostics}
\alias{diagnostics}
\title{Calculating diagnostics for hierarchical linear models.}
\usage{diagnostics(model, delete, type)}
\description{Calculating diagnostics for hierarchical linear models.}
\details{
	This is a wrapper function that calls the all of the diagnostic functions. 
	
	In its current state, the package only supports computation of the diagnostic measures for the fixed effects
	parameters; however, work is underway to extend the functionality to the covariance parameters, which is why
	the future option for "vcov" has been retained for \code{type}.
}
\value{The function returns a data frame with columns:
	\item{IDS}{id of the deleted group}
	\item{COOKSD}{Cook's distance}
	\item{MDFFITS}{Multivariate DFFITS}
	\item{COVTRACE}{}
	\item{COVRATIO}{}
}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{model}{an object containing the original hierarchical model fit using lmer()}
\item{delete}{an object containing the output returned by the \code{case_delete} function}
\item{type}{specifies whether the diagnostics will be calculated for the fixed effects ("fixef")
or the covariance parameters ("vcov")}}
\references{
Christensen, R., Pearson, L.M., and Johnson, W. (1992), ``Case-Deletion Diagnostics for Mixed Models,'' \emph{Technometrics}, 34, 38 -- 45.
	
Schabenberger, O. (2004),"Mixed Model Influence Diagnostics", in \emph{Proceedings of the Twenty-Ninth SAS Users Group International Conference}, SAS Users Group International.
}
\examples{
data(Oxboys, package = 'mlmRev')
fm <- lmer(formula = height ~ age + I(age^2) + (age + I(age^2)| Subject), data = Oxboys)
fmDel <- case_delete(model = fm, group = "Subject")
fmDiag <- diagnostics(model = fm, delete = fmDel, type = "fixef")

\dontrun{
library(mlmRev)
exm1 <- lmer(normexam ~ standLRT + sex + schgend + (1 | school), data = Exam)
exm1DEL <- case_delete(model = exm1, group = "school")
exm1DIAG <- diagnostics(model = exm1, delete = exm1DEL, type = "fixef")
}
}
