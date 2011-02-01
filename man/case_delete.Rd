\name{case_delete}
\alias{case_delete}
\title{Implementing Case Deletion.}
\usage{case_delete(model, group)}
\description{Implementing Case Deletion.}
\details{used to iteratively delete groups that correspond to the levels of a 
hierarchical linear model. It uses \code{lmer} to fit the models for each
deleted case (i.e. uses brute force). To investigate numerous levels of
the model, the function will need to be called multiple times, specifying
the group (level) of interest every time.}
\value{a list with the following components: 
\item{fixef.original}{the original fixed effects}
\item{ranef.original}{the original random effects}
\item{vcov.original}{the original variance-covariance parameters}
\item{fixef.delete}{a list of the fixed effects obtained through case deletion}
\item{ranef.delete}{a list of the random effects obtained through case deletion}
\item{vcov.delete}{a list of the variance-covariance parameters obtained 
through case deletion}
\item{fitted.delete}{a list of the fitted values obtained through case deletion}}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{model}{the original hierarchical model fit using \code{lmer}}
\item{group}{a variable used to define the group for which cases will be deleted.
If this is left NULL, then the function will delete individual observations.}}
\references{
Christensen, R., Pearson, L.M., and Johnson, W. (1992), ``Case-Deletion Diagnostics for Mixed Models,'' \emph{Technometrics}, 34, 38 -- 45.
	
Schabenberger, O. (2004),``Mixed Model Influence Diagnostics,'' in \emph{Proceedings of the Twenty-Ninth SAS Users Group International Conference}, SAS Users Group International.
}
\examples{
data(Oxboys, package = 'mlmRev')
fm <- lmer(formula = height ~ age + I(age^2) + (age + I(age^2)| Subject), data = Oxboys)
fmDel <- case_delete(model = fm, group = "Subject")

\dontrun{library(mlmRev)
exm1 <- lmer(normexam ~ standLRT + sex + schgend + (1 | school), data = Exam)
exm1DEL <- case_delete(model = exm1, group = "school")}
}
