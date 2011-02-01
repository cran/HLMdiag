\name{dotplot_diag}
\alias{dotplot_diag}
\title{Dot plots for influence diagnostics...}
\usage{dotplot_diag(diag.out, type, cutoff, ...)}
\description{This is a function that can be used to create modified dotplots for the diagnostic measures.  The plot allows the user to understand the distribution of the diagnostic measure and visually identify unusual cases.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{diag.out}{an object containing the output from the \code{diagnostics} function.}
\item{type}{specification of which diagnostic to plot (either COOKSD, MDFFITS, COVTRACE, COVRATIO).}
\item{cutoff}{value specifying unusual values of the diagnostic}
% \item{label}{variable specifying group identity}
\item{...}{other arguments to be passed to \code{qplot}}}
\details{
Additional arguments used with the \code{qplot} can be added. Some (recommended) examples of this are:
\describe{
\item{xlab}{title for the vertical axis (this is flipped due to \code{coord_flip()})}
\item{ylab}{title for the horizontal axis (this is flipped due to \code{coord_flip()})}
\item{xlim}{limits for the vertical axis (this is flipped due to \code{coord_flip()})}
\item{ylim}{limits for the horizontal axis (this is flipped due to \code{coord_flip()})}
}}
\examples{
data(Oxboys, package = 'mlmRev')
fm <- lmer(formula = height ~ age + I(age^2) + (age + I(age^2)| Subject), data = Oxboys)
fmDel <- case_delete(model = fm, group = "Subject")
fmDiag <- diagnostics(model = fm, delete = fmDel, type = "fixef")
dotplot_diag(diag.out = fmDiag, type = "cooksd", cutoff = "internal", xlab = "Subject", ylab = "Cook's Distance")

\dontrun{
library(mlmRev)
exm1 <- lmer(normexam ~ standLRT + sex + schgend + (1 | school), data = Exam)
exm1DEL <- case_delete(model = exm1, group = "school")
exm1DIAG <- diagnostics(model = exm1, delete = exm1DEL, type = "fixef")
dotplot_diag(diag.out = exm1DIAG, type = "cooksd", cutoff = "internal", xlab = "School ID", ylab = "Cook's Distance")
dotplot_diag(diag.out = exm1DIAG, type = "mdffits", cutoff = "internal", xlab = "School ID", ylab = "MDFFITS")
dotplot_diag(diag.out = exm1DIAG, type = "covratio", cutoff = "internal", xlab = "School ID", ylab = "COVRATIO")
dotplot_diag(diag.out = exm1DIAG, type = "covtrace", cutoff = "internal", xlab = "School ID", ylab = "COVTRACE")
}
}