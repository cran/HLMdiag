% TODO: review example for R CMD check
\name{plot_group_est}
\alias{plot_group_est}
\title{Ordering the random effects...}
\usage{plot_group_est(delete, group, estimate, ...)}
\description{Ordering the random effects}
\details{This function creates a plot that allows the user to assess the influence of case
deletion on the estimation of the random effects.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{delete}{an object containing the output from the \code{ranef.delete} matrix returned by the \code{case_delete} function}
\item{group}{the grouping factor}
\item{estimate}{the random effect to be plotted}
\item{...}{other arguments to be passed to \code{qplot}}}
\examples{
\dontrun{
wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
wages.del2 <- case_delete(wages.fm1, group = "id")
small.del <- subset(subset(wages.del2$ranef.delete, id %in% sample(unique(wages.else$id), 100)))
plot_group_est(delete = small.del, group = "id", estimate = "exper", xlab = "deleted group", ylab = "exper")}}
