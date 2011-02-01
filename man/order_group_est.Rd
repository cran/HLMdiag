% TODO: update example for R CMD check
\name{order_group_est}
\alias{order_group_est}
\title{Ordering the random effects...}
\usage{order_group_est(data, group, estimate)}
\description{Ordering the random effects}
\details{This function returns the order of the data frame of random effects found through case-
deletion so that the plot produced by the \code{plot_est_group} function will display the 
estimated ordered by the mean estimate.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{data}{an object containing the output from the \code{group_estimate_data} function}
\item{group}{the grouping factor}
\item{estimate}{estimate the random effect to be plotted}}
\examples{
\dontrun{wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
wages.del2 <- case_delete(wages.fm1, group = "id")
dat <- group_estimate_data(delete = wages.del2, group = "id")
order_group_est(data = dat, group = "id", estimate = "exper")}}
