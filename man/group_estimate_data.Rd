% TODO: update example to run with R CMD check
\name{group_estimate_data}
\alias{group_estimate_data}
\title{Organizing the random effects found in case-deletion...}
\usage{group_estimate_data(delete, group)}
\description{Organizing the random effects found in case-deletion}
\details{This function creates a data frame that will be used by the \code{plot_group_est} function.
It checks to make sure that the levels of the ID factor are defined identically
across columns of the data set and fills in rows for the deleted cases so that 
the data can be plotted.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{delete}{an object containing the output from the \code{ranef.delete} matrix returned by the \code{case_delete} function}
\item{group}{the grouping factor}}
\examples{
\dontrun{wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
wages.del2 <- case_delete(wages.fm1, group = "id")
group_estimate_data(delete = wages.del2, group = "id")
}}
