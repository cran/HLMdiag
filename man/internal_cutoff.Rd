\name{internal_cutoff}
\alias{internal_cutoff}
\title{Calculating a cutoff value for diagnostic measures...}
\usage{internal_cutoff(diag.out, type)}
\description{Calculating a cutoff value for diagnostic measures}
\details{This function provides cutoff values using internal scaling. 
In other words, a measure of relative standing is used (3*IQR)
to specify unusual values of the diagnostic of interest relative
to the vector of diagnostics.}
\author{Adam Loy \email{aloy@iastate.edu}}
\arguments{\item{diag.out}{an object containing the output from the \code{diagnostics} function.}
\item{type}{specification of which diagnostic is being plotted (COOKSD, MDFFITS, COVTRACE, or COVRATIO).}}
