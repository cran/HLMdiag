% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hlm_influence.R
\name{hlm_influence.default}
\alias{hlm_influence.default}
\alias{hlm_influence.lmerMod}
\alias{hlm_influence}
\alias{hlm_influence.lme}
\title{Calculating influence diagnostics for HLMs}
\usage{
\method{hlm_influence}{default}(model, ...)

\method{hlm_influence}{lmerMod}(
  model,
  level = 1,
  delete = NULL,
  approx = TRUE,
  leverage = "overall",
  data = NULL,
  ...
)

\method{hlm_influence}{lme}(
  model,
  level = 1,
  delete = NULL,
  approx = TRUE,
  leverage = "overall",
  ...
)
}
\arguments{
\item{model}{an object of class \code{lmerMod} or \code{lme}}

\item{...}{not in use}

\item{level}{used to define the group for which cases are deleted and influence
diagnostics are calculated. If \code{level = 1} (default), then influence diagnostics are
calculated for individual observations. Otherwise, \code{level} should be the name of a grouping
factor as defined in \code{flist} for a \code{lmerMod} object or as in \code{groups} for a \code{lme} object.}

\item{delete}{numeric index of individual cases to be deleted. If the \code{level} parameter 
is specified, \code{delete} may also take the form of a character vector consisting of group 
names as they appear in \code{flist} for \code{lme4} models or as in \code{groups} for \code{nlme} models. 
If \code{delete = NULL} then all cases are iteratively deleted.}

\item{approx}{logical parameter used to determine how the influence diagnostics are calculated.
If \code{FALSE} (default), influence diagnostics are calculated using a one step approximation.
If \code{TRUE}, influence diagnostics are calculated by iteratively deleting groups and refitting
the model using \code{lmer}. This method is more accurate, but slower than the one step approximation.
If \code{approx = FALSE}, the returned tibble also contains columns for relative variance change (RVC).}

\item{leverage}{a character vector to determine which types of leverage should be included in the
returned tibble. There are four options: 'overall' (default), 'fixef', 'ranef', or 'ranef.uc'.
One or more types may be specified. For additional information about the types of leverage, see
\code{?leverage}.}

\item{data}{(optional) the data frame used to fit the model. This is only necessary for \code{lmerMod} models if
\code{na.action = "na.exclude"} was set.}
}
\description{
This function is used to compute influence diagnostics for a hierarchical linear model.
It takes a model fit as a \code{lmerMod} object or as a \code{lme} object and returns a tibble with Cook's
distance, MDFFITS, covtrace, covratio, and leverage.
}
\details{
The \code{hlm_influence} function provides a wrapper that appends influence diagnostics
to the original data. The approximated influence diagnostics returned by this
function are equivalent to those returned by \code{cooks.distance}, \code{mdffits}, \code{covtrace},
\code{covratio}, and \code{leverage}. The exact influence diagnostics obtained through a full
refit of the data are also available through \code{case_delete} and the accompanying functions
\code{cooks.distance}, \code{mdffits}, \code{covtrace}, and \code{covratio} that can be called
directly on the \code{case_delete} object.
}
\note{
It is possible to set \code{level} and delete individual cases from different groups using 
\code{delete}, so numeric indices should be double checked to confirm that they encompass entire groups.
Additionally, if \code{delete} is specified, leverage values are not returned in the resulting tibble.
}
