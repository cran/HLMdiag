#' Identifying unusual points
#'
#' This function will identify the unusual points found when
#' looking at the plot comparing the shrinkage estimates and
#' the random coefficients found through OLS.
#'
#' @param formula a formula that can be used in the 'lm' function
#' @param identify the percentage of points to identify as unusual
#' @author Adam Loy \email{aloy@@istate.edu}
identify_resid <- function(formula, identify){
# CHANGED: formerly identify.resid
	model <- lm(formula = formula)
	res <- resid(model)
	res <- res[order(abs(res), decreasing = TRUE)]
	unusual <- rep(FALSE, length(res))
	n <- round(length(res) * identify)
	unusual[1:n] <- TRUE
	# iqr <- IQR(res)
	# cutoff <- c(lower = quantile(res, 0.25) - 3 * iqr, upper = quantile(res, 0.75) + 3 * iqr)
	# unusual <- res < cutoff[1] | res > cutoff[2]
	return(data.frame(ids = names(res), residual = res, unusual = unusual))
}

#' Plotting unusual cases over the rest
#'
#' This function will produce a plot of the unusual groups
#' identified by 'compare.eb.ls'. The unusual cases will be
#' plotted on top of the other groups.
#'
#' @param data
#' @param unusual.groups
#' @param group
#' @param ... 
#' @author Adam Loy \email{aloy@@istate.edu}
#data_unusual <- function(data, group, unusual.groups, ...){
# CHANGED: formerly data.unusual
#	unusual <- subset(data, group %in% unusual.groups)
#	return(data)
#}