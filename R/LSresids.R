#' Calculating case-level (level-1) least squares residuals
#'
#' This function calculates the case-level least squares residuals
#' found by fitting separate LS regression models to each case.
#'
#' @param formula a linear formula that is used by \code{adjust_lmList}' (y ~ x1 + ... + xn | g where g is a grouping factor)
#' @param lme.model  an object contatining the original hierarchical model fit using lmer()
#' @param semi.standardize if \code{TRUE} the semi-standardized residuals will also be returned
#' @return a data frame with the following columns: id, residual, fitted
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' data(Oxboys, package = 'mlmRev')
#' fm <- lmer(formula = height ~ age + I(age^2) + (age + I(age^2)| Subject), data = Oxboys)
#' level1Resids <- LSresids(formula = height ~ age + I(age^2) | Subject, lme.model = fm, semi.standardize = TRUE)
#' 
#' \dontrun{wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' LSresids(formula = lnw ~ exper | id, lme.model = wages.fm1)}
#' @export
LSresids <- function(formula, data, semi.standardize = TRUE){	
	# fitting a separate LS regression model to each group
	ls.models <- adjust_lmList(formula = formula, data = data)
	
	# obtaining the residuals, fitted values, and model frame
	# from that regression for each group
	ls.residuals <- lapply(ls.models, resid)
	ls.fitted <- lapply(ls.models, fitted)
	
	if(semi.standardize == TRUE){
		ls.influence <- lapply(ls.models, lm.influence)
		ls.hat <- lapply(ls.influence, function(x) x$hat)
	}
	
	# creating a data frame of the residuals, fitted values, and model frames
	
	return.df <- cbind(data, residual = unlist(ls.residuals), fitted = unlist(ls.fitted), 
		hat = unlist(ls.hat))
	
	if(semi.standardize == TRUE){
		semi.std.resid  <- with(return.df, residual / sqrt(1 - hat))
		semi.std.resid[is.infinite(semi.std.resid)] <- NaN
	
		return.df <- cbind(return.df, semi.std.resid = semi.std.resid)
	}
		
	return(return.df)
}
