#' Calculating case-level (level-1) least squares residuals
#'
#' This function calculates the case-level least squares residuals
#' found by fitting separate LS regression models to each case.
#'
#' @param formula a linear formula that is used by the function 'lmList' (y ~ x1 + ... + xn | g where g is a grouping factor)
#' @param lme.model  an object contatining the original hierarchical model fit using lmer()
#' @return a data frame with the following columns: id, residual, fitted
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' ls.residuals(formula = lnw ~ exper | id, lme.model = wages.fm1)
#LSresids <- function(formula, lme.model){
	# fitting a separate LS regression model to each group
#	ls.models <- lmList(formula = formula, data = lme.model@frame)
	
	# obtaining the residuals, fitted values, and model frame
	# from that regression for each group
#	ls.residuals <- lapply(ls.models, resid)
#	ls.fitted <- lapply(ls.models, fitted)
#	ls.model.frame <- lapply(ls.models, model <- function(list){list$model})
	
	# creating a data frame of the residuals, fitted values, and model frames
#	return.df <- do.call('rbind', lapply(1:length(ls.models), function(x){cbind(id = names(ls.models)[[x]], residual = ls.residuals[[x]], fitted = ls.fitted[[x]], ls.model.frame[[x]])}))
	
	# creating a data frame of the residuals, fitted values, and model frames
	# ls.residuals.df <- melt(ls.residuals)
	# colnames(ls.residuals.df) <- c("residual", "groupID")
	# ls.fitted.df <- melt(ls.fitted)
	# colnames(ls.fitted.df) <- c("fitted.value", "groupID")
	# ls.model.frame.df <- ldply(ls.model.frame)
	# colnames(ls.model.frame.df)[1] <- "groupID"
	
	# return.df <- merge(ls.residuals.df, ls.fitted.df, by.x = "groupID", by.y = "groupID")
	# return.df <- merge(return.df, ls.model.frame.df, by.x = "groupID", by.y = "groupID")
	
#	return(return.df)
#}



#----------------------------------------------------------------------------------
# eb.residuals <- function(){
	# This function will calculate the case-level residuals using the estimates
	# provided through 'lmer'. I do not need to code such a function, since using
	# the resid() command will provide me with this type of residual based on
	# lmer. Hilden-Minton (1995) discusses this type of residual for the case-level,
	# but Snijders and Berkhof (2008) use OLS residuals. Since the EB estimate of 
	# beta is a shrinkage estimator, the EB residuals will not typically be orthogonal
	# to the columns of X, so that we are likely to observed a nonzero linear trend
	# on a residual plot of the EB residuals and a linear combination of the columns
	# of X.

#----------------------------------------------------------------------------------
# double_resid_plot <- function(ls.resid, lme.model, smooth = TRUE, ...){ # CHANGED: formerly double.resid.plot
	# This function will construct a "double residual plot" as termed by 
	# Hilden-Minton(1995), or in other words, this is a plot of the residuals obtained
	# through fitting separate least squares regressions to our data and the residuals 
	# obtained by fitting our model as a mixed model (using lmer).
	# Arguments:
	# ls.resid:		A vector of least squares residuals
	# lme.model:	the model fit using 'lmer'
	# smooth: 		A logical value. If TRUE, a linear smoother will be applied
	# ... : 		any additional arguments passed to qplot
#	
#	
#	p <- qplot(x = resid(lme.model), y = ls.resid, ylab="LS residual", xlab = "EB residual", ...) 
#	if(smooth==TRUE){p <- p + geom_smooth(method = "lm", se = FALSE)}
#	return(p)	
#}

#----------------------------------------------------------------------------------
LSresids <- function(formula, lme.model, semi.standardize = TRUE){
# CHANGED: formerly ls.residuals2, this has all the functionality of ls.residuals and includes semi standardized residuals
	# This function will calculate the case-level residuals using the LS estimates.
	# Arguments:	
	# lme.model:	the model fit using 'lmer'
	# formula: 		linear formula of the form y ~ x1 + ... + xn | g where g
	#				is the grouping factors (see ?lmList)
	# semi.standardize: If TRUE, the function will return an additional column, which 
	#	includes the semi-standardized residuals obtained by dividing the residual 
	#	by 1 - h_ij
	
	# fitting a separate LS regression model to each group
	ls.models <- lmList(formula = formula, data = lme.model@frame)
	
	# obtaining the residuals, fitted values, and model frame
	# from that regression for each group
	ls.residuals <- lapply(ls.models, resid)
	ls.fitted <- lapply(ls.models, fitted)
	ls.model.frame <- lapply(ls.models, model <- function(list){list$model})
	
	if(semi.standardize == TRUE){
		ls.influence <- lapply(ls.models, lm.influence)
		ls.hat <- lapply(ls.influence, function(x) x$hat)
	}
	
	# creating a data frame of the residuals, fitted values, and model frames
	if(semi.standardize == TRUE){
		return.df <- do.call('rbind', lapply(1:length(ls.models), function(x){cbind(id = names(ls.models)[[x]], residual = ls.residuals[[x]], 
			fitted = ls.fitted[[x]], ls.model.frame[[x]], hat = ls.hat[[x]], semi.std.resid = ls.residuals[[x]]/sqrt(1-ls.hat[[x]]))}))
	}
	else{
		return.df <- do.call('rbind', lapply(1:length(ls.models), function(x){cbind(id = names(ls.models)[[x]], residual = ls.residuals[[x]], 
			fitted = ls.fitted[[x]], ls.model.frame[[x]])}))
	}
	# creating a data frame of the residuals, fitted values, and model frames
	# ls.residuals.df <- melt(ls.residuals)
	# colnames(ls.residuals.df) <- c("residual", "groupID")
	# ls.fitted.df <- melt(ls.fitted)
	# colnames(ls.fitted.df) <- c("fitted.value", "groupID")
	# ls.model.frame.df <- ldply(ls.model.frame)
	# colnames(ls.model.frame.df)[1] <- "groupID"
	
	# return.df <- merge(ls.residuals.df, ls.fitted.df, by.x = "groupID", by.y = "groupID")
	# return.df <- merge(return.df, ls.model.frame.df, by.x = "groupID", by.y = "groupID")
	
	return(return.df)
}