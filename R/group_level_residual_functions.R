#' Finding LS estimates for the random coefficient model
#'
#' This function calculates estimates of the random coefficients (intercept and slope)
#' by fitting separate LS regression models to each group. This function will work
#' only with one grouping factor.
#'
#' @param formula a linear formula that is used by the function 'lmList' (y ~ x1 + ... + xn | g where g is a grouping factor)
#' @param lme.model  an object contatining the original hierarchical model fit using lmer()
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' random.ls.coef(formula = lnw ~ exper | id, lme.model = wages.fm1)
random_ls_coef <- function(formula, lme.model){
# CHANGED: formerly random.ls.coef
	ols.models <- lmList(formula = formula, data = lme.model@frame)
	ols.coefs <- lapply(ols.models, coef)
	ols.coefs <- do.call('rbind', ols.coefs)
	
	return(ols.coefs)
}

#' Visually comparing shrinkage and LS estimates
#'
#' This function creates a plot (using qplot) where the shrinkage estiate appears on the
#' horizontal axis and the LS estimate appears on the vertical axis.
#'
#' @param eb a matrix of random effects
#' @param ols a matrix of the LS estimates found using 'random.ls.coef'
#' @param identify  identify the percentage of points to identify as unusual, FALSE if you do not want the points identified.
#' @param ... other arguments to be passed to qplot
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' rancoef.eb <- coef(wages.fm1)$id
#' rancoef.ols <- random.ls.coef(formula = lnw ~ exper | id, lme.model = wages.fm1)
#' compare.eb.ls(eb = rancoef.eb, ols = rancoef.ols, identify = 0.01)
compare_eb_ls <- function(eb, ols, identify = FALSE, ...){
# CHANGED: formerly compare.eb.ls
	ret <- NULL
	for(i in 1:dim(ols)[2]){
	p <- qplot(x = eb[,i], y = ols[,i], geom = "point", main = colnames(eb)[i], xlab = "shrinkage estimate", ylab = "LS estimate", ...) + 
			geom_abline(intercept = 0, slope = 1) +
			geom_smooth(method = "lm", se = FALSE) 
		if(identify != FALSE){
			extreme <- identify_resid(formula = ols[,i] ~ eb[,i], identify = identify)
			dat <- na.omit(cbind(eb = eb[,i], ols = ols[,i]))
			dat <- as.data.frame(dat)
			dat$ids <- row.names(dat)
			extreme <- merge(x = dat, y = extreme)
			# extreme <- cbind(dat, extreme)
			# extreme <- extreme[order(abs(extreme$residual), decreasing = TRUE), ]
			# n <- round(dim(extreme)[1] * identify)
			
			p <- p + geom_text(data = subset(extreme, unusual == TRUE), aes(x = eb, y = ols, label = ids, hjust=.5, vjust=1.5), size=I(3))
			#ret <- list(ret, subset(extreme, unusual == TRUE))
			ret <- list(ret, extreme)
		}
	browser()
	print(p)
	}
	ret <- list(ret[[1]][[2]], ret[[2]])
	return(ret)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#ls.residuals.group <- function(formula, lme.model){
#	# This function will find the group-level residuals using the OLS method, which
#	# can be obtained by treating the random effects as fixed effects.	# Arguments:
#	# formula: linear formula of the form y ~ x1 + ... + xn | g where g
#	#			is the grouping factors (see ?lmList)
#	# lme.model:	the model fit using 'lmer'
#	
#	# I tried something here from Gelman and Hill, but it doesn't seem general...
#	# ols.model <- lmList(formula = formula, data = data)
#	# ols.ranef <- coef(ols.model)
#	
#	# Snijders and Berhok (2008) give an equation that allows us to calculate th
#	# OLS level-2 residuals: solve(t(Z) %*% Z) %*% t(Z) (y - X %*% beta)
#	# for each group j. Therefore, we must first obtain OLS estimates of beta for 
#	# each group, and then obtain the Z matrix for each gorup.
#	
#	# Fitting OLS regression for each level-2 grouping
#	ols.models <- lmList(formula = formula, data = lme.model@frame)
#	ols.models.resid <- lapply(ols.models, resid)
#	
#	# Obtaining the Z matrix for each level-2 grouping
#	z.all <- as.matrix(t(lme.model@Zt))
#	z.all <- cbind(z.all, lme.model@flist) # adding column denoting group membership
#	
#	# Splitting the data set into groups
#	z.group <- split(z.all, lme.model@flist)
#	z.group <- lapply(z.group, rm.grp <- function(df){df[,-dim(df)[2]]}) # remove grp column
#	z.group <- lapply(z.group, as.matrix)
#	
#	# z.part <- lapply(z.group, function(df){ginv(as.matrix(t(df)) %*% as.matrix(df)) %*% as.matrix(t(df))}) 
#	
#	## THIS IS VERY SLOW FOR LARGE DATA SETS
#	z.part <- lapply(z.group, function(df){ginv(t(df) %*% df) %*% t(df)})
#	
#	# Now I need to use the two lists I have obtained to calculate the OLS residuals
#	# First
#	ols.resid <- mlply(cbind(z.id=names(z.part), ols.id=names(ols.models.resid)), 		mult <- function(z.id, ols.id){z.part[[z.id]] %*% ols.models.resid[[ols.id]]})
#	
#	ols.resid <- ldply(ols.resid, sum)
#	
#	ols.resid <- ols.resid[,-2]
#	colnames(ols.resid) <- c("ID", names(ranef(lme.model)))
#	
#	return(ols.resid)
#}

#----------------------------------------------------------------------------------
# eb.residuals.group <- function(){
	# This function will calculate the group-level residuals using the estimates
	# provided through 'lmer'. Using what they call the empirical bayes method
	# we do not need a separate function to obtain these, since ranef() in lmer
	# will give us a shrinkage (EB) estimate.		# Arguments:
	
	
	
#----------------------------------------------------------------------------------
# double.resid.plot <- function(ls.resid, lme.model, smooth = TRUE, ...){
# 	# This function will construct a "double residual plot" as termed by 
# 	# Hilden-Minton(1995), or in other words, this is a plot of the residuals obtained
# 	# through fitting separate least squares regressions to our data and the residuals 
# 	# obtained by fitting our model as a mixed model (using lmer).
# 	# Arguments:
# 	# ls.resid:		A vector of least squares residuals
# 	# lme.model:	the model fit using 'lmer'
# 	# smooth: 		A logical value. If TRUE, a linear smoother will be applied
# 	# ... : 		any additional arguments passed to qplot
# 	
# 	
# 	p <- qplot(x = resid(lme.model), y = ls.resid, ylab="LS residual", xlab = "EB residual", ...) 
# 	if(smooth==TRUE){p <- p + geom_smooth(method = "lm", se = FALSE)}
# 	return(p)	
# }
#----------------------------------------------------------------------------------
