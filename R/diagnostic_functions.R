#' Calculating Cook's Distance
#' 
#' This function can be used to calculate Cook's Distance for the fixed effects
#' or variance components for a hierarchical linear model.
#'
#' @param model an object contatining the original hierarchical model fit using lmer()
#' @param delete an object containing the output returned by the case.delete function
#' @param type specifies whether Cook's Distance will be calculated for the fixed effects ("fixef")
#'   or the covariance parameters ("vcov")
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' wages.fm1.del <- case.delete(wages.fm1, group = "id")
#' CooksD(model = wages.fm1, delete = wages.fm1.del, type = "fixef")
cooksd_hlm <- function(model, delete, type){ #CHANGED: renamed cooksd.hlm -> cooksd_hlm
	if(!is.element(type, c("fixef", "vcov")))
		stop("type must either be specified as 'fixef' or 'vcov'", call. = FALSE)
	groups <- rownames(delete$fixef.delete)
	
	if(type == "fixef"){
		rank.X <- qr(model.matrix(model))$rank
		cook <- NULL
		for(i in 1:length(groups)){
			change.fixef <- as.matrix(delete$fixef.original - delete$fixef.delete[i,])
			cook <- c(cook, t(change.fixef) %*% ginv(as.matrix(vcov(model))) %*% change.fixef / rank.X)
		}
	}
	return(cook)
}

#' Calculating Multivariate DFFITS
#' 
#' This function can be used to calculate the multivariate DFFITS (or MDFFITS) for the fixed effects
#' or variance components for a hierarchical linear model.
#'
#' @param model an object contatining the original hierarchical model fit using lmer()
#' @param delete an object containing the output returned by the case.delete function
#' @param type specifies whether Cook's Distance will be calculated for the fixed effects ("fixef")
#'   or the covariance parameters ("vcov")
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' wages.fm1.del <- case.delete(wages.fm1, group = "id")
#' mdffits(model = wages.fm1, delete = wages.fm1.del, type = "fixef")
mdffits_hlm <- function(model, delete, type){ #CHANGED: renamed mdffits.hlm -> mdffits_hlm
	if(!is.element(type, c("fixef", "vcov")))
		stop("type must either be specified as 'fixef' or 'vcov'", call. = FALSE)
	groups <- rownames(delete$fixef.delete)
	
	if(type == "fixef"){
		rank.X <- qr(model.matrix(model))$rank
		MDFFITS <- NULL
		for(i in 1:length(groups)){
			change.fixef <- as.matrix(delete$fixef.original - delete$fixef.delete[i,])
			MDFFITS <- c(MDFFITS, t(change.fixef) %*% ginv(as.matrix(delete$vcov.delete[[i]])) %*% change.fixef / rank.X)
		}	
	}
	return(MDFFITS)
}

#' Calculating COVTRACE
#' 
#' This function can be used to calculate the COVTRACE for the fixed effects
#' or variance components for a hierarchical linear model. This is  a measure of
#' the precision of those estimates.
#'
#' @param model an object contatining the original hierarchical model fit using lmer()
#' @param delete an object containing the output returned by the case.delete function
#' @param type specifies whether Cook's Distance will be calculated for the fixed effects ("fixef")
#'   or the covariance parameters ("vcov")
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' wages.fm1.del <- case.delete(wages.fm1, group = "id")
#' covtrace(model = wages.fm1, delete = wages.fm1.del, type = "fixef")
covtrace_hlm <- function(model, delete, type){ #CHANGED: renamed covtrace.hlm -> covtrace_hlm
	if(!is.element(type, c("fixef", "vcov")))
		stop("type must either be specified as 'fixef' or 'vcov'", call. = FALSE)
	groups <- rownames(delete$fixef.delete)
	
	if(type == "fixef"){
		rank.X <- qr(model.matrix(model))$rank
		COVTRACE <- NULL
		for(i in 1:length(groups)){
			V.original <- as.matrix(vcov(model))
			V.delete <- as.matrix(delete$vcov.delete[[i]])
			COVTRACE <- c(COVTRACE, abs(sum(diag(ginv(V.original) %*% V.delete)) - rank.X))
		}
	}
	return(COVTRACE)	
}

#' Calculating COVRATIO
#' 
#' This function can be used to calculate the COVRATIO for the fixed effects
#' or variance components for a hierarchical linear model. This is  a measure of
#' the precision of those estimates.
#'
#' @param model an object contatining the original hierarchical model fit using lmer()
#' @param delete an object containing the output returned by the case.delete function
#' @param type specifies whether Cook's Distance will be calculated for the fixed effects ("fixef")
#'   or the covariance parameters ("vcov")
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' wages.fm1.del <- case.delete(wages.fm1, group = "id")
#' covratio(model = wages.fm1, delete = wages.fm1.del, type = "fixef")
covratio_hlm <- function(model, delete, type){ #CHANGED: renamed covtrace.hlm -> covtrace_hlm
	if(!is.element(type, c("fixef", "vcov")))
		stop("type must either be specified as 'fixef' or 'vcov'", call. = FALSE)
	groups <- rownames(delete$fixef.delete)
	if(type == "fixef"){		
		COVRATIO <- NULL
		for(i in 1:length(groups)){
			V.original <- as.matrix(vcov(model))
			V.delete <- as.matrix(delete$vcov.delete[[i]])
			COVRATIO <- c(COVRATIO, det(V.delete) / det(V.original))
		}
	}
	return(COVRATIO)
}

#' Calculating diagnostics for hierarchical linear models.
#'
#' This is a wrapper function that calls the all of the diagnostic functions
#' for a hierarchical model. The function returns a data frame with columns
#' for each diagnostic (cook's d, mdffits, covtrace, and covratio) and
#' a column for the group id.
#'
#' @param model an object contatining the original hierarchical model fit using lmer()
#' @param delete an object containing the output returned by the case.delete function
#' @param type specifies whether Cook's Distance will be calculated for the fixed effects ("fixef")
#'   or the covariance parameters ("vcov")
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' wages.fm1.del <- case.delete(wages.fm1, group = "id")
#' diagnostics(model = wages.fm1, delete = wages.fm1.del, type = "fixef")
diagnostics <- function(model, delete, type){
	if(!is.element(type, c("fixef", "vcov")))
		stop("type must either be specified as 'fixef' or 'vcov'", call. = FALSE)
	
	ids <- as.vector(rownames(delete$fixef.delete))
	
	return(data.frame(IDS = ids, COOKSD = cooksd_hlm(model, delete, type), MDFFITS = mdffits_hlm(model, delete, type), COVTRACE = covtrace_hlm(model, delete, type), COVRATIO = covratio_hlm(model, delete, type)))
}