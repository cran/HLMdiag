#' Implementing Case Deletion.
#'
#' used to iteratively delete groups that correspond to the levels of a 
#' hierarchical linear model. It uses lmer() to fit the models for each
#' deleted case (i.e. uses brute force). To investigate numerous levels of
#' the model, the function will need to be called multiple times, specifying
#' the group (level) of interest every time.
#'
#' @param model the original hierarchical model fit using lmer()
#' @param group a variable used to define the group for which cases will be deleted.
#'   If this is left NULL, then the function will delete individual observations.
#' @return a list with the following compontents: 
#'   \item{fixef.original}{the original fixed effects}
#'   \item{ranef.original}{the origingal random effects}
#'   \item{vcov.original}{the original variance-covariance parameters}
#'   \item{fixef.delete}{a list of the fixed effects obtained through case deletion}
#'   \item{ranef.delete}{a list of the random effects obtained through case deletion}
#'   \item{vcov.delete}{a list of the variance-covariance parameters obtained 
#'   through case deletion}
#'   \item{fitted.delete}{a list of the fitted values obtained through case deletion} 
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples
#' exm1 <- lmer(normexam ~ standLRT + sex + schgend + (1 | school), data = Exam)
#' exm1DEL <- case.delete(model = exm1, group = "school") 
case_delete <- function(model, group = NULL){ # CHANGED: formerly names case.delete
	if(is.null(group) == TRUE){
		model@frame$INDEX <- row(model@frame)[,1]
		# SINGLE CASE DELETION DIAGNOSTICS
#		data.delete <- data.frame(model@frame, INDEX = row(model@frame)[,1])
		data.delete <- split(model@frame, row(model@frame)[,1])
		data.delete <- lapply(data.delete,  function(x){
			data.delete[[x$INDEX]] <- NULL
			do.call('rbind', data.delete)
		})
		
		model.delete <- lapply(data.delete, lmer, formula = formula(model))

		ranef.delete <- lapply(model.delete, function(x){
			data.frame(deleted = setdiff(model@frame[, "INDEX"], as.numeric(rownames(x@frame))), 
			id = rownames(ranef(x)[[1]]), ranef(x)[[1]])
		})
		
		fitted.delete <- lapply(model.delete, function(x){
			data.frame(deleted = setdiff(model@frame[, "INDEX"], as.numeric(rownames(x@frame))), 
			x@frame, fitted(x))
		})
	}
	
	
	else{
		column.groups <- which(names(model@frame) == group)
		# MULTIPLE CASE DELETION DIAGNOSTICS
#		data.delete <- dlply(model@frame, group, function(df){
#			subset(model@frame, model@frame[, column.groups] != unique(df[, column.groups]))
#		})	
		
		data.delete <- split(model@frame, model@frame[,group])
		data.delete <- lapply(data.delete, function(df){
			data.delete[[unique(df[,group])]] <- NULL
			do.call('rbind', data.delete)
		})
		
		model.delete <- lapply(data.delete, lmer, formula = formula(model))
		
		ranef.delete <- lapply(model.delete, function(x){
			data.frame(deleted = setdiff(model@frame[, column.groups], x@frame[, column.groups]), 
			id = rownames(ranef(x)[[1]]), ranef(x)[[1]])
		})
		
		fitted.delete <- lapply(model.delete, function(x){
			data.frame(deleted = setdiff(model@frame[, column.groups], x@frame[, column.groups]), 
			x@frame, fitted(x))
		})
	}
		
	
	ranef.delete <- do.call('rbind', ranef.delete)
	fitted.delete <- do.call('rbind', fitted.delete)
	
	fixef.delete <- lapply(model.delete, fixef)
	fixef.delete <- do.call('rbind', fixef.delete)
	
	vcov.delete <- lapply(model.delete, vcov)
	vcov.delete <- lapply(vcov.delete, as.matrix)
	# Should I name the dimensions here? or is it not necessary?
	# Do I need a warning about the dimensions of the vcov matrices??
		
	fixef.original <- model@fixef
	ranef.original <- ranef(model)[[1]]
	vcov.original <- as.matrix(vcov(model))
	# Should I name the dimensions here? or is it not necessary?
	
	val <- list(fixef.original = fixef.original, ranef.original = ranef.original, 
		vcov.original = vcov.original, fixef.delete = fixef.delete, 
		ranef.delete = ranef.delete, vcov.delete = vcov.delete,
		fitted.delete = fitted.delete)
	
	return(val)

}