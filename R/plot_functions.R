#' Dot plots for influence diagnostics
#'
#' This is a function that can be used to create dotplots for the diagnostic measures.
#'
#' @param diag.out an object containing the output from the diagnostics function.
#' @param type specification of which diagnostic to plot (either COOKSD, MDFFITS, COVTRACE, COVRATIO).
#' @param cutoff value specifying unusual values of the diagnostic
#' @param label variable specifying group identity
#' @param ... other arguments to be passed to qplot
#' @author Adam Loy \email{aloy@@istate.edu}
#' @examples 
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' wages.fm1.del <- case.delete(wages.fm1, group = "id")
#' wages.fm1.diag <- diagnostics(model = wages.fm1, delete = wages.fm1.del, type = "fixef")
#' dotplot.diag(diag.out = wages.fm1.diag, type = "cooksd", cutoff = "internal")
#' dotplot.diag(diag.out = wages.fm1.diag, type = "mdffits", cutoff = "internal")
#' dotplot.diag(diag.out = wages.fm1.diag, type = "covtrace", cutoff = "internal")
#' dotplot.diag(diag.out = wages.fm1.diag, type = "covratio", cutoff = "internal")
dotplot_diag <- function(diag.out, type, cutoff = NULL, ... ){
# CHANGED: formerly dotplot.diag
	type <- toupper(type)
	
	p <- qplot(x = reorder(IDS, get(type)), y = get(type), data = diag.out, geom = "blank", ... )
	
	if(cutoff == "internal"){cutoff <- internal_cutoff(diag.out=diag.out, type=type)}
	
	if(is.numeric(cutoff)){
		
		if(type != "COVRATIO"){
			diag.out$extreme <- with(diag.out, get(type) > cutoff)
			
			if(sum(diag.out$extreme) > 0){
			p <- p + geom_point(data = subset(diag.out, extreme == TRUE), colour = I("red"), shape = 17)
			}
			p +
			geom_point(data = subset(diag.out, extreme == FALSE), colour = I("blue")) + 
			geom_hline(aes(yintercept = cutoff), colour=I("red")) + 
			geom_text(data = subset(diag.out, extreme == TRUE), aes(label = IDS, hjust=.5, vjust=1.5, size=3)) + 
			opts(legend.position = "none") +
			coord_flip()
		}
		
		else{
			diag.out$extreme <- with(diag.out, get(type) < cutoff[1] | get(type) > cutoff[2])

			if(sum(diag.out$extreme) > 0){
			p <- p + geom_point(data = subset(diag.out, extreme == TRUE), colour = I("red"), shape = 17)
			}
			p + 
			geom_point(data = subset(diag.out, extreme == FALSE), colour = I("blue")) + 
			geom_hline(aes(yintercept = cutoff[1]), colour=I("red")) + 
			geom_hline(aes(yintercept = cutoff[2]), colour=I("red")) + 
			geom_text(data = subset(diag.out, extreme == TRUE), aes(label = IDS, hjust=.5, vjust=1.5, size=3)) + 
			opts(legend.position = "none") +
			coord_flip()	
		}
		
	}
	
	else{
		p + geom_point()+ coord_flip()
	}
}


#' Calculating a cutoff value for diagnostic measures
#'
#' This function provides cutoff values using internal scaling. 
#' In other words, a measure of relative standing is used (3*IQR)
#' to specify unusual values of the diagnostic of interest relative
#' to the vector of diagnostics.
#'
#' @param diag.out an object containing the output from the diagnostics function.
#' @param type specification of which diagnostic to plot (either COOKSD, MDFFITS, COVTRACE, COVRATIO).
#' @author Adam Loy \email{aloy@@istate.edu}
internal_cutoff <- function(diag.out, type){
# CHANGED: formerly internal.cutoff
	#series <- subset(diag.out, select = toupper(type))
	series <- diag.out[, toupper(type)]
	q3 <- quantile(series, p=0.75)
	series.iqr <- IQR(series)
	
	if(toupper(type) == "COVRATIO"){
		q1 <- quantile(series, p=0.25)
		cutoff <- c(lower = q1 - 3*series.iqr, upper = q3 + 3*series.iqr)	
	}
	
	else{cutoff <- q3 + 3*series.iqr}
	
	return(cutoff)
	
}

#' Plotting the fixed effects estimates for mixed and hierarchical linear models
#'
#' This function plots the fixed effects from a linear mixed or hierarchical linear
#' models that were found through case-deletion. The plot will be facetted by the 
#' fixed effect where the scales are free, so that it is possible to see how the
#' estimates vary as groups are deleted.
#'
#' @param delete an object containing the output returned by the case.delete function
#' @param conf if TRUE will plot 95% normal confidence bands (these are inappropriate).
#' @param ... other arguments to be passed to qplot
#' @author Adam Loy \email{aloy@@istate.edu}
# plot.fixef1 <- function(delete, conf = TRUE, ... ){
#	datframe <- data.frame(IDS = as.vector(rownames(delete$fixef.delete)), delete$fixef.delete)
#	datframe <- melt(datframe, id.vars = "IDS", measure.vars = setdiff(colnames(datframe), "IDS"))
	
#	original <- melt(delete$fixef.original, measure.vars = colnames(delete$fixef.original))
	# print(rownames(original))
	# p <- qplot(x=IDS, y=value, data = datframe, geom="line", group = variable, colour = variable, ...)
#	p <- ggplot(data = datframe, aes(x = IDS, y = value, colour = factor(variable), group = variable)) +  
#			facet_grid(variable ~ ., scales = "free") + 
			# geom_hline(data = original, aes(yintercept = value, alpha = 0.4)) +
			# geom_ribbon()
#			geom_line() + 
			# coord_flip()
#			opts(legend.position = "none") + 
#			opts(axis.text.x = theme_text(angle = 90))
	
	
#	if(conf == FALSE) print(p)
#	else{
#		confint.data <- norm.confint.fixef(delete)
#		p <- ggplot(data = confint.data, aes(x = factor(IDS), y = estimate, colour = factor(parameter), group = parameter)) +  
#		facet_grid(parameter ~ ., scales = "free") +  
#		opts(legend.position = "none") + 
#		opts(axis.text.x = theme_text(angle = 90)) + 
#		geom_ribbon(data = confint.data, aes(ymin = lower, ymax = upper, alpha = 0.5, linetype=0)) + 
#		geom_line()
#		print(p)	
#	} 
#}

#' Calculating 95% Normal Confidence Intervals for fixed effects estimates
#'
#' This function calculates the upper and lower bounds for the (inappropriate)
#' 95% confidence intervals for the fixed effects estimates. These should only
#' be used as very rough guidelines, and if actual intervals are of interest
#' one should either look into defining the appropriate reference distribution
#' or using another method such as profiling or bootstrapping.
#'
#' @param delete an object containing the output returned by the case.delete function
#' @author Adam Loy \email{aloy@@istate.edu}
#norm.confint.fixef <- function(delete){
	# Arguments:
	# delete: the output from the function 'case.delete'
	
	# Function:
#	fixed.effects <- melt(delete$fixef.delete)
#	colnames(fixed.effects) <- c("IDS", "parameter", "estimate")
#	vcov.fixed <- delete$vcov.delete
#	se.fixed <- melt( ldply(vcov.fixed, function(x){sqrt(diag(x))}) )
#	colnames(se.fixed) <- c("parameter", "se")
	
#	fixed.effects.df <- cbind(fixed.effects, se=se.fixed$se)
	
#	fixed.effects.df <- transform(fixed.effects.df, lower = estimate - 2 * se, upper = estimate + 2 * se)
	
#	return(fixed.effects.df)
#}


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

# Function Description:
# This function provides the recommended cutoff values for each diagnostic.

# Citations:

# auto_cutoff <- function(delete, type){
# CHANGED: formerly auto_cutoff
	# Arguments:
	# delete: the output from the function 'case.delete'
	# type: which diagnostic to plot (COOKSD, MDFFITS, COVTRACE, or COVRATIO)
	
	# Function:
#	n.fixef <- length(delete$fixef.original)
#	n.group <- dim(delete$fixef.delete)[1]
#	type <- toupper(type)
#	
#	cutoff <- switch(type, 
#					COOKSD = 4/n.group,
#					MDFFITS = 2*sqrt(n.fixef/n.group),
#					COVTRACE = NULL,
#					COVRATIO = c(1 - 3*(n.fixef/n.group), 1 + 3*(n.fixef/n.group)))
#	
#	return(cutoff)
#}