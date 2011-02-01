#' Organizing the random effects found in case-deletion
#'
#' This function creates a data frame that will be used by the 'plot.group.est' function.
#' It checks to make sure that the levels of the ID factor are defined identically
#' across columns of the data set and fills in rows for the deleted cases so that 
#' the data can be plotted. 
#'
#' @param delete an object containing the output from the 'ranef.delete' matrix returned by the case.delete function
#' @param group the grouping factor
#' @author Adam Loy \email{aloy@@iastate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' wages.del2 <- case.delete2(wages.fm1, group = "id")
#' group.estimate.data(delete = wages.del2, group = "id")
group_estimate_data <- function(delete, group){
# CHANGED: formerly group.estimate.data
	
	estimate.df <- delete
	
	# filling in the missing rows for the deleted ids
	missing.rows <- function(df){
		del.ID <- unique(df$deleted)
		del.row <- data.frame(del.ID, del.ID)
		colnames(del.row) <- c("deleted", group)
		return(rbind.fill(df, del.row))
	}
	
	estimate.df <- ddply(estimate.df, .(deleted), missing.rows)
	
	# Defining the levels in the same manner
	estimate.df$deleted <- as.numeric(levels(estimate.df$deleted)[estimate.df$deleted])
	estimate.df[, group] <- as.numeric(levels(estimate.df[, group])[estimate.df[, group]])
	estimate.df$deleted <- factor(estimate.df$deleted)
	estimate.df[, group] <- factor(estimate.df[, group])

	return(estimate.df)
}

#' Ordering the random effects
#'
#' This function returns the order of the data frame of random effects found through case-
#' deletion so that the plot produced by the 'plot.est.group' function will display the 
#' estimated ordered by the mean estimate.
#'
#' @param data an object containing the output from the 'group.estimate.data' function
#' @param group the grouping factor
#' @param estimate estimate the random effect to be plotted
#' @author Adam Loy \email{aloy@@iastate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' wages.del2 <- case.delete2(wages.fm1, group = "id")
#' dat <- group.estimate.data(delete = wages.del2, group = "id")
#' order.group.est(data = dat, group = "id", estimate = "exper")
order_group_est <- function(data, group, estimate){
# CHANGED: formerly order.group.est
	force(estimate)
	# order.by <- ddply(data, group, summarise, mean.est = mean(estimate, na.rm = T))
	order.by <- ddply(data, group, function(df) data.frame(mean.est = mean(df[,estimate], na.rm = TRUE)))
	
	ordered.data <- order(order.by$mean.est, decreasing = TRUE)
		
	return(ordered.data)	
}

#' Ordering the random effects
#'
#' This function creates a plot that allows the user to assess the influence of case
#' deletion on the estimation of the random effects.
#'
#' @param delete an object containing the output from the 'ranef.delete' matrix returned by the case.delete function
#' @param group the grouping factor
#' @param estimate the random effect to be plotted
#' @param ... other arguments to be passed to qplot
#' @author Adam Loy \email{aloy@@iastate.edu}
#' @examples
#' wages.fm1 <- lmer(lnw ~ exper + (exper | id), data = wages)
#' wages.del2 <- case.delete2(wages.fm1, group = "id")
#' small.del <- subset(subset(wages.del2$ranef.delete, id %in% sample(unique(wages.else$id), 100)))
#' plot.group.est(delete = small.del, group = "id", estimate = "exper", xlab = "deleted group", ylab = "exper")
plot_group_est <- function(delete, group, estimate, ...){
# CHANGED: formerly plot.group.est
	conditional.est <- group_estimate_data(delete, group)
	ordered.data <- order_group_est(data = conditional.est, group = group, estimate = estimate)
	
	p <- qplot(x = factor(deleted, levels(deleted)[ordered.data]), y = conditional.est[,estimate], data = conditional.est, geom = "line", group = get(group), colour = get(group), ...) + 
		coord_flip() + opts(legend.position = "none")
		
	return(p)	
}