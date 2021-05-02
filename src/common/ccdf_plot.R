#############################################################################################
# Functions used to handle various standard plots.
# 
# 05/2021 Vincent Labatut
#############################################################################################
library("ercv")




#############################################################################################
# Plots the complementary cumulated distribution function of the specifie data.
#
# data: data to plot.
# log: TRUE to use logarithmic scales for the axes.
#############################################################################################
plot.ccdf <- function(data, main, xlab, log=FALSE)
{	# compute cumulative function
	my_ecdf <- ecdf(data)
	
	# compute complementary cumulative function
	x <- sort(unique(data))
	y <- 1 - my_ecdf(x) 
	
	# only if log scale
	if(log)
	{	# remove zero values
		idx <- which(y>0)
		x <- x[idx]
		y <- y[idx]

		# this is to plot proper powers of 10
		expmax <- floor(log(min(y),10))
	
		# render the plot
		plot(
			x, y, 
			col="RED", 
			xlab=xlab, 
			ylab="Complementary Cumulative Density", 
			main=main, 
			log="xy", 
			yaxt="n"
		)
		# render the y-axis
		axis(
			side=2, 
			at=10^(expmax:0), 
			label=parse(text=paste("10^", expmax:0, sep="")), 
			las=1
		)
	}
	else
	{	# render the plot
		plot(
			x, y, 
			col="RED", 
			xlab=xlab, 
			ylab="Complementary Cumulative Density", 
			main=main, 
			las=1
		)
	}
}
