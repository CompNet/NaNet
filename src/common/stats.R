#############################################################################################
# Functions used to handle various statistical tasks.
# 
# 05/2021 Vincent Labatut
#############################################################################################
library("ercv")




#############################################################################################
# Computes the statistical mode for the specified sample.
# taken from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
#
# x: sample.
# na.rm: what to do with NA values.
#
# returns: the statistical mode(s).
#############################################################################################
stat.mode <- function(x, na.rm=FALSE)
{	if(na.rm)
		x = x[!is.na(x)]
	
	ux <- unique(x)
	tt <- tabulate(match(x, ux))
	idx <- which.max(tt)
	
	res <- ux[idx]
	return(res)
}




#############################################################################################
# Plots the complementary cumulative distribution function of the specified data.
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

## density function
#data <- <my data>
#ml <- "main title"
#xl <- "x-axis title"
## histogram
#h <- hist(
#	data,
#	breaks=0:max(data),
##	col="RED",
##	xlab=xl,
##	main=ml,
##	freq=FALSE,
#	plot=FALSE
#)
## scatterplot
#x <- h$breaks[2:length(h$breaks)]
#y <- h$density
#idx <- which(y>0)
#x <- x[idx]
#y <- y[idx]
#expmax <- floor(log(min(y),10))
#plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy", yaxt="n") #las=1
#axis(side=2, at=10^(expmax:0), label=parse(text=paste("10^", expmax:0, sep="")), las=1)
