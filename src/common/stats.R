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
# data: data to plot. Can be a single series or a list of series.
# main: main title of the plot.
# xlab: label of the x-axis.
# log: TRUE to use logarithmic scales for the axes.
# cols: color of each series.
# leg.title: title of the legend, or NA of none.
# lines: line types, or NA if points.
#
# returns: TRUE iff a plot could be produced.
#############################################################################################
plot.ccdf <- function(data, main, xlab, log=FALSE, cols=NA, leg.title=NA, lines=NA)
{	# init vars
	if(all(is.na(cols)))
		cols <- rep(MAIN_COLOR, length(data))
	if(!class(data)=="list")
		data <- list(data)
	x <- list()
	y <- list()
	
	# compute ccdf
	for(s in 1:length(data))
	{	# compute cumulative function
		my_ecdf <- ecdf(data[[s]])
		
		# compute complementary cumulative function
		x[[s]] <- sort(unique(data[[s]]))
		y[[s]] <- 1 - my_ecdf(x[[s]])
	}
	
	# init the plot
	if(log)
	{	# compute ranges
		xs <- unlist(x)
		ys <- unlist(y)
		idx <- which(ys>0)
		xs <- xs[idx]
		ys <- ys[idx]
		idx <- which(xs>0)
		xs <- xs[idx]
		ys <- ys[idx]
		xlim <- range(xs)
		ylim <- range(ys)
		if(any(is.infinite(c(xlim, ylim))))
		{	tlog("WARNING: nothing to plot, all values are zero or infinite")
			return(FALSE)
		}
		
		# this is to plot proper powers of 10
		expmax <- floor(log(min(ylim[1]),10))
		
		# render the plot
		plot(
			NULL, 
			xlab=xlab, ylab="Complementary Cumulative Density", main=main,
			xlim=xlim, ylim=ylim,
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
			NULL, 
			xlab=xlab, ylab="Complementary Cumulative Density", main=main, 
			las=1
		)
	}
	
	# add the series
	for(s in 1:length(data))
	{	x.vals <- x[[s]]
		y.vals <- y[[s]]
		
		# remove zero values
		if(log)
		{	idx <- which(y.vals>0)
			x.vals <- x.vals[idx]
			y.vals <- y.vals[idx]
		}
		
		# add the series
		if(all(is.na(lines)))
		{	points(
				x.vals, y.vals,
				col=cols[s]
			)
		}
		else
		{	lines(
				x.vals, y.vals,
				col=cols[s],
				lty=lines[s],	# 0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash
			)
		}
	}
	
	# add legend
	if(!is.na(leg.title))
	{	legend(
			x="topright", 
			fill=cols, 
			legend=names(data),
			title=leg.title
		)
	}
	
	return(TRUE)
}

## density function
#data <- <my data>
#ml <- "main title"
#xl <- "x-axis title"
## histogram
#h <- hist(
#	data,
#	breaks=0:max(data),
##	col=MAIN_COLOR,
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
#plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy", yaxt="n") #las=1
#axis(side=2, at=10^(expmax:0), label=parse(text=paste("10^", expmax:0, sep="")), las=1)
