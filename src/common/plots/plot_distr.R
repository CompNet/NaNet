#############################################################################################
# Functions used to produce custom plots.
# 
# 09/2019 Vincent Labatut
#
# source("src/common/plot/plot_stats.R")
#############################################################################################




#############################################################
# Generates all plots representing the distribution of a
# discrete variable: histogram, CCDF, and fit.
#
# vals: raw values (possibly several series, as columns).
# xlab: x-axis label.
# breaks: histogram breaks.
# log: scale of the ccdf axes.
# col: color used to plot the data (one per series).
# main: main title of the plots.
# leg.title: title of the legend, or NA if none.
# leg.pos: legend position (multiple series).
# las: orientation of axis labels (0:parallel to axis, 1:horizontal, 2:perpendicular to axis, 3:vertical)
# file: (optional) file name, to record the histogram plot.
# histo: whether to plot the histogram.
# ccdf: whether to plot the complement cumulative distribution function.
# test: whether to fit standard distribution to the values.
#############################################################
plot.disc.distribution <- function(vals, xlab, breaks="Sturges", log=FALSE, cols, main=NA, leg.title=NA, leg.pos="topright", las=1, file=NA, histo=TRUE, ccdf=TRUE, test=FALSE)
{	# plot histo
	if(histo)
	{	plot.bars(
			vals=vals, breaks=breaks, 
			xlab=xlab, main=main, 
			cols=cols, 
			freq=FALSE, beside=TRUE, 
			leg.title=leg.title, leg.pos=leg.pos, 
			las=las,
			file=paste0(file,"_histo")
		)
	}
	
	# plot ccdv
	if(ccdf)
	{	# prepare data
		if(is.null(dim(vals)))
			data <- list(vals)
		else
			data <- lapply(1:ncol(vals), function(c) vals[,c])
		# plot
		plot.ccdf(
			data=data, 
			xlab=xlab, main=main, 
			log=TRUE, cols=cols,
			leg.title=leg.title, leg.pos=leg.pos,
			las=las,
			file=paste0(file,"_ccdf")
		)
	}
	
	# fit distribution
	
}




#############################################################
# Generates all plots representing the distribution of a
# continuous variable: histogram, CCDF, and fit.
#
# vals: raw values (single series assumed).
# xlab: name of the values (used for the x-axis label).
# breaks: histogram breaks.
# log: scale of the ccdf axes.
# col: color used to plot the data.
# main: main title of the plots.
# las: orientation of axis labels (0:parallel to axis, 1:horizontal, 2:perpendicular to axis, 3:vertical)
# file: (optional) file name, to record the histogram plot.
# histo: whether to plot the histogram.
# ccdf: whether to plot the complement cumulative distribution function.
# test: whether to fit standard distribution to the values.
#############################################################
plot.cont.distribution <- function(vals, xlab, breaks="Sturges", log=FALSE, col, main=NA, las=1, file, histo=TRUE, ccdf=TRUE, test=FALSE)
{	
	# plot histo
	if(histo)
	{	plot.hist(
			vals=vals, breaks=breaks, 
			xlab=xlab, main=main, 
			col=col, 
			freq=FALSE, 
			points=FALSE, line=FALSE,
			las=las,
			file=paste0(file,"_histo")
		)
	}
	
	# plot ccdv
	if(ccdf)
	{	plot.ccdf(
			data=vals, 
			xlab=xl, ylab="default", main=main, 
			log=TRUE, cols=col,
			las=las,
			file=paste0(file,"_ccdf")
		)
	}
	
	# fit distribution
	
}




#############################################################
# Plots a custom histogram.
#
# vals: raw values.
# xlab: name of the values (used for the x-axis label).
# breaks: histogram breaks.
# col: color used to plot the data.
# freq: plot frequencies (TRUE) vs. densities (FALSE).
# main: main title of the plots.
# file: (optional) file name, to record the histogram plot.
# points: adds scatter plots to the bars, representing the corresponding points.
# line: add density estimate.
# las: orientation of axis labels (0:parallel to axis, 1:horizontal, 2:perpendicular to axis, 3:vertical)
# ...: additional parameters, fetched to the hist function.
#############################################################
plot.hist <- function(vals, breaks="Sturges", xlab, main=NA, col, freq=FALSE, points=FALSE, line=FALSE, las=1, file=NA, ...)
{	vals <- vals[!is.na(vals) & !is.nan(vals) & !is.infinite(vals)]
	if(length(vals)>0 && length(unique(vals))>1)
	{	# set params
		if(freq)
			ylab <- "Frequency"
		else
			ylab <- "Density"
		
		# set plot format
		if(is.na(file))
			fformats <- NA
		else
			fformats <- PLOT_FORMAT
		
		for(fformat in fformats)
		{	# possibly open plot file
			if(!is.na(fformat))
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(paste0(file,fformat), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(paste0(file,fformat), width=800, height=800, units="px", pointsize=20, bg="white")
			}
			
			# configure margins
#			par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
			par(mar=c(5.1, 4.1, 4.1, 2.1))
			
			# draw histogram
			h <- hist(
				x=vals,			# data
				col=col,		# bars color
				main=main,		# main title
				freq=freq,		# frenquency density
				breaks=breaks,	# number of bars
				xlab=xlab,		# x-axis label
				ylab=ylab,		# y-axis label
				las=las,		# axis label orientation
				...
			)
			
			# add density estimate
			if(line && !freq)
			{	lines(
					density(vals), 	# density estimate
					lwd=2, 			# line thickness
					col=col			# line color
				)
			}
			
			# add scatter plot
			if(freq)
				at <- max(h$counts)
			else
				at <- max(h$density)
			if(points)
			{	stripchart(
					vals, 				# data
					at=0.5*at, 			# central position of points (y)
					pch=21, 			# point shape
					col="BLACK", 		# point color
					method="jitter",	# noise to avoid overlaps
					jitter=0.5*at, 		# noise magnitude
					add=TRUE			# add to current plot
				)
			}
			
			# close plot file
			if(!is.na(fformat))
				dev.off()
		}
	}
	else
		tlog(0,"WARNING: could not plot \"",file,"\" (not enough values)")
}




#############################################################
# Plots a custom barplot.
#
# vals: raw values.
# breaks: histogram breaks.
# xlab: label of the x-axis.
# main: plot title.
# cols: colors (one for each series).
# freq: plot frequencies (TRUE) vs. densities (FALSE).
# beside: whether to plot multiple series as stacked (FALSE) or groupped (TRUE) bars.
# leg.title: title of the legend, or NA if none.
# leg.pos: legend position (multiple series).
# las: orientation of axis labels (0:parallel to axis, 1:horizontal, 2:perpendicular to axis, 3:vertical)
# file: (optional) file name, to record the histogram plot.
# ...: additional parameters, fetched to the barplot function.
#############################################################
plot.bars <- function(vals, breaks, xlab, main=NA, cols=NA, freq=FALSE, beside=TRUE, leg.title=NA, leg.pos="topright", las=1, file=NA, ...)
{	# init vars
	if(is.null(dim(vals)))
		vals <- matrix(vals,ncol=1)
	if(all(is.na(cols)))
	{	if(ncol(vals)==1)
			cols <- MAIN_COLOR
		else
			cols <- get.palette(ncol(vals))
	}
	
	# compute bar heights
	data <- NA
	for(d in 1:ncol(vals))
	{	h <- hist(
			vals[,d], 
			breaks=breaks, 	# min(vals):max(vals)
			plot=FALSE
		)
		if(all(is.na(data)))
			data <- matrix(NA, ncol=ncol(vals), nrow=length(h$counts))
		if(freq)
			data[,d] <- h$counts
		else
			data[,d] <- h$density
	}
	
	# set params
	if(freq)
		ylab <- "Frequency"
	else
		ylab <- "Density"
	
	# set file formats
	if(is.na(file))
		fformats <- NA
	else
		fformats <- PLOT_FORMAT
	
	# produce plot
	for(fformat in fformats)
	{	# possibly open plot file
		if(!is.na(fformat))
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(paste0(file,fformat), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(paste0(file,fformat), width=800, height=800, units="px", pointsize=20, bg="white")
		}
		
		# set margins
#		par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
		par(mar=c(5, 4, 1, 0)+0.1)
		
		# single series
		if(ncol(vals)==1)
		{	# barplot
			barplot(
				height=c(data),							# data
				names.arg=h$breaks[2:length(h$breaks)],	# bar names
				xlab=xlab, ylab=ylab, main=main,		# labels
				col=cols,								# bar colors
				space=0,								# space between bars
				las=las,								# axis label orientation
				...
			)
		}
		# multiple series
		else
		{	if(beside)
				space <- c(0,1)
			else
				space <- 0
			# barplot
			barplot(
				height=t(data),							# data
				names.arg=h$breaks[2:length(h$breaks)],	# bar names
				xlab=xlab, ylab=ylab, main=main,		# labels
				col=cols,								# bar colors
				space=space,							# space between bars
				beside=beside,							# grouped/stacked bars
				las=las,								# axis label orientation
				...
			)
			# add legend
			legend(
				x=leg.pos,						# position of the legend
				fill=cols,						# fill colors
				legend=colnames(vals),			# associated text
				title=leg.title					# main title
			)
		}
		
		# possibly close plot file
		if(!is.na(fformat))
			dev.off()
	}
}




#############################################################################################
# Plots the complementary cumulative distribution function of the specified data.
# 
# Note: it is normal if the series do not start from zero.
# 
# data: data to plot. Can be a single series or a list of series.
# main: main title of the plot.
# xlab: label of the x-axis, or NA for no label.
# ylab: label of the y-axis, or "default" for default, or NA for no label at all.
# log: TRUE to use logarithmic scales for the axes.
# cols: color of each series.
# leg.title: title of the legend, or NA if none.
# leg.pos: position of the legend (by default, "topright").
# lines: line types, or NA if points.
# cex.axis: size of the axis ticks text.
# las: orientation of axis labels (0:parallel to axis, 1:horizontal, 2:perpendicular to axis, 3:vertical)
# file: file to create, or NA to plot in current output.
# ...: additional parameters, fetched to the plot function.
#
# returns: TRUE iff a plot could be produced.
#############################################################################################
plot.ccdf <- function(data, xlab=NA, ylab="default", main=NA, log=FALSE, cols=NA, leg.title=NA, leg.pos="topright", lines=NA, cex.axis=1, las=1, file=NA, ...)
{	# init vars
	if(!class(data)=="list")
		data <- list(data)
	if(all(is.na(cols)))
	{	if(length(data)==1)
			cols <- MAIN_COLOR
		else
			cols <- get.palette(length(data))
	}
	x <- list()
	y <- list()
	if(!is.na(ylab) && ylab=="default")
		ylab <- "Complementary Cumulative Density"
	
	# compute ccdf
	for(s in 1:length(data))
	{	# compute cumulative function
		ecdf.foo <- ecdf(data[[s]])
		
		# compute complementary cumulative function
		x[[s]] <- sort(unique(data[[s]]))
		y[[s]] <- 1 - ecdf.foo(x[[s]])
	}
	
	# compute ranges
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
	
	# init formats
	if(is.na(file))
		fformats <- NA
	else
		fformats <- PLOT_FORMAT
	
	for(fformat in fformats)
	{	# possibly open plot file
		if(!is.na(fformat))
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(paste0(file,fformat), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(paste0(file,fformat), width=800, height=800, units="px", pointsize=20, bg="white")
		}
			
		# set margins
		par(mar=c(4, 4, 0.3, 0.3))		# remove the title space Bottom Left Top Right
		
		# init the plot
		if(log)
		{	# this is to plot proper powers of 10
			xexpmax <- floor(log(min(xlim[1]),10))
			yexpmax <- floor(log(min(ylim[1]),10))
			
			# render the plot
			plot(
				NULL, 
				xlab=xlab, ylab=ylab, main=main,
				xlim=xlim, ylim=ylim,
				log="xy", 
				xaxt="n", yaxt="n",
				cex.axis=cex.axis,
				...
			)
			# render the x-axis
			if(xlim[2]/xlim[1]<100)
				axis(side=1)
			else
			{	eaxis(
					side=1, 
					n.axp=1,
					cex.axis=cex.axis
				)
#				axis(
#					side=1,
#					at=10^(xexpmax:0), 
#					label=parse(text=paste("10^", xexpmax:0, sep="")), 
#					cex.axis=cex.axis
#				)
			}
			# render the y-axis
			axis(
				side=2, 
				at=10^(yexpmax:0), 
				label=parse(text=paste("10^", yexpmax:0, sep="")), 
				las=las,
				cex.axis=cex.axis
			)
		}
		else
		{	# render the plot
			plot(
				NULL, 
				xlab=xlab, ylab=ylab, main=main, 
				xlim=xlim, ylim=ylim,
				las=las,
				cex.axis=cex.axis,
				...
			)
		}
		
		# add the series
		for(s in 1:length(data))
		{	x.vals <- x[[s]]
			y.vals <- y[[s]]
			
			# possibly remove zero values
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
		
		# add the legend
		if(!is.na(leg.title) && length(data)>1)
		{	legend(
				x=leg.pos, 
				fill=cols, 
				legend=names(data),
				title=leg.title
			)
		}
		
		# close plot file
		if(!is.na(fformat))
			dev.off()
	}
	
	return(TRUE)
}




########################################################""
# tests
#vals=sample(x=1:100,size=200,replace=TRUE)
#plot.hist(vals=vals, breaks=min(vals):max(vals), xlab="Xlab", main="Main title", col="RED", freq=FALSE, points=TRUE, line=TRUE, file=NA)
#plot.bars(vals=vals, breaks=min(vals):max(vals), xlab="XLab", main="Main title", cols="RED", freq=FALSE, beside=TRUE, leg.title="Legend", leg.pos="topright", file=NA)
#plot.ccdf(data=vals, xlab="XLab", main="Main Title", log=FALSE, cols="Red", leg.title="Legend", leg.pos="topright", las=1, file=NA)
#plot.disc.distribution(vals=vals, xlab="Xlab", breaks=min(vals):max(vals), log=FALSE, cols="RED", main="Main title", leg.title="Legend", leg.pos="topright", las=1, file=NA, histo=TRUE, ccdf=TRUE, test=FALSE)
#
#vals=cbind(sample(x=1:10,size=200,replace=TRUE),sample(x=1:10,size=200,replace=TRUE),sample(x=1:10,size=200,replace=TRUE));colnames(vals) <- c("R","B","G")
#plot.bars(vals=vals, breaks=min(vals):max(vals), xlab="XLab", main="Main title", cols=c("RED","BLUE","GREEN"), freq=FALSE, beside=TRUE, leg.title="Legend", leg.pos="topright", file=NA)
#data <- lapply(1:ncol(vals),function(c) vals[,c]); names(data) <- colnames(vals)
#plot.ccdf(data=data, xlab="XLab", main="Main Title", log=FALSE, cols=c("RED","BLUE","GREEN"), leg.title="Legend", leg.pos="topright", las=1, file=NA)
