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
# cols: colors used to plot each series.
# main: main title of the plots.
# leg.title: title of the legend, or NA if none.
# leg.pos: legend position (multiple series).
# las: orientation of axis labels (0:parallel to axis, 1:horizontal, 2:perpendicular to axis, 3:vertical)
# file: (optional) file name, to record the histogram plot.
# histo: whether to plot the histogram.
# ccdf: whether to plot the complement cumulative distribution function.
# test: whether to fit standard distribution to the values.
#############################################################
plot.disc.distribution <- function(vals, xlab, breaks="Sturges", log=FALSE, cols=NA, main=NA, leg.title=NA, leg.pos="topright", las=1, file=NA, histo=TRUE, ccdf=TRUE, test=FALSE)
{	# plot histo
	if(histo)
	{	# make plot
		plot.bars(
			vals=vals, breaks=breaks, 
			xlab=xlab, main=main, 
			cols=cols, 
			freq=FALSE, beside=TRUE, 
			leg.title=leg.title, leg.pos=leg.pos, 
			las=las,
			file=file
		)
	}
	
	# plot ccdv
	if(ccdf)
	{	# set param
		if(test)
			test <- "disc"
		else
			test <- NA
		# make plot
		plot.ccdf(
			data=vals, 
			xlab=xlab, main=main, 
			log=TRUE, cols=cols,
			leg.title=leg.title, leg.pos=leg.pos,
			las=las,
			file=file,
			test=test
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
# col: colors used to plot each series.
# main: main title of the plots.
# leg.title: title of the legend, or NA if none.
# leg.pos: legend position (multiple series).
# las: orientation of axis labels (0:parallel to axis, 1:horizontal, 2:perpendicular to axis, 3:vertical)
# file: (optional) file name, to record the histogram plot.
# histo: whether to plot the histogram.
# ccdf: whether to plot the complement cumulative distribution function.
# test: whether to fit standard distribution to the values.
#############################################################
plot.cont.distribution <- function(vals, xlab, breaks="Sturges", log=FALSE, cols=NA, main=NA, leg.title=NA, leg.pos="topright", las=1, file=NA, histo=TRUE, ccdf=TRUE, test=FALSE)
{	# plot histo
	if(histo)
	{	# make plot
		plot.hist(
			vals=vals, breaks=breaks, 
			xlab=xlab, main=main, 
			cols=cols, 
			freq=FALSE, 
			points=FALSE, line=FALSE,
			las=las,
			file=file
		)
	}
	
	# plot ccdv
	if(ccdf)
	{	# set param
		if(test)
			test <- "cont"
		else
			test <- NA
		# make plot
		plot.ccdf(
			data=vals, 
			xlab=xlab, main=main, 
			log=TRUE, cols=cols,
			leg.title=leg.title, leg.pos=leg.pos,
			las=las,
			file=file,
			test=test
		)
	}
	
	# fit distribution
	
}




#############################################################
# Plots a custom histogram.
#
# vals: data to plot. Can be a single series, or several as an array or a list.
# xlab: name of the values (used for the x-axis label).
# breaks: histogram breaks.
# cols: color used to plot each series.
# freq: plot frequencies (TRUE) vs. densities (FALSE).
# main: main title of the plots.
# file: (optional) file name, to record the histogram plot.
# points: adds scatter plots to the bars, representing the corresponding points.
# line: add density estimate.
# las: orientation of axis labels (0:parallel to axis, 1:horizontal, 2:perpendicular to axis, 3:vertical)
# ...: additional parameters, fetched to the hist function.
#############################################################
plot.hist <- function(vals, breaks="Sturges", xlab, main=NA, cols, freq=FALSE, points=FALSE, line=FALSE, las=1, file=NA, ...)
{	# prepare data
	if(!all(class(vals)=="list"))
	{	if(is.null(dim(vals)))
			vals <- list(vals)
		else
		{	cn <- colnames(vals)
			vals <- lapply(1:ncol(vals), function(c) vals[,c])
			names(vals) <- cn
		}
	}
	
	# possibly init colors
	if(all(is.na(cols)))
	{	if(length(vals)==1)
			cols <- MAIN_COLOR
		else
			cols <- get.palette(length(vals))
	}
	
	# plot each series separately
	for(s in 1:length(vals))
	{	# clean data
		data <- vals[[s]]
		data <- data[!is.na(data) & !is.nan(data) & !is.infinite(data)]
		if(length(data)>0 && length(unique(data))>1)
		{	# set params
			if(freq)
				ylab <- "Frequency"
			else
				ylab <- "Density"
			
			# set plot format
			if(is.na(file))
				fformats <- NA
			else
			{	plot.file <- paste0(file,"_histo")
				if(length(vals)>1)
					plot.file <- paste0(plot.file, "_", names(vals)[s])
				fformats <- PLOT_FORMAT
			}
			
			for(fformat in fformats)
			{	# possibly open plot file
				if(!is.na(fformat))
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(paste0(plot.file,fformat), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(paste0(plot.file,fformat), width=800, height=800, units="px", pointsize=20, bg="white")
				}
				
				# configure margins
	#			par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
				par(mar=c(5.1, 4.1, 4.1, 2.1))
				
				# draw histogram
				h <- hist(
					x=data,			# data
					col=cols[s],	# bars color
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
						density(data), 	# density estimate
						lwd=2, 			# line thickness
						col="BLACK"		# line color
					)
				}
				
				# add scatter plot
				if(freq)
					at <- max(h$counts)
				else
					at <- max(h$density)
				if(points)
				{	stripchart(
						data, 				# data
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
			tlog(0,"WARNING: could create plot (not enough values)")
	}
}




#############################################################
# Plots a custom barplot.
#
# vals: data to plot. Can be a single series, or several as an array or a list.
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
{	# prepare data
	if(!all(class(vals)=="list"))
	{	if(is.null(dim(vals)))
			vals <- list(vals)
		else
		{	cn <- colnames(vals)
			vals <- lapply(1:ncol(vals), function(c) vals[,c])
			names(vals) <- cn
		}
	}
	
	# possibly init colors
	if(all(is.na(cols)))
	{	if(ncol(vals)==1)
			cols <- MAIN_COLOR
		else
			cols <- get.palette(ncol(vals))
	}
	
	# init vars
	data <- NA
	if(freq)
		ylab <- "Frequency"
	else
		ylab <- "Density"
	
	# compute bar heights
	for(s in 1:length(vals))
	{	h <- hist(
			vals[[s]], 
			breaks=breaks, 	# min(vals):max(vals)
			plot=FALSE
		)
		if(all(is.na(data)))
			data <- matrix(NA, ncol=length(vals), nrow=length(h$counts))
		if(freq)
			data[,s] <- h$counts
		else
			data[,s] <- h$density
	}
	colnames(data) <- names(vals)
	
	# set file formats
	if(is.na(file))
		fformats <- NA
	else
	{	plot.file <- paste0(file, "_histo")
		fformats <- PLOT_FORMAT
	}
	
	# produce plot
	for(fformat in fformats)
	{	# possibly open plot file
		if(!is.na(fformat))
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(paste0(plot.file,fformat), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(paste0(plot.file,fformat), width=800, height=800, units="px", pointsize=20, bg="white")
		}
		
		# set margins
#		par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
		par(mar=c(5, 4, 1, 0)+0.1)
		
		# single series
		if(length(vals)==1)
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
				legend=names(vals),			# associated text
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
# data: data to plot. Can be a single series, or several as an array or a list.
# main: main title of the plot.
# xlab: label of the x-axis, or NA for no label.
# ylab: label of the y-axis, or "default" for default, or NA for no label at all.
# log: TRUE to use logarithmic scales for the axes.
# cols: color of each series.
# leg.title: title of the legend, or NA if none.
# leg.pos: position of the legend (by default, "topright").
# lines: line types, or NA if points.
# cex.axis: size of the axis ticks text.
# las: orientation of axis labels (0:parallel to axis, 1:horizontal, 2:perpendicular to axis, 3:vertical).
# file: file to create, or NA to plot in current output.
# test: whether to fit the distribution ("cont" or "disc") to standard laws, using Clauset et al.'s method. NA means no fitting at all.
# ...: additional parameters, fetched to the plot function.
#
# returns: TRUE iff a plot could be produced.
#############################################################################################
plot.ccdf <- function(data, xlab=NA, ylab="default", main=NA, log=FALSE, cols=NA, leg.title=NA, leg.pos="topright", lines=NA, cex.axis=1, las=1, file=NA, test=NA, ...)
{	# prepare data
	if(!all(class(data)=="list"))
	{	if(is.null(dim(data)))
			data <- list(data)
		else
		{	cn <- colnames(data)
			data <- lapply(1:ncol(data), function(c) data[,c])
			names(data) <- cn
		}
	}
	
	# possibly init colors
	if(all(is.na(cols)))
	{	if(length(data)==1)
			cols <- MAIN_COLOR
		else
			cols <- get.palette(length(data))
	}
	
	# init vars
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
	
	# possibly fit distribution
	if(!is.na(test))
	{	fits <- list()
		for(s in 1:length(data))
		{	plot.file <- NA
			if(!is.na(file))
			{	plot.file <- paste0(file, "_distr-test")
				if(length(data)>1)
					plot.file <- paste0(plot.file, "_", names(data)[s])
			}
			if(test=="disc")
				tmp <- test.disc.distr(data=data[[s]], xlab=xlab, return_stats=TRUE, plot.file=plot.file)
			else if(test=="cont")
				tmp <- test.cont.distr(data=data[[s]], xlab=xlab, return_stats=TRUE, plot.file=plot.file)
			if(!is.na(file))
				write.table(tmp$stats, file=paste0(plot.file,".csv"), sep=",", row.names=FALSE, col.names=TRUE)
			fits[[s]] <- tmp$laws
		}
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
	{	plot.file <- paste0(file, "_ccdf")
		fformats <- PLOT_FORMAT
	}
	
	for(fformat in fformats)
	{	# possibly open plot file
		if(!is.na(fformat))
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(paste0(plot.file,fformat), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(paste0(plot.file,fformat), width=800, height=800, units="px", pointsize=20, bg="white")
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
		
		# possibly add the fit lines
		law.order <- c("Power Law", "Truncated Power Law", "Log-Normal Law", "Exponential Law", "Poisson Law", "Weibull Law", "Yule-Simon Law")
		line.types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "42121")
		if(!is.na(test))
		{	# draw lines
			f.laws <- intersect(law.order, unique(unlist(lapply(fits, function(fit) if(all(is.na(fit))) NA else names(fit)))))
			f.lty <- line.types[1:length(f.laws)]
			for(s in 1:length(fits))
			{	fit <- fits[[s]]
				if(!all(is.na(fit)))
				{	for(f in 1:length(fit))
					{	law <- names(fit)[f]
						lt <- f.lty[which(f.laws==law)]
						if(length(fits)>1)
							scol <- cols[s]
						else
							scol <- "BLACK"
						add.line.plot(data=x[[s]], model=fit[[f]], col=scol, lty=lt)
					}
				}
			}
		}
		
		# add the legend
		if(!is.na(leg.title) && length(data)>1)
		{	# inclue fit lines in the legend
			if(!is.na(test) && length(data)>1)
			{	legend(
					x=leg.pos, 
					fill=c(cols,rep(NA,length(f.laws))),
					border=c(rep("BLACK",length(cols)), rep(NA, length(f.laws))),
					legend=c(names(data),f.laws), text.col="WHITE",
					title=leg.title, title.col="WHITE"
				)
				legend(
					x=leg.pos, 
					bty="n",		# no box around legend
					col=c(rep(NA,length(cols)), rep("BLACK",length(f.laws))),
					lty=c(rep(NA,length(cols)), f.lty),
					legend=c(names(data),f.laws),
#					legend=rep("", length(data)+length(f.laws)),
					title=leg.title, 
					seg.len=1
				)
			}
			# or not (just series)
			else
			{	legend(
					x=leg.pos, 
					fill=cols, 
					legend=names(data),
					title=leg.title
				)
			}
		}
		
		# close plot file
		if(!is.na(fformat))
			dev.off()
	}
	
	return(TRUE)
}




########################################################""
## single series
#vals <- sample(x=1:100,size=200,replace=TRUE)
#plot.hist(vals=vals, breaks=min(vals):max(vals), xlab="Xlab", main="Main title", col="RED", freq=FALSE, points=TRUE, line=TRUE, file=NA)
#plot.bars(vals=vals, breaks=min(vals):max(vals), xlab="XLab", main="Main title", cols="RED", freq=FALSE, beside=TRUE, leg.title="Legend", leg.pos="topright", file=NA)
#plot.ccdf(data=vals, xlab="XLab", main="Main Title", log=FALSE, cols="Red", leg.title="Legend", leg.pos="topright", las=1, file=NA, test="disc")
#plot.disc.distribution(vals=vals, xlab="Xlab", breaks=min(vals):max(vals), log=FALSE, cols="RED", main="Main title", leg.title="Legend", leg.pos="topright", las=1, file="Test", histo=TRUE, ccdf=TRUE, test=TRUE)
##
#vals <- runif(100)
#plot.hist(vals=vals, xlab="Xlab", main="Main title", col="RED", freq=FALSE, points=TRUE, line=TRUE, file=NA)
#plot.ccdf(data=vals, xlab="XLab", main="Main Title", log=FALSE, cols="Red", leg.title="Legend", leg.pos="topright", las=1, file=NA, test="cont")
#plot.cont.distribution(vals=vals, xlab="Xlab", log=FALSE, cols="RED", main="Main title", leg.title="Legend", leg.pos="topright", las=1, file="Test", histo=TRUE, ccdf=TRUE, test=TRUE)
##
## multiple series
#vals <- cbind(sample(x=1:10,size=200,replace=TRUE),sample(x=1:10,size=200,replace=TRUE),sample(x=1:10,size=200,replace=TRUE));colnames(vals) <- c("R","B","G")
#plot.hist(vals=vals, xlab="Xlab", main="Main title", col="RED", freq=FALSE, points=TRUE, line=TRUE, file="Test")
#plot.bars(vals=vals, breaks=min(vals):max(vals), xlab="XLab", main="Main title", cols=c("RED","BLUE","GREEN"), freq=FALSE, beside=TRUE, leg.title="Legend", leg.pos="topright", file=NA)
#plot.ccdf(data=vals, xlab="XLab", main="Main Title", log=FALSE, cols=c("RED","BLUE","GREEN"), leg.title="Legend", leg.pos="topright", las=1, file="Test", test="disc")
#plot.cont.distribution(vals=vals, xlab="Xlab", log=FALSE, cols=c("RED","BLUE","GREEN"), main="Main title", leg.title="Legend", leg.pos="topright", las=1, file="Test", histo=TRUE, ccdf=TRUE, test=TRUE)

