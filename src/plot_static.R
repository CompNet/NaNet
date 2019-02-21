# This script contains functions related to the generation of plots representing
# the previously computed statistics.
# 
# Vincent Labatut
# 02/2019
###############################################################################


# graph measures based on assortativity
assort.groups <- list(
		c("betweenness", "betweenness-norm", "betweenness-weighted", "betweenness-weighted-norm"),
		c("closeness", "closeness-norm", "closeness-weighted", "closeness-weighted-norm"),
		c("degree", "degree-norm", "strength"),
		c("eccentricity"),
		c("eigenvector", "eigenvector-norm", "eigenvector-weighted", "eigenvector-weighted-norm"),
		c("transitivity-local", "transitivity-weighted-local")
)
assort.suffix <- "-assortativity"
# graph measures based on centralization
ctrlztn.group <- list(
		c("betweenness", "betweenness-norm"),
		c("closeness", "closeness-norm"),
		c("degree", "degree-norm"),
		c("eigenvector", "eigenvector-norm")
)
ctrlztn.suffix <- "-centralization"
# single graph measures
single.group <- c("modularity", "community-number", "modularity-weighted", "community-weighted-number", "component-number",
		"node-count", "link-count", "density", "transitivity-global"
)



###############################################################################
# Colors used in the plots.
# Taken from http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
###############################################################################
COLORS <- c(
		rgb(228,26,28,maxColorValue=255),
		rgb(55,126,184,maxColorValue=255),
		rgb(77,175,74,maxColorValue=255),
		rgb(152,78,163,maxColorValue=255),
		rgb(255,127,0,maxColorValue=255),
#		rgb(255,255,51,maxColorValue=255),	# yellow
		rgb(166,86,40,maxColorValue=255),
		rgb(247,129,191,maxColorValue=255),
		rgb(153,153,153,maxColorValue=255),
		rgb(0,0,0,maxColorValue=255)
)



###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "segments", "panel.window" or "page.window".
# window.size: fixed value for this parameter.
# overlaps: vector of values for this parameter.
# measure: name of the concerned topological measure.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.by.window <- function(mode, window.size, overlaps, measure)
{	res <- rep(NA, length(overlaps))
	for(j in 1:length(overlaps))
	{	overlap <- overlaps[j]
		table.file <- get.statname.static(object="graph", mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[j] <- tmp.tab[measure,1]
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "segments", "panel.window" or "page.window".
# window.sizes: vector of values for this parameter.
# overlap: fixed value for this parameter.
# measure: name of the concerned topological measure.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.by.overlap <- function(mode, window.sizes, overlap, measure)
{	res <- rep(NA, length(window.sizes))
	for(i in 1:length(window.sizes))
	{	window.size <- window.sizes[i]
		table.file <- get.statname.static(object="graph", mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[i] <- tmp.tab[measure,1]
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# measure: name of the concerned topological measure.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.segments <- function(weights, measure)
{	table.file <- get.statname.static(object="graph", mode="segments", weights=weights)
	tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	res <- tmp.tab[measure,1]
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters: fixed window size,
# varying overlap.
#
# object: either "nodes" or "links" (not "graph").
# mode: either "panel.window" or "page.window" (not "segments").
# window.size: fixed value for this parameter.
# overlaps: vector of values for this parameter.
# measure: name of the concerned topological measure.
#
# returns: a list of vectors, each one representing the link/node values for
#          one parameter set.
###############################################################################
load.static.nodelink.stats.by.window <- function(object, mode, window.size, overlaps, measure)
{	res <- list()
	for(j in 1:length(overlaps))
	{	overlap <- overlaps[j]
		table.file <- get.statname.static(object=object, mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
		values <- tmp.tab[,measure]
		res[[j]] <- values
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters: varying window size,
# fixed overlap.
#
# object: either "nodes" or "links" (not "graph").
# mode: either "panel.window" or "page.window" (not "segments").
# window.sizes: vector of values for this parameter.
# overlap: fixed value for this parameter.
# measure: name of the concerned topological measure.
#
# returns: a list of vectors, each one representing the link/node values for
#          one parameter set.
###############################################################################
load.static.nodelink.stats.by.overlap <- function(object, mode, window.sizes, overlap, measure)
{	res <- list()
	for(i in 1:length(window.sizes))
	{	window.size <- window.sizes[i]
		table.file <- get.statname.static(object=object, mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
		values <- tmp.tab[,measure]
		res[[i]] <- values
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the segment-based graph.
#
# object: either "nodes" or "links" (not "graph").
# measure: name of the concerned topological measure.
# weights: either "occurrences" or "duration".
#
# returns: a vector representing the link/node values for the specified measure.
###############################################################################
load.static.nodelink.stats.segments <- function(object, measure, weights)
{	table.file <- get.statname.static(object=object, mode="segments", weights=weights)
	tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
	res <- tmp.tab[,measure]
	return(res)
}


###############################################################################
# Generates the plots containing a single series as boxplots.
#
# mode: either "panel.window" or "page.window" (not "segments").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
###############################################################################
generate.static.graph.plots.single <- function(mode, window.sizes, overlaps)
{	
## Old version : just error bars	
#	# process each appropriate measure
#	for(meas.name in ASMM_MEASURES)
#	{	# generate a plot for each window size value
#		for(window.size in window.sizes)
#		{	# the series corresponds to the values of the overlap
#			avg.vals <- load.static.graph.stats.by.window(mode, window.size, overlaps, paste0(meas.name,SFX_AVG))
#			std.vals <- load.static.graph.stats.by.window(mode, window.size, overlaps, paste0(meas.name,SFX_STDEV))
#			min.vals <- load.static.graph.stats.by.window(mode, window.size, overlaps, paste0(meas.name,SFX_MIN))
#			max.vals <- load.static.graph.stats.by.window(mode, window.size, overlaps, paste0(meas.name,SFX_MAX))
#			
#			# generate the stdev plot
#			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, window.size=window.size),"_stdev.pdf")
#			pdf(file=plot.file,bg="white")
#				plot(x=overlaps, y=avg.vals,
#						ylim=c(min(avg.vals-std.vals),max(avg.vals+std.vals)),
#						xlab="Overlap",
#						ylab=meas.name
#				)
#				segments(overlaps, avg.vals-std.vals, overlaps, avg.vals+std.vals)
#				epsilon <- 0.02
#				segments(overlaps-epsilon, avg.vals-std.vals, overlaps+epsilon, avg.vals-std.vals)
#				segments(overlaps-epsilon, avg.vals+std.vals, overlaps+epsilon, avg.vals+std.vals)
#			dev.off()
#			
#			# generate the min/max plot
#			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, window.size=window.size),"_minmax.pdf")
#			pdf(file=plot.file,bg="white")
#				plot(x=overlaps, y=avg.vals,
#						ylim=c(min(avg.vals-min.vals),max(avg.vals+max.vals)),
#						xlab="Overlap",
#						ylab=meas.name
#				)
#				segments(overlaps, avg.vals-min.vals, overlaps, avg.vals+max.vals)
#				epsilon <- 0.02
#				segments(overlaps-epsilon, avg.vals-min.vals, overlaps+epsilon, avg.vals-min.vals)
#				segments(overlaps-epsilon, avg.vals+max.vals, overlaps+epsilon, avg.vals+max.vals)
#			dev.off()
#		}
#	}

## new version: boxplots
	# setup measure name lists
	nmn <- names(NODE_MEASURES)
	lmn <- names(LINK_MEASURES)
	# identify common overlap values (over window sizes)
#	tmp <- table(unlist(overlaps))
#	common.overlaps <- as.integer(names(tmp)[which(tmp>1)])
	common.overlaps <- sort(unique(unlist(overlaps)))	# finally, don't remove values occurring just once
	# process each appropriate measure
	for(meas.name in c(nmn,lmn))
	{	tlog(4,"Generating single plots for measure ",meas.name," (mode=",mode,")")
		
		if(meas.name %in% nmn)
			object <- "nodes"
		else
			object <- "links"
		
		# load the reference values (segment-based graph)
		seg.occ.vals <- load.static.nodelink.stats.segments(object=object, measure=meas.name, weights="occurrences")
		seg.dur.vals <- load.static.nodelink.stats.segments(object=object, measure=meas.name, weights="duration")
		seg.vals <- list()
		seg.vals[[1]] <- seg.occ.vals
		seg.vals[[2]] <- seg.dur.vals
	
		# generate a plot for each window size value
		for(i in 1:length(window.sizes))
		{	# the series corresponds to the values of the overlap
			window.size <- window.sizes[i]
			tlog(5,"Dealing with window.size=",window.size)
			values <- load.static.nodelink.stats.by.window(object=object, mode=mode, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name)
			values <- c(seg.vals, values)
			
			nms <- overlaps[[i]]
			nms <- c("SO","SD",nms)
			
			# generate the stdev plot
			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, window.size=window.size),"_",meas.name,"_boxplot.png")
			tlog(5,"Plotting file \"",plot.file,"\"")
#			pdf(file=plot.file,bg="white")
			png(filename=plot.file,width=800,height=800,units="px",pointsize=20,bg="white")
				bp <- boxplot(x=values, 
					outline=FALSE,
					names=nms,
					plot=FALSE
				)
				bp$stats[1,] <- sapply(values, min)	# replace bottom whisker by min
				bp$stats[5,] <- sapply(values, max) # replace top whisker by max
				bxp(bp, 
					outline=FALSE,
					xlab="Overlap",
					ylab=meas.name,
					main=paste0("mode=",mode," window.size=",window.size),
					border=c(rep("RED",2),rep("BLUE",length(values)-2))
				)
			dev.off()
		}
		
		# generate a plot for each overlap value appearing at least twice
		for(overlap in common.overlaps)
		{	tlog(5,"Dealing with overlap=",overlap)
			
			# the series corresponds to the values of the window sizes
			idx <- sapply(overlaps, function(vect) overlap %in% vect)
			values <- load.static.nodelink.stats.by.overlap(object=object, mode=mode, window.sizes=window.sizes[idx], overlap=overlap, measure=meas.name)
			values <- c(seg.vals, values)
			
			nms <- window.sizes[idx]
			nms <- c("SO","SD",nms)
			
			# generate the stdev plot
			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, overlap=overlap),"_",meas.name,"_boxplot.png")
			tlog(5,"Plotting file \"",plot.file,"\"")
#			pdf(file=plot.file,bg="white")
			png(filename=plot.file,width=800,height=800,units="px",pointsize=20,bg="white")
				bp <- boxplot(x=values, 
					outline=FALSE,
					names=nms,
					plot=FALSE
				)
				bp$stats[1,] <- sapply(values, min)	# replace bottom whisker by min
				bp$stats[5,] <- sapply(values, max) # replace top whisker by max
				bxp(bp, 
					outline=FALSE,
					xlab="Window size",
					ylab=meas.name,
					main=paste0("mode=",mode," overlap=",overlap),
					border=c(rep("RED",2),rep("BLUE",length(values)-2))
				)
			dev.off()
		}
	}
}



###############################################################################
# Generates the plots containing several series at once, as lines.
# 
# mode: either "panel.window" or "page.window" (not "segments").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
###############################################################################
generate.static.graph.plots.multiple <- function(mode, window.sizes, overlaps)
{	# setup measure name lists
	gmn <- names(GRAPH_MEASURES)
	black.sfx <- c(SFX_STDEV) # remove all measures containing this suffix
	for(sfx in black.sfx)
		gmn <- gmn[!grepl(sfx, gmn, fixed=TRUE)]
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))	# finally, don't remove values occurring just once
	# process each appropriate measure
	for(meas.name in gmn)
	{	tlog(4,"Generating multiple plots for measure ",meas.name," (mode=",mode,")")
		
		# load the reference values (segment-based graph)
		seg.occ.vals <- load.static.graph.stats.segments(measure=meas.name, weights="occurrences")
		seg.dur.vals <- load.static.graph.stats.segments(measure=meas.name, weights="duration")
		
		# retrieve the window.size data series
		tlog(5,"Gathering and plotting data by window.size")
		data <- list()
		for(i in 1:length(window.sizes))
		{	# the series corresponds to the values of the overlap
			window.size <- window.sizes[i]
			data[[i]] <- load.static.graph.stats.by.window(mode=mode, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name)
		}
		# generate a plot containing each window size value as a series
		plot.file <- paste0(get.plotname.static(object="graph", mode=mode),"_ws_",meas.name,"_series.png")
		tlog(5,"Plotting file \"",plot.file,"\"")
		if(all(is.na(unlist(data))))
			warning(paste0("All values are NA for ", plot.file))
		else{
#			pdf(file=plot.file,bg="white")
			png(filename=plot.file,width=800,height=800,units="px",pointsize=20,bg="white")
				# init plot
				plot(NULL, 
					xlim=c(min(common.overlaps,na.rm=TRUE),max(common.overlaps,na.rm=TRUE)),
					ylim=c(if(is.na(GRAPH_MEASURES[[meas.name]]$bounds[1]))
								min(unlist(data),na.rm=TRUE)
							else
								GRAPH_MEASURES[[meas.name]]$bounds[1],
							if(is.na(GRAPH_MEASURES[[meas.name]]$bounds[2]))
								max(unlist(data),na.rm=TRUE)
							else
								GRAPH_MEASURES[[meas.name]]$bounds[2]),
					xlab="Overlap",
					ylab=meas.name,
					main=paste0("mode=",mode)
				)
				# draw series
				for(d in 1:length(data))
				{	lines(x=overlaps[[d]],y=data[[d]],
						col=COLORS[d]
					)
				}
				# add legend
				legend(x="topright",fill=COLORS,legend=window.sizes, title="Window Size")
			dev.off()
		}
#TODO insérer les réfs segments	 (=lignes horizontales en pointillés ?)	
		
		# retrieve the overlap data series
		tlog(5,"Gathering and plotting data by overlap")
		data <- list()
		axis <- list()
		for(i in 1:length(common.overlaps))
		{	# the series corresponds to the values of the window sizes
			overlap <- common.overlaps[i]
			idx <- sapply(overlaps, function(vect) overlap %in% vect)
			data[[i]] <- load.static.graph.stats.by.overlap(mode=mode, window.sizes=window.sizes[idx], overlap=overlap, measure=meas.name)
			axis[[i]] <- window.sizes[idx]
		}
		# generate a plot representing each overlap value as a series
		plot.file <- paste0(get.plotname.static(object="graph", mode=mode),"_ol_",meas.name,"_series.png")
		tlog(5,"Plotting file \"",plot.file,"\"")
		if(all(is.na(unlist(data))))
			warning(paste0("All values are NA for ", plot.file))
		else{	
#			pdf(file=plot.file,bg="white")
			png(filename=plot.file,width=800,height=800,units="px",pointsize=20,bg="white")
				# init plot
				plot(NULL, 
					xlim=c(min(window.sizes,na.rm=TRUE),max(window.sizes,na.rm=TRUE)),
					ylim=c(if(is.na(GRAPH_MEASURES[[meas.name]]$bounds[1]))
								min(unlist(data),na.rm=TRUE)
							else
								GRAPH_MEASURES[[meas.name]]$bounds[1],
							if(is.na(GRAPH_MEASURES[[meas.name]]$bounds[2]))
								max(unlist(data),na.rm=TRUE)
							else
								GRAPH_MEASURES[[meas.name]]$bounds[2]),
					xlab="Window Size",
					ylab=meas.name,
					main=paste0("mode=",mode)
				)
				# draw series
				for(d in 1:length(data))
				{	lines(x=axis[[d]],y=data[[d]],
						col=COLORS[d]
					)
				}
				# add legend
				legend(x="topright",fill=COLORS,legend=common.overlaps, title="Overlap")
			dev.off()
		}
	}
}



###############################################################################
# Generates the plots related to the statistics of static graphs.
#
# mode: either "panel.window" or "page.window" (not "segments").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
generate.all.static.plots <- function(mode, window.sizes, overlaps)
{	
	tlog(3,"Generating single plots for mode=",mode)
	generate.static.graph.plots.single(mode, window.sizes, overlaps)
	
	tlog(3,"Generating multiple plots for mode=",mode)
	generate.static.graph.plots.multiple(mode, window.sizes, overlaps)
}



###############################################################################
# Main function for the generation of plots describing static graphs.
# The statistics must have been previously extracted.
#
# panel.window.sizes: values for this parameter
# panel.overlaps: values for this parameter, specified for of the above parameter values.
# page.window.sizes: same for page-based windows instead of panel-based.
# page.overlaps: same.
###############################################################################
generate.static.plots <- function(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Generating plots for static graphs")
	
	tlog(2,"Generating plots for static graphs with panel-based windows")
	generate.all.static.plots(mode="panel.window", window.sizes=panel.window.sizes, overlaps=panel.overlaps)
	
	tlog(2,"Generating plots for static graphs with page-based windows")
	generate.all.static.plots(mode="page.window", window.sizes=page.window.sizes, overlaps=page.overlaps)
	
	tlog(1,"Generation of plots for static graphs complete")	
}
