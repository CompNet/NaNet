# This script contains functions related to the generation of plots representing
# the previously computed statistics.
# 
# Vincent Labatut
# 02/2019
###############################################################################


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# object: either "graph" or "graphcomp".
# mode: either "panel.window" or "page.window" (not "scenes").
# window.size: fixed value for this parameter.
# overlaps: vector of values for this parameter.
# measure: name of the concerned topological measure.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.by.window <- function(object, mode, window.size, overlaps, measure)
{	res <- rep(NA, length(overlaps))
	for(j in 1:length(overlaps))
	{	overlap <- overlaps[j]
		table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[j] <- tmp.tab[measure,1]
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# object: either "graph" or "graphcomp".
# mode: either "panel.window" or "page.window" (not "scenes").
# window.sizes: vector of values for this parameter.
# overlap: fixed value for this parameter.
# measure: name of the concerned topological measure.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.by.overlap <- function(object, mode, window.sizes, overlap, measure)
{	res <- rep(NA, length(window.sizes))
	for(i in 1:length(window.sizes))
	{	window.size <- window.sizes[i]
		table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[i] <- tmp.tab[measure,1]
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# object: either "graph" or "graphcomp".
# weights: either "occurrences" or "duration".
# measure: name of the concerned topological measure.
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, ignored if arc is specified).
# filtered: whether to use the filter version of the graph.
#
# returns: the value corresponding to the specified parameters.
###############################################################################
load.static.graph.stats.scenes <- function(object, weights, measure, arc=NA, vol=NA, filtered=FALSE)
{	table.file <- get.path.stat.table(object=object, mode="scenes", weights=weights, arc=arc, vol=vol, filtered=filtered)
	tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	res <- tmp.tab[measure,1]
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# window.size: fixed value for this parameter.
# overlaps: vector of values for this parameter.
# measure: name of the concerned topological measure.
# weights: either "occurrences" or "duration".
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.corr.by.window <- function(mode, window.size, overlaps, measure, weights)
{	res <- rep(NA, length(overlaps))
	if(weights=="duration")
		col <- COL_SPEAR_DUR
	else if(weights=="occurrences")
		col <- COL_SPEAR_OCC
	for(j in 1:length(overlaps))
	{	overlap <- overlaps[j]
		table.file <- get.path.stat.table(object="corr", mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[j] <- tmp.tab[measure,col]
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# window.sizes: vector of values for this parameter.
# overlap: fixed value for this parameter.
# measure: name of the concerned topological measure.
# weights: either "occurrences" or "duration".
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.corr.by.overlap <- function(mode, window.sizes, overlap, measure, weights)
{	res <- rep(NA, length(window.sizes))
	if(weights=="duration")
		col <- COL_SPEAR_DUR
	else if(weights=="occurrences")
		col <- COL_SPEAR_OCC
	for(i in 1:length(window.sizes))
	{	window.size <- window.sizes[i]
		table.file <- get.path.stat.table(object="corr", mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[i] <- tmp.tab[measure,col]
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# weights: either "occurrences" or "duration".
# measure: name of the concerned topological measure.
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, ignored if arc is specified).
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.corr.scenes <- function(weights, measure, arc=NA, vol=NA)
{	table.file <- get.path.stat.table(object="corr", mode="scenes", weights=weights, arc=arc, vol=vol)
	tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	res <- tmp.tab[measure,]
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters: fixed window size,
# varying overlap.
#
# object: either "nodes" or "links" (not "graph").
# mode: either "panel.window" or "page.window" (not "scenes").
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
		table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap)
		tlog(6,"Loading file \"",table.file,"\" (",j,"/",length(overlaps),")")
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
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
# mode: either "panel.window" or "page.window" (not "scenes").
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
		table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		values <- tmp.tab[,measure]
		res[[i]] <- values
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the scene-based graph.
#
# object: either "nodes", "nodepairs" or "links" (not "graph").
# weights: either "occurrences" or "duration".
# measure: name of the concerned topological measure.
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, ignored if arc is specified).
# filtered: whether to use the filter version of the graph.
#
# returns: a vector representing the link/node values for the specified measure.
###############################################################################
load.static.nodelink.stats.scenes <- function(object, weights, measure, arc=NA, vol=NA, filtered=FALSE)
{	table.file <- get.path.stat.table(object=object, mode="scenes", weights=weights, arc=arc, vol=vol, filtered=filtered)
	tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	res <- tmp.tab[,measure]
	return(res)
}


###############################################################################
# Generates the plots containing a single series as boxplots or violin plots.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
###############################################################################
generate.static.plots.single <- function(mode, window.sizes, overlaps)
{	# setup measure name lists
	nmn <- names(NODE_MEASURES)
	lmn <- names(LINK_MEASURES)
	cmn <- names(NODECOMP_MEASURES)
	
	# identify common overlap values (over window sizes)
	#tmp <- table(unlist(overlaps))
	#common.overlaps <- as.integer(names(tmp)[which(tmp>1)])
	common.overlaps <- sort(unique(unlist(overlaps)))	# finally, don't remove values occurring just once
	
	# process each appropriate measure
	for(meas.name in c(nmn,lmn,cmn))
	{	tlog(4,"Generating single plots for measure ",meas.name," (mode=",mode,")")
		
		if(meas.name %in% nmn)
			object <- "nodes"
		else if(meas.name %in% lmn)
			object <- "links"
		else if(meas.name %in% cmn)
			object <- "nodescomp"
		
		# load the reference values (scene-based graph)
		seg.occ.vals <- load.static.nodelink.stats.scenes(object=object, weights="occurrences", measure=meas.name, filtered=FALSE)
		seg.dur.vals <- load.static.nodelink.stats.scenes(object=object, weights="duration", measure=meas.name, filtered=FALSE)
		seg.vals <- list()
		seg.vals[[1]] <- seg.occ.vals[!is.na(seg.occ.vals)]
		seg.vals[[2]] <- seg.dur.vals[!is.na(seg.dur.vals)]
	
		# generate a plot for each window size value
		for(i in 1:length(window.sizes))
		{	# the series corresponds to the values of the overlap
			window.size <- window.sizes[i]
			tlog(5,"Dealing with window.size=",window.size)
			values <- load.static.nodelink.stats.by.window(object=object, mode=mode, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name)
			values <- lapply(values, function(v) v[!is.na(v)])
			values <- c(seg.vals, values)
			
			nms <- overlaps[[i]]
			nms <- c("SO","SD",nms)
			names(values) <- nms
			
			# generate the boxplot plot
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, plot.type="boxplot")
			tlog(5,"Plotting file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
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
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode," window.size=",window.size),
							border=c(rep("RED",2),rep("BLUE",length(values)-2))
						)
					dev.off()
			}
			
			# generate the violin plot
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, plot.type="violin")
			tlog(5,"Plotting file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					# check if there's a single value
					if(all(sapply(values, function(vect) all(vect[!is.na(vect)]==unique(vect[!is.na(vect)])[1]))))
						plot(NULL,xlim=0:1,ylim=0:1,xlab=NA,ylab=NA)	# empty plot if only one unique value
					# regular case
					else
						vioplot(x=values, 
							names=nms,
#							outline=FALSE,
							xlab="Overlap",
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode," window.size=",window.size),
							border=c(rep("RED",2),rep("BLUE",length(values)-2)),
							col=c(rep("PINK",2),rep("LIGHTBLUE",length(values)-2))
						)
				dev.off()
			}
			
			# generate distribution plots
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, plot.type="distrib")
			tlog(5,"Plotting file \"",plot.file,"\"")
			cols <- c("BLACK", "BLACK", viridis(length(values)-2))
			lty <- c(2, 3, rep(1,length(values)-1))
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					tmp <- plot.ccdf(
						data=values, 
						main=paste0("mode=",mode," window.size=",window.size),
						xlab=meas.name, ylab="default",
						log=TRUE, 
						cols=cols, 
						lines=lty
					)
					if(tmp) legend(
						x="topright", 
						col=cols,
						lty=lty,
						lwd=2,
						legend=names(values),
						title="Overlap"
					)
				dev.off()
			}
		}
		
		# generate a plot for each overlap value appearing at least twice
		for(overlap in common.overlaps)
		{	tlog(5,"Dealing with overlap=",overlap)
			
			# the series corresponds to the values of the window sizes
			idx <- sapply(overlaps, function(vect) overlap %in% vect)
			values <- load.static.nodelink.stats.by.overlap(object=object, mode=mode, window.sizes=window.sizes[idx], overlap=overlap, measure=meas.name)
			values <- lapply(values, function(v) v[!is.na(v)])
			values <- c(seg.vals, values)
			
			nms <- window.sizes[idx]
			nms <- c("SO","SD",nms)
			names(values) <- nms
			
			# generate the boxplot
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, overlap=overlap, plot.type="boxplot")
			tlog(5,"Plotting file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
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
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode," overlap=",overlap),
							border=c(rep("RED",2),rep("BLUE",length(values)-2))
						)
					dev.off()
			}
			
			# generate the violin plot
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, overlap=overlap, plot.type="violin")
			tlog(5,"Plotting file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						vioplot(x=values, 
							names=nms,
	#						outline=FALSE,
							xlab="Window size",
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode," overlap=",overlap),
							border=c(rep("RED",2),rep("BLUE",length(values)-2)),
							col=c(rep("PINK",2),rep("LIGHTBLUE",length(values)-2))
						)
					dev.off()
			}
			
			# generate distribution plots
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, overlap=overlap, plot.type="distrib")
			tlog(5,"Plotting file \"",plot.file,"\"")
			cols <- c("BLACK", "BLACK", viridis(length(values)-2))
			lty <- c(2, 3, rep(1,length(values)-1))
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						tmp <- plot.ccdf(
							data=values, 
							main=paste0("mode=",mode," overlap=",overlap),
							xlab=meas.name, ylab="default",
							log=TRUE, 
							cols=cols, 
							lines=lty
						)
						if(tmp) legend(
							x="topright", 
							col=cols,
							lty=lty,
							lwd=2,
							legend=names(values),
							title="Overlap"
						)
					dev.off()
			}
		}
	}
}


###############################################################################
# Generates the plots containing several series at once, as lines.
# 
# mode: either "panel.window" or "page.window" (not "scenes").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
###############################################################################
generate.static.plots.multiple <- function(mode, window.sizes, overlaps)
{	# setup measure name lists
	gmn <- names(GRAPH_MEASURES)
	cmn <- names(GRAPHCOMP_MEASURES)
	amn <- c(gmn, cmn)
#amn <- amn[grepl(SFX_FILTERED, amn, fixed=TRUE)]
	
	black.sfx <- c(SFX_STDEV) # remove all measures containing this suffix
	for(sfx in black.sfx)
		amn <- amn[!grepl(sfx, amn, fixed=TRUE)]
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))	# finally, don't remove values occurring just once
	
	# process each appropriate measure
	for(meas.name in amn)
	{	tlog(4,"Generating multiple plots for measure ",meas.name," (mode=",mode,")")
		
		if(meas.name %in% gmn)
			object <- "graph"
		else if(meas.name %in% cmn)
			object <- "graphcomp"
		
		# load the reference values (scene-based graph)
		seg.occ.vals <- load.static.graph.stats.scenes(object=object, measure=meas.name, weights="occurrences")
		seg.dur.vals <- load.static.graph.stats.scenes(object=object, measure=meas.name, weights="duration")
		
		# retrieve the window.size data series
		tlog(5,"Gathering and plotting data by window.size")
		data <- list()
		for(i in 1:length(window.sizes))
		{	# the series corresponds to the values of the overlap
			window.size <- window.sizes[i]
			data[[i]] <- load.static.graph.stats.by.window(object=object, mode=mode, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name)
			data[[i]][is.infinite(data[[i]])] <- NA
		}
		# generate a plot containing each window size value as a series
		#cols <- get.palette(length(data))
		cols <- viridis(length(data))
		plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, window.size="", plot.type="series")
		tlog(5,"Plotting file \"",plot.file,"\"")
		if(all(is.na(unlist(data))))
		{	msg <- paste0("WARNING: All values are NA for ", plot.file)
			tlog(6,msg)
			#warning(msg)
		}
		else
		{	for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						# init plot
						plot(NULL, 
							xlim=c(min(common.overlaps,na.rm=TRUE),max(common.overlaps,na.rm=TRUE)),
							ylim=c(if(is.na(ALL_MEASURES[[meas.name]]$bounds[1]))
										min(c(unlist(data),seg.occ.vals,seg.dur.vals),na.rm=TRUE)
									else
										ALL_MEASURES[[meas.name]]$bounds[1],
									if(is.na(ALL_MEASURES[[meas.name]]$bounds[2]))
										max(c(unlist(data),seg.occ.vals,seg.dur.vals),na.rm=TRUE)
									else
										ALL_MEASURES[[meas.name]]$bounds[2]),
							xlab="Overlap",
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode)
						)
						# draw reference lines
						abline(h=seg.occ.vals, lty=2) # dashed
						abline(h=seg.dur.vals, lty=3) # dotted
						# draw series
						for(d in 1:length(data))
						{	lines(x=overlaps[[d]],y=data[[d]],
								col=cols[d], lwd=2
							)
						}
						# add color legend
						legend(
							x="topright", 
							fill=cols, 
							legend=window.sizes, 
							title="Window Size"
						)
						# add line legend
						legend(
							x="bottomright",
							lty=c(2,3),
							legend=c("Occurrences","Duration"),
							title="Scene-Based"
						)
					dev.off()
			}
		}
		
		# retrieve the overlap data series
		tlog(5,"Gathering and plotting data by overlap")
		data <- list()
		axis <- list()
		for(i in 1:length(common.overlaps))
		{	# the series corresponds to the values of the window sizes
			overlap <- common.overlaps[i]
			idx <- sapply(overlaps, function(vect) overlap %in% vect)
			data[[i]] <- load.static.graph.stats.by.overlap(object=object, mode=mode, window.sizes=window.sizes[idx], overlap=overlap, measure=meas.name)
			data[[i]][is.infinite(data[[i]])] <- NA
			axis[[i]] <- window.sizes[idx]
		}
		# generate a plot representing each overlap value as a series
		#cols <- get.palette(length(data))
		cols <- viridis(length(data))
		plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, overlap="", plot.type="series")
		tlog(5,"Plotting file \"",plot.file,"\"")
		if(all(is.na(unlist(data))))
		{	msg <- paste0("WARNING: All values are NA for ", plot.file)
			tlog(6,msg)
			#warning(msg)
		}
		else
		{	for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						# init plot
						plot(NULL, 
							xlim=c(min(window.sizes,na.rm=TRUE),max(window.sizes,na.rm=TRUE)),
							ylim=c(if(is.na(ALL_MEASURES[[meas.name]]$bounds[1]))
										min(c(unlist(data),seg.occ.vals,seg.dur.vals),na.rm=TRUE)
									else
										ALL_MEASURES[[meas.name]]$bounds[1],
									if(is.na(ALL_MEASURES[[meas.name]]$bounds[2]))
										max(c(unlist(data),seg.occ.vals,seg.dur.vals),na.rm=TRUE)
									else
										ALL_MEASURES[[meas.name]]$bounds[2]),
							xlab="Window Size",
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode)
						)
						# draw reference lines
						abline(h=seg.occ.vals, lty=2) # dashed
						abline(h=seg.dur.vals, lty=3) # dotted
						# draw series
						for(d in 1:length(data))
						{	lines(x=axis[[d]],y=data[[d]],
								col=cols[d], lwd=2
							)
						}
						# add color legend
						legend(
							x="topright", 
							fill=cols, 
							legend=common.overlaps, 
							title="Overlap"
						)
						# add line legend
						legend(
							x="bottomright",
							lty=c(2,3),
							legend=c("Occurrences","Duration"),
							title="Scene-Based"
						)
					dev.off()
			}
		}
	}
}


###############################################################################
# Generates the plots containing several series at once, as lines, for rank
# correlation values.
# 
# mode: either "panel.window" or "page.window" (not "scenes").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
###############################################################################
generate.static.plots.corr <- function(mode, window.sizes, overlaps)
{	# setup measure name lists
	gmn <- c(names(NODE_MEASURES), names(NODEPAIR_MEASURES)) #, names(NODECOMP_MEASURES))
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))
	
	# process each appropriate measure
	for(meas.name in gmn)
	{	tlog(4,"Generating rank correlation plots for measure ",meas.name," (mode=",mode,")")
		object <- "graph"
		
		# load the reference values (scene-based graph)
		seg.vals <- load.static.corr.scenes(weights="occurrences", measure=meas.name)
		
		for(weights in c("duration","occurrences"))
		{	if(weights=="duration")
				ylab <- "Spearman Correlation with Scene-Based Duration Graph"
			else
				ylab <- "Spearman Correlation with Scene-Based Occurrences Graph"
			
			# retrieve the window.size data series
			tlog(5,"Gathering and plotting data by window.size")
			data <- list()
			for(i in 1:length(window.sizes))
			{	# the series corresponds to the values of the overlap
				window.size <- window.sizes[i]
				data[[i]] <- load.static.corr.by.window(mode=mode, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name, weights=weights)
			}
			# generate a plot containing each window size value as a series
			cols <- get.palette(length(data))
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, window.size="", plot.type="corr")
			tlog(5,"Plotting file \"",plot.file,"\"")
			if(all(is.na(unlist(data))))
			{	msg <- paste0("WARNING: All values are NA for ", plot.file)
				tlog(6,msg)
				#warning(msg)
			}
			else
			{	for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
							# init plot
							plot(NULL, 
								xlim=c(min(common.overlaps,na.rm=TRUE),max(common.overlaps,na.rm=TRUE)),
								ylim=c(-1,1),
								xlab="Overlap",
								ylab=ylab,
								main=paste0("mode=",mode)
							)
							# draw reference lines
							abline(h=seg.vals[COL_SPEAR_OCC], lty=2) # dashed
							abline(h=seg.vals[COL_SPEAR_DUR], lty=3) # dotted
							# draw series
							for(d in 1:length(data))
							{	lines(x=overlaps[[d]],y=data[[d]],
										col=cols[d], lwd=2
								)
							}
							# add color legend
							legend(
								x="topright", 
								fill=cols, 
								legend=window.sizes, 
								title="Window Size"
							)
							# add line legend
							legend(
								x="bottomright",
								lty=c(2,3),
								legend=c("Occurrences","Duration"),
								title="Scene-Based"
							)
						dev.off()
				}
			}
			
			# retrieve the overlap data series
			tlog(5,"Gathering and plotting data by overlap")
			data <- list()
			axis <- list()
			for(i in 1:length(common.overlaps))
			{	# the series corresponds to the values of the window sizes
				overlap <- common.overlaps[i]
				idx <- sapply(overlaps, function(vect) overlap %in% vect)
				data[[i]] <- load.static.corr.by.overlap(mode=mode, window.sizes=window.sizes[idx], overlap=overlap, measure=meas.name, weights=weights)
				axis[[i]] <- window.sizes[idx]
			}
			# generate a plot representing each overlap value as a series
			cols <- get.palette(length(data))
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, overlap="", plot.type="corr")
			tlog(5,"Plotting file \"",plot.file,"\"")
			if(all(is.na(unlist(data))))
			{	msg <- paste0("WARNING: All values are NA for ", plot.file)
				tlog(6,msg)
				#warning(msg)
			}
			else
			{	for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
							# init plot
							plot(NULL, 
								xlim=c(min(window.sizes,na.rm=TRUE),max(window.sizes,na.rm=TRUE)),
								ylim=c(-1,1),
								xlab="Window Size",
								ylab=ylab,
								main=paste0("mode=",mode)
							)
							# draw reference lines
							abline(h=seg.vals[COL_SPEAR_OCC], lty=2) # dashed
							abline(h=seg.vals[COL_SPEAR_DUR], lty=3) # dotted
							# draw series
							for(d in 1:length(data))
							{	lines(x=axis[[d]],y=data[[d]],
										col=cols[d], lwd=2
								)
							}
							# add color legend
							legend(
									x="topright", 
									fill=cols, 
									legend=common.overlaps, 
									title="Overlap"
							)
							# add line legend
							legend(
									x="bottomright",
									lty=c(2,3),
									legend=c("Occurrences","Duration"),
									title="Scene-Based"
							)
						dev.off()
				}
			}
		}
	}
}


#############################################################################################
# Generates a bar plot comparing two node or node-pair measures. The reference measure is used as
# a baseline, and to order the nodes on the x axis. The comparison measure is used to process
# the ranking difference with the reference measure, and the result appears as the bar heights.
# 
# mode: either "panel.window" or "page.window" (not "scenes").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
#############################################################################################
generate.static.plots.ranks <- function(mode, window.sizes, overlaps)
{	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))
	
	# process each appropriate measure
	mn <- c(names(NODE_MEASURES), names(NODEPAIR_MEASURES))
#mn <- names(NODEPAIR_MEASURES)	
	for(meas.name in mn)
	{	tlog(4,"Generating rank difference for measure ",meas.name," (mode=",mode,")")
		
		if(meas.name %in% names(NODE_MEASURES))
			object <- "nodes"
		else if(meas.name %in% names(NODEPAIR_MEASURES))
			object <- "nodepairs"
		
		# load the reference values (scene-based graph)
		seg.occ.vals <- load.static.nodelink.stats.scenes(object=object, weights="occurrences", measure=meas.name, filtered=FALSE)
		seg.occ.ranks <- rank(seg.occ.vals, ties.method="min")
		seg.dur.vals <- load.static.nodelink.stats.scenes(object=object, weights="duration", measure=meas.name, filtered=FALSE)
		seg.dur.ranks <- rank(seg.occ.vals, ties.method="min")
		
		for(weights in c("duration","occurrences"))
		{	if(weights=="duration")
				ylab <- "Rank difference with Scene-Based Duration Graph"
			else
				ylab <- "Rank difference with Scene-Based Occurrences Graph"
			
			# generate a plot for each window size value
			for(i in 1:length(window.sizes))
			{	window.size <- window.sizes[i]
				lst.values <- load.static.nodelink.stats.by.window(object=object, mode=mode, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name)
				
				# and for each corresponding overlap value
				for(j in 1:length(overlaps[[i]]))
				{	overlap <- overlaps[[i]][j]
					tlog(5,"Dealing with window.size=",window.size," and overlap=",overlap)
					values <- lst.values[[j]]
					ranks <- rank(values, ties.method="min")
					
					# compute the ranks
					if(weights=="duration")
					{	diff <- ranks - seg.dur.ranks
						idx <- order(seg.dur.vals, decreasing=TRUE)
					}
					else
					{	diff <- ranks - seg.occ.ranks
						idx <- order(seg.occ.vals, decreasing=TRUE)
					}
					
					# compute the colors 
					colors <- heat.colors(max(ranks))
					
					# generate the plot
					plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, plot.type=paste0("ranks=",substr(weights,1,3)))
					tlog(5,"Plotting file \"",plot.file,"\"")
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
								barplot(
									diff[idx],
									col=colors,
									ylim=c(-length(diff),length(diff)),
									xlab=paste0("Nodes ordered by decreasing ",ALL_MEASURES[[meas.name]]$cname),
									ylab=ylab,
									main=paste0("mode=",mode," window.size=",window.size," overlap=",overlap)
								)
							dev.off()
					}
				}
			}
		}
	}
}


###############################################################################
# Computes the TP/FP/FN graph comparison measures for the specified static graph.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
###############################################################################
generate.static.plots.tfpn <- function(mode, window.sizes=NA, overlaps=NA)
{	object <- "graphcomp"
	
	# setup measure name lists
	ms <- rbind(
		c(paste0(MEAS_TRUEPOS, SFX_TOTAL, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_TOTAL, SFX_DUR), paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_DUR)), 
		c(paste0(MEAS_TRUEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_DUR)), 
		c(paste0(MEAS_TRUEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR)), 
		c(paste0(MEAS_TRUEPOS, SFX_TOTAL, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_TOTAL, SFX_OCC), paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_OCC)), 
		c(paste0(MEAS_TRUEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_OCC)), 
		c(paste0(MEAS_TRUEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC)) 
	)
	rownames(ms) <- c(
		paste0("tfpn", SFX_TOTAL, SFX_DUR),
		paste0("tfpn", SFX_TOTAL, SFX_WEIGHT, SFX_DUR),
		paste0("tfpn", SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR),
		paste0("tfpn", SFX_TOTAL, SFX_OCC),
		paste0("tfpn", SFX_TOTAL, SFX_WEIGHT, SFX_OCC),
		paste0("tfpn", SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC)
	)
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))	# finally, don't remove values occurring just once
	
	for(m in 1:nrow(ms))
	{	meas.name <- rownames(ms)[m]
		tlog(4,"Computing measure \"",meas.name,"\"")
		
		# load the reference values (scene-based graph)
		data0 <- cbind(
			sapply(1:ncol(ms), function(i) load.static.graph.stats.scenes(object=object, weights="occurrences", measure=ms[m,i])),
			sapply(1:ncol(ms), function(i) load.static.graph.stats.scenes(object=object, weights="duration", measure=ms[m,i]))
		)
		
		# generate a plot for each window size value
		for(i in 1:length(window.sizes))
		{	# the series corresponds to the values of the overlap
			window.size <- window.sizes[i]
			tlog(5,"Dealing with window.size=",window.size)
			
			# load values for estimations
			tmp <- sapply(1:ncol(ms), function(j) load.static.graph.stats.by.window(object=object, mode=mode, window.size=window.size, overlaps=overlaps[[i]], measure=ms[m,j]))
			if(length(overlaps[[i]])>1)
				tmp <- t(tmp)
			data <- cbind(data0, tmp)
			nms <- c("SO","SD",overlaps[[i]])
			
			mds <- c("freq", "prop")
			for(md in mds)
			{	# prop vs freq
				values <- data
				if(md=="prop")
				{	values <- values/matrix(rep(colSums(values),3), ncol=ncol(values), byrow=TRUE)
					ylim <- c(0,1)
					ylab <- "Proportion"
				}
				else
				{	ylim <- c(0,max(colSums(values)))
					ylab <- "Frequency"
				}
				
				plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, plot.type=paste0(md,"_barplot"))
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
							barplot(
								values,
								col=get.palette(3),
								ylim=ylim,
								xlab="Overlap",
								ylab=ylab,
								legend.text=c("TP","FP","FN"),
								names.arg=nms,
								main=paste0("mode=",mode," window.size=",window.size)
							)
						dev.off()
				}
			}
		}
		
		# generate a plot for each overlap value appearing at least twice
		for(overlap in common.overlaps)
		{	tlog(5,"Dealing with overlap=",overlap)
			
			# the series corresponds to the values of the window sizes
			idx <- sapply(overlaps, function(vect) overlap %in% vect)
			tmp <- sapply(1:ncol(ms), function(j) load.static.graph.stats.by.overlap(object=object, mode=mode, window.sizes=window.sizes[idx], overlap=overlap, measure=ms[m,j]))
			if(length(window.sizes[idx])>1)
				tmp <- t(tmp)
			data <- cbind(data0, tmp)
			nms <- c("SO","SD",window.sizes[idx])
			
			mds <- c("freq", "prop")
			for(md in mds)
			{	# prop vs freq
				values <- data
				if(md=="prop")
				{	values <- values/matrix(rep(colSums(values),3), ncol=ncol(values), byrow=TRUE)
					ylim <- c(0,1)
					ylab <- "Proportion"
				}
				else
				{	ylim <- c(0,max(colSums(values)))
					ylab <- "Frequency"
				}
				
				plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, overlap=overlap, plot.type=paste0(md,"_barplot"))
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
							barplot(
								values,
								col=get.palette(3),
								ylim=ylim,
								xlab="Window size",
								ylab=ylab,
								legend.text=c("TP","FP","FN"),
								names.arg=nms,
								main=paste0("mode=",mode," overlap=",overlap)
							)
						dev.off()
				}
			}
		}
	}
}


###############################################################################
# Generates the plots related to the statistics of static graphs.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
generate.static.plots.all <- function(mode, window.sizes, overlaps)
{	
	tlog(3,"Generating single plots for mode=",mode)
	generate.static.plots.single(mode=mode, window.sizes=window.sizes, overlaps=overlaps)
	
	tlog(3,"Generating multiple plots for mode=",mode)
	generate.static.plots.multiple(mode=mode, window.sizes=window.sizes, overlaps=overlaps)
	
	tlog(3,"Generating correlation plots for mode=",mode)
	generate.static.plots.corr(mode=mode, window.sizes=window.sizes, overlaps=overlaps)
	
	tlog(3,"Generating rank comparison plots for mode=",mode)
	generate.static.plots.ranks(mode=mode, window.sizes=window.sizes, overlaps=overlaps)
	
	tlog(3,"Generating comparison plots for mode=",mode)
	generate.static.plots.tfpn(mode=mode, window.sizes=window.sizes, overlaps=overlaps)
}


###############################################################################
# Generates the plots related to the topological measures obtained on the
# scene-based graph, for both types of weights (occurrences, durations).
# The stats must have been computed beforehand.
#
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether to use the filtered version of the graph.
###############################################################################
generate.static.plots.scene <- function(arc=NA, vol=NA, filtered=FALSE)
{	tlog(3,"Generating plots for the ",if(filtered) "filtered" else "unfiltered"," scene-based graphs")
	mode <- "scenes"
	wmodes <- c("occurrences","duration")
	col <- get.palette(2)[if(filtered) 2 else 1]
	
	# list measures to plot
	nmn <- names(NODE_MEASURES)
	lmn <- names(LINK_MEASURES)
	npmn <- names(NODEPAIR_MEASURES)
	
	# plot each measure
	for(meas.name in c(nmn,lmn))
	{	tlog(4,"Generating plots for measure ",meas.name)
		
		if(meas.name %in% nmn)
			object <- "nodes"
		else if(meas.name %in% lmn)
			object <- "links"
		else if(meas.name %in% npmn)
			object <- "nodepairs"
		
		# process each type of weight
		for(wmode in wmodes)
		{	tlog(4,"Dealing with weights=",wmode)
			
			# load pre-computed values (scene-based graph)
			vals <- load.static.nodelink.stats.scenes(object=object, weights=wmode, measure=meas.name, arc=arc, vol=vol, filtered=filtered)
			# remove possible NAs
			vals <- vals[!is.na(vals)]
			#vals <- vals[vals>0]	# remove the zeroes?
			
			# plot histogram
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, weights=wmode, arc=arc, vol=vol, filtered=filtered, plot.type="histo")
			# TODO why is this the "comparison" folder? shouldn't it be stats or plots?
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						ml <- paste0("weights=",wmode)
						xl <- ALL_MEASURES[[meas.name]]$cname
						# histogram
						h <- hist(
							vals,
							breaks=20, #breaks=0:max(vals),
							col=col,
							xlab=xl,
							main=ml,
							freq=FALSE
						)
					dev.off()
			}
			
			# plot complementary cumulative distribution function
			plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, weights=wmode, arc=arc, vol=vol, filtered=filtered, plot.type="ccdf")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						plot.ccdf(data=vals, main=ml, xlab=xl, ylab="default", log=TRUE)
					dev.off()
			}
			
			# test the type of distribution (very slow, doing it only for the whole graph)
			if(is.na(arc) && is.na(vol))
			{	plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, weights=wmode, arc=arc, vol=vol, filtered=filtered, plot.type="disttest")
				if(all(vals%%1==0))
					test.disc.distr(data=vals, xlab=ALL_MEASURES[[meas.name]]$cname, return_stats=TRUE, sims=100, plot.file=plot.file)
				else
					test.cont.distr(data=vals, xlab=ALL_MEASURES[[meas.name]]$cname, return_stats=TRUE, sims=100, plot.file=plot.file)
			}
		}
	}
	
	# compute and plot additional stuff
	# read the graph
	graph.file <- get.path.graph.file(mode="scenes", filtered=FALSE, desc="static", ext=".graphml")
	g <- read.graphml.file(file=graph.file)
	if(filtered)
		g <- delete_vertices(graph=g, v=which(V(g)$Filtered))
	g.dur <- g; E(g.dur)$weight <- E(g)$Duration
	g.occ <- g; E(g.occ)$weight <- E(g)$Occurrences
	
	# degree vs. neighbors' degree
	filename <- get.path.comparison.plot(object="nodes", mode="scenes", meas.name=MEAS_DEGREE, arc=arc, vol=vol, filtered=filtered)
	neigh.degree.vs.degree(g, weights=FALSE, filename, col)
	for(wmode in wmodes)
	{	filename <- get.path.comparison.plot(object="nodes", mode="scenes", meas.name="strength", weights=wmode, arc=arc, vol=vol, filtered=filtered)
		if(wmode=="duration")
			neigh.degree.vs.degree(g.dur, weights=TRUE, filename, col)
		else if(wmode=="occurrences")
			neigh.degree.vs.degree(g.occ, weights=TRUE, filename, col)
	}
	
	# degree vs. transitivity
	filename <- get.path.comparison.plot(object="nodes", mode="scenes", meas.name=MEAS_DEGREE, arc=arc, vol=vol, filtered=filtered)
	transitivity.vs.degree(g, weights=FALSE, filename, col)
	for(wmode in wmodes)
	{	filename <- get.path.comparison.plot(object="nodes", mode="scenes", meas.name="strength", weights=wmode, arc=arc, vol=vol, filtered=filtered)
		if(wmode=="duration")
			transitivity.vs.degree(g.dur, weights=TRUE, filename, col)
		else if(wmode=="occurrences")
			transitivity.vs.degree(g.occ, weights=TRUE, filename, col)
	}
	
	# hop plots
	filename <- get.path.comparison.plot(object="nodepairs", mode="scenes", meas.name=MEAS_DISTANCE, arc=arc, vol=vol, filtered=filtered)
	hop.plot(g, weights=FALSE, filename, col)
	for(wmode in wmodes)
	{	filename <- get.path.comparison.plot(object="nodepairs", mode="scenes", meas.name="distance", weights=wmode, arc=arc, vol=vol, filtered=filtered)
		if(wmode=="duration")
			hop.plot(g.dur, weights=TRUE, filename, col)
		else if(wmode=="occurrences")
			hop.plot(g.occ, weights=TRUE, filename, col)
	}
}


###############################################################################
# Generates the plots showing the evolution of measures over arcs or volumes.
#
# data: preprocessed data.
# arcs: TRUE to process arcs, FALSE to process volumes.
# filtered: whether to use the filter version of the graph.
###############################################################################
generate.static.plots.evol <- function(data, arcs, filtered)
{	# init arc/vol-dependent variables
	if(arcs)
	{	emode <- "arc"
		items <- unique(data$volume.stats[,COL_ARC])
	}
	else
	{	emode <- "volume"
		items <- data$volume.stats[,COL_VOLUME]
	}
	
	# init other variables
	tlog(3,"Generating ",emode,"-based evolution plots for the ",if(filtered) "filtered" else "unfiltered"," scene-based graphs")
	mode <- "scenes"
	wmodes <- c("occurrences","duration")
	col <- get.palette(2)[if(filtered) 2 else 1]
	
	# list measures to plot
	gmn <- names(GRAPH_MEASURES)
	
	# plot each measure
	for(meas.name in gmn)
	{	tlog(4,"Generating ",emode,"-based evolution plots for measure ",meas.name)
		object <- "graph"
		
		# process each type of weight
		for(wmode in wmodes)
		{	tlog(4,"Dealing with weights=",wmode)
			
			# load pre-computed values (scene-based graph)
			vals <- rep(NA, length(items))
			for(i in 1:length(items))
			{	vals[i] <- load.static.graph.stats.scenes(object=object, weights=wmode, measure=meas.name, 
							arc=if(arcs) i else NA, vol=if(arcs) NA else items[i], 
							filtered=filtered)
			}
			
			# generate barplots
			file <- get.path.topomeas.plot(object=object, mode=mode, meas.name=meas.name, weights=wmode, arc=if(arcs) TRUE else NA, vol=if(arcs) NA else TRUE, filtered=filtered, plot.type="evolution")
			tlog(4,"Generating file ",file)
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					barplot(
						height=vals,
						names.arg=if(arcs) 1:length(items) else items,
						ylab=ALL_MEASURES[[meas.name]]$cname,
						xlab=if(arcs) "Narrative Arcs" else "Volumes",
						main=paste0("Evolution of ",ALL_MEASURES[[meas.name]]$cname," by ",if(arcs) "arc" else "volume"),
						col=col
					)
				dev.off()
			}
		}
	}
}


###############################################################################
# Main function for the generation of plots describing static graphs.
# The statistics must have been previously computed.
#
# data: preprocessed data.
# panel.window.sizes: values for this parameter
# panel.overlaps: values for this parameter, specified for of the above parameter values.
# page.window.sizes: same for page-based windows instead of panel-based.
# page.overlaps: same.
###############################################################################
generate.static.plots <- function(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Generating plots for static graphs")
	
	# deal with scene-based graph
	tlog(2,"Generating plots for static graphs with scene-based windows")
	for(filtered in c(FALSE, TRUE))
		generate.static.plots.scene(filtered=filtered)
	# same for each narrative arc
	arc.titles <- unique(data$volume.stats[,COL_ARC])
	for(arc in 1:length(arc.titles))
	{	for(filtered in c(FALSE, TRUE))
			generate.static.plots.scene(arc=arc, filtered=filtered)
	}
	# same for each volume
	volume.nbr <- nrow(data$volume.stats)
	for(v in 1:volume.nbr)
	{	vol <- data$volume.stats[v, COL_VOLUME]
		for(filtered in c(FALSE, TRUE))
			generate.static.plots.scene(vol=vol, filtered=filtered)
	}
	# evolution plots
	for(flag in c(TRUE,FALSE))
	{	for(filtered in c(FALSE, TRUE))
			generate.static.plots.evol(data=data, arcs=flag, filtered=filtered)
	}

#	# panel-based windows
#	tlog(2,"Generating plots for static graphs with panel-based windows")
#	generate.static.plots.all(mode="panel.window", window.sizes=panel.window.sizes, overlaps=panel.overlaps)
#	
#	# page-based windows
#	tlog(2,"Generating plots for static graphs with page-based windows")
#	generate.static.plots.all(mode="page.window", window.sizes=page.window.sizes, overlaps=page.overlaps)
	
	tlog(1,"Generation of plots for static graphs complete")	
}
