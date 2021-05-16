# This script contains functions related to the generation of plots representing
# the previously computed statistics.
# 
# Vincent Labatut
# 02/2019
###############################################################################


## graph measures based on assortativity
#assort.groups <- list(
#	c("betweenness", "betweenness-norm", "betweenness-weighted", "betweenness-weighted-norm"),
#	c("closeness", "closeness-norm", "closeness-weighted", "closeness-weighted-norm"),
#	c("harmo-closeness", "harmo-closeness-norm", "harmo-closeness-weighted", "harmo-closeness-weighted-norm"),
#	c("degree", "degree-norm", "strength"),
#	c("eccentricity"),
#	c("eigenvector", "eigenvector-norm", "eigenvector-weighted", "eigenvector-weighted-norm"),
#	c("transitivity-local", "transitivity-weighted-local")
#)
#assort.suffix <- "-assortativity"
## graph measures based on centralization
#ctrlztn.group <- list(
#	c("betweenness", "betweenness-norm", "betweenness-weighted"),
#	c("closeness", "closeness-norm", "closeness-weighted"),
#	c("harmo-closeness", "harmo-closeness-weighted"),
#	c("degree", "degree-norm", "strength"),
#	c("eigenvector", "eigenvector-norm", "eigenvector-weighted")
#)
#ctrlztn.suffix <- "-centralization"


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "panel.window" or "page.window" (not "scenes").
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
# mode: either "panel.window" or "page.window" (not "scenes").
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
# weights: either "occurrences" or "duration".
# measure: name of the concerned topological measure.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.scenes <- function(weights, measure)
{	table.file <- get.statname.static(object="graph", mode="scenes", weights=weights)
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
		table.file <- get.statname.static(object="corr", mode=mode, window.size=window.size, overlap=overlap)
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
		table.file <- get.statname.static(object="corr", mode=mode, window.size=window.size, overlap=overlap)
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
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.corr.scenes <- function(weights, measure)
{	table.file <- get.statname.static(object="corr", mode="scenes", weights=weights)
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
		table.file <- get.statname.static(object=object, mode=mode, window.size=window.size, overlap=overlap)
		tlog(6,"Loading file \"",table.file,"\" (",j,"/",length(overlaps),")")
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
		table.file <- get.statname.static(object=object, mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
		values <- tmp.tab[,measure]
		res[[i]] <- values
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the scene-based graph.
#
# object: either "nodes", "nodepairs" or "links" (not "graph").
# measure: name of the concerned topological measure.
# weights: either "occurrences" or "duration".
#
# returns: a vector representing the link/node values for the specified measure.
###############################################################################
load.static.nodelink.stats.scenes <- function(object, measure, weights)
{	table.file <- get.statname.static(object=object, mode="scenes", weights=weights)
	tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
	res <- tmp.tab[,measure]
	return(res)
}


###############################################################################
# Generates the plots containing a single series as boxplots.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
###############################################################################
generate.static.plots.single <- function(mode, window.sizes, overlaps)
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
#			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, window.size=window.size, pre=meas.name),"_stdev.pdf")
#			pdf(file=plot.file,bg="white")
#				plot(x=overlaps, y=avg.vals,
#						ylim=c(min(avg.vals-std.vals),max(avg.vals+std.vals)),
#						xlab="Overlap",
#						ylab=meas.name
#				)
#				scenes(overlaps, avg.vals-std.vals, overlaps, avg.vals+std.vals)
#				epsilon <- 0.02
#				scenes(overlaps-epsilon, avg.vals-std.vals, overlaps+epsilon, avg.vals-std.vals)
#				scenes(overlaps-epsilon, avg.vals+std.vals, overlaps+epsilon, avg.vals+std.vals)
#			dev.off()
#			
#			# generate the min/max plot
#			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, window.size=window.size, pre=meas.name),"_minmax.pdf")
#			pdf(file=plot.file,bg="white")
#				plot(x=overlaps, y=avg.vals,
#						ylim=c(min(avg.vals-min.vals),max(avg.vals+max.vals)),
#						xlab="Overlap",
#						ylab=meas.name
#				)
#				scenes(overlaps, avg.vals-min.vals, overlaps, avg.vals+max.vals)
#				epsilon <- 0.02
#				scenes(overlaps-epsilon, avg.vals-min.vals, overlaps+epsilon, avg.vals-min.vals)
#				scenes(overlaps-epsilon, avg.vals+max.vals, overlaps+epsilon, avg.vals+max.vals)
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
		
		# load the reference values (scene-based graph)
		seg.occ.vals <- load.static.nodelink.stats.scenes(object=object, measure=meas.name, weights="occurrences")
		seg.dur.vals <- load.static.nodelink.stats.scenes(object=object, measure=meas.name, weights="duration")
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
			
			# generate the boxplot plot
			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, window.size=window.size, pre=meas.name),"_",meas.name,"_boxplot")
			tlog(5,"Plotting file \"",plot.file,"\"")
#			pdf(file=paste0(plot.file,".pdf"),bg="white")
			png(filename=paste0(plot.file,".png"),width=800,height=800,units="px",pointsize=20,bg="white")
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
			
			# generate the violin plot
			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, window.size=window.size, pre=meas.name),"_",meas.name,"_violin")
			tlog(5,"Plotting file \"",plot.file,"\"")
#			pdf(file=paste0(plot.file,".pdf"),bg="white")
			png(filename=paste0(plot.file,".png"),width=800,height=800,units="px",pointsize=20,bg="white")
				vioplot(x=values, 
					names=nms,
#					outline=FALSE,
					xlab="Overlap",
					ylab=ALL_MEASURES[[meas.name]]$cname,
					main=paste0("mode=",mode," window.size=",window.size),
					border=c(rep("RED",2),rep("BLUE",length(values)-2)),
					col=c(rep("PINK",2),rep("LIGHTBLUE",length(values)-2))
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
			
			# generate the boxplot
			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, overlap=overlap, pre=meas.name),"_",meas.name,"_boxplot")
			tlog(5,"Plotting file \"",plot.file,"\"")
#			pdf(file=paste0(plot.file,".pdf"),bg="white")
			png(filename=paste0(plot.file,".png"),width=800,height=800,units="px",pointsize=20,bg="white")
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
			
			# generate the violin plot
			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, overlap=overlap, pre=meas.name),"_",meas.name,"_violin")
			tlog(5,"Plotting file \"",plot.file,"\"")
#			pdf(file=paste0(plot.file,".pdf"),bg="white")
			png(filename=paste0(plot.file,".png"),width=800,height=800,units="px",pointsize=20,bg="white")
				vioplot(x=values, 
					names=nms,
#					outline=FALSE,
					xlab="Window size",
					ylab=ALL_MEASURES[[meas.name]]$cname,
					main=paste0("mode=",mode," overlap=",overlap),
					border=c(rep("RED",2),rep("BLUE",length(values)-2)),
					col=c(rep("PINK",2),rep("LIGHTBLUE",length(values)-2))
				)
			dev.off()
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
	gmn <- c(names(GRAPH_MEASURES), names(COMP_MEASURES))
	black.sfx <- c(SFX_STDEV) # remove all measures containing this suffix
	for(sfx in black.sfx)
		gmn <- gmn[!grepl(sfx, gmn, fixed=TRUE)]
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))	# finally, don't remove values occurring just once
	# process each appropriate measure
	for(meas.name in gmn)
	{	tlog(4,"Generating multiple plots for measure ",meas.name," (mode=",mode,")")
		
		# load the reference values (scene-based graph)
		seg.occ.vals <- load.static.graph.stats.scenes(measure=meas.name, weights="occurrences")
		seg.dur.vals <- load.static.graph.stats.scenes(measure=meas.name, weights="duration")
		
		# retrieve the window.size data series
		tlog(5,"Gathering and plotting data by window.size")
		data <- list()
		for(i in 1:length(window.sizes))
		{	# the series corresponds to the values of the overlap
			window.size <- window.sizes[i]
			data[[i]] <- load.static.graph.stats.by.window(mode=mode, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name)
		}
		# generate a plot containing each window size value as a series
		cols <- get.palette(length(data))
		plot.file <- paste0(get.plotname.static(object="graph", mode=mode, pre=meas.name),"_ws_",meas.name,"_series")
		tlog(5,"Plotting file \"",plot.file,"\"")
		if(all(is.na(unlist(data))))
		{	msg <- paste0("WARNING: All values are NA for ", plot.file)
			tlog(6,msg)
			#warning(msg)
		}
		else{
#			pdf(file=paste0(plot.file,".pdf"),bg="white")
			png(filename=paste0(plot.file,".png"),width=800,height=800,units="px",pointsize=20,bg="white")
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
		cols <- get.palette(length(data))
		plot.file <- paste0(get.plotname.static(object="graph", mode=mode, pre=meas.name),"_ol_",meas.name,"_series")
		tlog(5,"Plotting file \"",plot.file,"\"")
		if(all(is.na(unlist(data))))
		{	msg <- paste0("WARNING: All values are NA for ", plot.file)
			tlog(6,msg)
			#warning(msg)
		}
		else{	
#			pdf(file=paste0(plot.file,".pdf"),bg="white")
			png(filename=paste0(plot.file,".png"),width=800,height=800,units="px",pointsize=20,bg="white")
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
	gmn <- c(names(NODE_MEASURES), names(NODEPAIR_MEASURES))
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))
	# process each appropriate measure
	for(meas.name in gmn)
	{	tlog(4,"Generating rank correlation plots for measure ",meas.name," (mode=",mode,")")
		
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
			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, pre=meas.name),"_ws_",meas.name,"_corr")
			tlog(5,"Plotting file \"",plot.file,"\"")
			if(all(is.na(unlist(data))))
			{	msg <- paste0("WARNING: All values are NA for ", plot.file)
				tlog(6,msg)
				#warning(msg)
			}
			else{
#				pdf(file=paste0(plot.file,".pdf"),bg="white")
				png(filename=paste0(plot.file,".png"),width=800,height=800,units="px",pointsize=20,bg="white")
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
			plot.file <- paste0(get.plotname.static(object="graph", mode=mode, pre=meas.name),"_ol_",meas.name,"_corr")
			tlog(5,"Plotting file \"",plot.file,"\"")
			if(all(is.na(unlist(data))))
			{	msg <- paste0("WARNING: All values are NA for ", plot.file)
				tlog(6,msg)
				#warning(msg)
			}
			else{	
#				pdf(file=paste0(plot.file,".pdf"),bg="white")
				png(filename=paste0(plot.file,".png"),width=800,height=800,units="px",pointsize=20,bg="white")
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
	for(meas.name in mn)
	{	tlog(4,"Generating rank difference for measure ",meas.name," (mode=",mode,")")
		
		if(meas.name %in% names(NODE_MEASURES))
			object <- "nodes"
		else if(meas.name %in% names(NODEPAIR_MEASURES))
			object <- "nodepairs"
		
		# load the reference values (scene-based graph)
		seg.occ.vals <- load.static.nodelink.stats.scenes(object=object, measure=meas.name, weights="occurrences")
		seg.occ.ranks <- rank(seg.occ.vals, ties.method="min")
		seg.dur.vals <- load.static.nodelink.stats.scenes(object=object, measure=meas.name, weights="duration")
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
					plot.file <- paste0(get.plotname.static(object="graph", mode=mode, window.size=window.size, overlap=overlap, pre=meas.name),"_",meas.name,"_ranks=",substr(weights,1,3))
					tlog(5,"Plotting file \"",plot.file,"\"")
#					pdf(file=paste0(plot.file,".pdf"),bg="white")
					png(filename=paste0(plot.file,".png"),width=800,height=800,units="px",pointsize=20,bg="white")
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
	generate.static.plots.single(mode, window.sizes, overlaps)
	
	tlog(3,"Generating multiple plots for mode=",mode)
	generate.static.plots.multiple(mode, window.sizes, overlaps)
	
	tlog(3,"Generating rank correlation plots for mode=",mode)
	generate.static.plots.corr(mode, window.sizes, overlaps)
	
	tlog(3,"Generating rank comparison plots for mode=",mode)
	generate.static.plots.ranks(mode, window.sizes, overlaps)
}


###############################################################################
# Generates the plots related to the topological measures obtained on the
# scene-based graph, for both types of weights (occurrences, durations).
# The stats must have been computed beforehand.
###############################################################################
generate.static.plots.scene <- function()
{	tlog(3,"Generating plots for the scene-based graphs")
	wmodes <- c("occurrences","duration")
	
	# list measures to process
	nmn <- names(NODE_MEASURES)
	lmn <- names(LINK_MEASURES)
	npmn <- names(NODEPAIR_MEASURES)
	
	# process each measure
	for(meas.name in c(nmn,lmn))
	{	tlog(4,"Generating plots for measure ",meas.name)
		
		if(meas.name %in% nmn)
			object <- "nodes"
		else if(meas.name %in% lmn)
			object <- "links"
		else if(meas.name %in% npmn)
			object <- "nodepairs"
		
		# process each type of weights
		for(wmode in wmodes)
		{	tlog(4,"Dealing with weights=",wmode)
			file <- paste0(get.plotname.static(object=object, mode="scenes", weights=wmode, pre=meas.name), "_", meas.name)
			
			# load pre-computed values (scene-based graph)
			vals <- load.static.nodelink.stats.scenes(object=object, measure=meas.name, weights=wmode)
			# remove possible NAs
			vals <- vals[!is.na(vals)]
			
			# plot histogram
			#pdf(file=paste0(file,"_histo.pdf"), bg="white")
			png(filename=paste0(file,"_histo.png"), width=800, height=800, units="px", pointsize=20, bg="white")
				ml <- paste0("weights=",wmode)
				xl <- ALL_MEASURES[[meas.name]]$cname
				# histogram
				h <- hist(
					vals,
					breaks=20, #breaks=0:max(vals),
					col="#3a548c",
					xlab=xl,
					main=ml,
					freq=FALSE
				)
			dev.off()
			
			# plot complementary cumulative distribution function
			#pdf(file=paste0(file,"_ccdf.pdf"), bg="white")
			png(filename=paste0(file,"_ccdf.png"), width=800, height=800, units="px", pointsize=20, bg="white")
				plot.ccdf(data=vals, main=ml, xlab=xl, log=TRUE)
			dev.off()
		}
	}
}


###############################################################################
# Main function for the generation of plots describing static graphs.
# The statistics must have been previously computed.
#
# panel.window.sizes: values for this parameter
# panel.overlaps: values for this parameter, specified for of the above parameter values.
# page.window.sizes: same for page-based windows instead of panel-based.
# page.overlaps: same.
###############################################################################
generate.static.plots <- function(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Generating plots for static graphs")

	# deal with scene-based graph
	tlog(2,"Generating plots for static graphs with scene-based windows")
	generate.static.plots.scene()

	# panel-based windows
	tlog(2,"Generating plots for static graphs with panel-based windows")
	generate.static.plots.all(mode="panel.window", window.sizes=panel.window.sizes, overlaps=panel.overlaps)
	
	# page-based windows
	tlog(2,"Generating plots for static graphs with page-based windows")
	generate.static.plots.all(mode="page.window", window.sizes=page.window.sizes, overlaps=page.overlaps)
	
	tlog(1,"Generation of plots for static graphs complete")	
}
