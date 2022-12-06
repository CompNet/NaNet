# This script contains functions related to the generation of plots representing
# the previously computed statistics.
# 
# Vincent Labatut
# 02/2019
#
# source("src/static/plot_stats_comp.R")
###############################################################################




###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.size: fixed value for this parameter.
# overlaps: vector of values for this parameter.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
# measure: name of the concerned topological measure.
# compare: whether to compute the regular stats or to compare with reference graphs.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.by.window <- function(mode, char.det=NA, window.size, overlaps, weights, filtered, measure, compare)
{	object <- ALL_MEASURES[[measure]]$object
	res <- rep(NA, length(overlaps))
	for(j in 1:length(overlaps))
	{	overlap <- overlaps[j]
		table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=compare)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[j] <- tmp.tab[measure,1]
	}
	
	return(res)
}




###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.sizes: vector of values for this parameter.
# overlap: fixed value for this parameter.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
# measure: name of the concerned topological measure.
# compare: whether to compute the regular stats or to compare with reference graphs.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.by.overlap <- function(mode, char.det=NA, window.sizes, overlap, weights, filtered, measure, compare)
{	object <- ALL_MEASURES[[measure]]$object
	res <- rep(NA, length(window.sizes))
	for(i in 1:length(window.sizes))
	{	window.size <- window.sizes[i]
		table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=compare)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[i] <- tmp.tab[measure,1]
	}
	
	return(res)
}




###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.size: fixed value for this parameter.
# overlaps: vector of values for this parameter.
# measure: name of the concerned topological measure.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.corr.by.window <- function(mode, char.det=NA, window.size, overlaps, measure, weights, filtered)
{	res <- c()
	for(j in 1:length(overlaps))
	{	overlap <- overlaps[j]
		table.file <- get.path.stat.table(object="correlation", mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=TRUE)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res <- rbind(res, tmp.tab[measure,])
	}
	
	return(res)
}




###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.sizes: vector of values for this parameter.
# overlap: fixed value for this parameter.
# measure: name of the concerned topological measure.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.corr.by.overlap <- function(mode, char.det=NA, window.sizes, overlap, measure, weights, filtered)
{	res <- c()
	for(i in 1:length(window.sizes))
	{	window.size <- window.sizes[i]
		table.file <- get.path.stat.table(object="correlation", mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=TRUE)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res <- rbind(res, tmp.tab[measure,])
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
# filtered: whether to consider the filtered version of the graph.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.corr.scenes <- function(weights, measure, arc=NA, vol=NA, filtered)
{	table.file <- get.path.stat.table(object="correlation", mode="scenes", char.det="implicit", net.type="static", weights=weights, arc=arc, vol=vol, filtered=filtered, compare=TRUE)
	tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	res <- tmp.tab[measure,]
	return(res)
}




###############################################################################
# Loads a series corresponding to the specified parameters: fixed window size,
# varying overlap.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.size: fixed value for this parameter.
# overlaps: vector of values for this parameter.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
# measure: name of the concerned topological measure.
# compare: whether to compute the regular stats or to compare with reference graphs.
#
# returns: a list of vectors, each one representing the link/node values for
#          one parameter set.
###############################################################################
load.static.nodelink.stats.by.window <- function(mode, char.det=NA, window.size, overlaps, weights, filtered, measure, compare)
{	object <- ALL_MEASURES[[measure]]$object
	res <- list()
	for(j in 1:length(overlaps))
	{	overlap <- overlaps[j]
		table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=compare)
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
# mode: either "panel.window" or "page.window" (not "scenes").
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.sizes: vector of values for this parameter.
# overlap: fixed value for this parameter.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
# measure: name of the concerned topological measure.
# compare: whether to compute the regular stats or to compare with reference graphs.
#
# returns: a list of vectors, each one representing the link/node values for
#          one parameter set.
###############################################################################
load.static.nodelink.stats.by.overlap <- function(mode, char.det=NA, window.sizes, overlap, weights, filtered, measure, compare)
{	object <- ALL_MEASURES[[measure]]$object
	res <- list()
	for(i in 1:length(window.sizes))
	{	window.size <- window.sizes[i]
		table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=compare)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		values <- tmp.tab[,measure]
		res[[i]] <- values
	}
	
	return(res)
}




###############################################################################
# Generates the plots containing a single series as boxplots or violin plots.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
# compare: whether to compute the regular stats or to compare with reference graphs.
###############################################################################
generate.static.plots.single <- function(mode, char.det=NA, window.sizes, overlaps, weights, filtered, compare)
{	filt.txt <- if(filtered) "filtered" else "unfiltered"
	
	# setup measure name lists
	if(!is.na(weights))
	{	if(weights=="none")
		{	nmn <- names(NODE_MEASURES)[sapply(NODE_MEASURES, function(meas) !meas$weighted)]
			lmn <- names(LINK_MEASURES)[sapply(LINK_MEASURES, function(meas) !meas$weighted)]
			cmn <- names(NODECOMP_MEASURES)[sapply(NODECOMP_MEASURES, function(meas) !meas$weighted)]
		}
		else
		{	nmn <- names(NODE_MEASURES)[sapply(NODE_MEASURES, function(meas) meas$weighted)]
			lmn <- names(LINK_MEASURES)[sapply(LINK_MEASURES, function(meas) meas$weighted)]
			cmn <- names(NODECOMP_MEASURES)[sapply(NODECOMP_MEASURES, function(meas) meas$weighted)]
		}
	}
	else
	{	nmn <- names(NODE_MEASURES)
		lmn <- names(LINK_MEASURES)
		cmn <- names(NODECOMP_MEASURES)
	}
	if(compare)
	{	if(filtered)
			mn <- cmn[sapply(cmn, function(meas.name) grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
		else
			mn <- cmn[sapply(cmn, function(meas.name) !grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
	}
	else
		mn <- c(nmn, lmn)
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))	# finally, don't remove values occurring just once
	
	# process each appropriate measure
	for(meas.name in mn)
	{	tlog(4,"Generating single plots for measure ",meas.name," (mode=",mode,")")
		
		# load the reference values (scene-based graph)
		if(is.na(weights) || weights=="none")
		{	seg.none.vals <- load.static.nodelink.stats.scenes(weights="none", measure=meas.name, filtered=filt.txt, compare=meas.name %in% cmn)
			seg.vals <- list()
			seg.vals[[1]] <- seg.none.vals[!is.na(seg.none.vals)]
			snames <- c("S")
			ltys <- c(2)
		}
		else
		{	seg.occ.vals <- load.static.nodelink.stats.scenes(weights="occurrences", measure=meas.name, filtered=filt.txt, compare=meas.name %in% cmn)
			seg.dur.vals <- load.static.nodelink.stats.scenes(weights="duration", measure=meas.name, filtered=filt.txt, compare=meas.name %in% cmn)
			seg.vals <- list()
			seg.vals[[1]] <- seg.occ.vals[!is.na(seg.occ.vals)]
			seg.vals[[2]] <- seg.dur.vals[!is.na(seg.dur.vals)]
			snames <- c("SO","SD")
			ltys <- c(2, 3)
		}
	
		# generate a plot for each window size value
		for(i in 1:length(window.sizes))
		{	# the series corresponds to the values of the overlap
			window.size <- window.sizes[i]
			tlog(5,"Dealing with window.size=",window.size)
			values <- load.static.nodelink.stats.by.window(mode=mode, char.det=char.det, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name, weights=weights, filtered=filt.txt, compare=meas.name %in% cmn)
			values <- lapply(values, function(v) v[!is.na(v)])
			values <- c(seg.vals, values)
			
			nms <- overlaps[[i]]
			nms <- c(snames, nms)
			names(values) <- nms
			
			# generate the boxplot plot
			plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, window.size=window.size, weights=weights, filtered=filt.txt, suf="boxplot")
			tlog(5,"Plotting file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						bp <- boxplot(
							x=values, 
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
							border=c(rep("RED",length(snames)),rep("BLUE",length(values)-length(snames)))
						)
					dev.off()
			}
			
			# generate the violin plot
			plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, window.size=window.size, weights=weights, filtered=filt.txt, suf="violin")
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
						vioplot(
							x=values, 
							names=nms,
#							outline=FALSE,
							xlab="Overlap",
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode," window.size=",window.size),
							border=c(rep("RED",length(snames)),rep("BLUE",length(values)-length(snames))),
							col=c(rep("PINK",length(snames)),rep("LIGHTBLUE",length(values)-length(snames)))
						)
				dev.off()
			}
			
			# generate distribution plots
			plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, window.size=window.size, weights=weights, filtered=filt.txt, suf="distrib")
			tlog(5,"Plotting file \"",plot.file,"\"")
			cols <- c(rep("BLACK",length(snames)), viridis(length(values)-length(snames)))
			lty <- c(ltys, rep(1,length(values)-length(snames))) # not sure 
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
			values <- load.static.nodelink.stats.by.overlap(mode=mode, char.det=char.det, window.sizes=window.sizes[idx], overlap=overlap, measure=meas.name, weights=weights, filtered=filt.txt, compare=meas.name %in% cmn)
			values <- lapply(values, function(v) v[!is.na(v)])
			values <- c(seg.vals, values)
			
			nms <- window.sizes[idx]
			nms <- c(snames, nms)
			names(values) <- nms
			
			# generate the boxplot
			plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, overlap=overlap, weights=weights, filtered=filt.txt, suf="boxplot")
			tlog(5,"Plotting file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						bp <- boxplot(
							x=values, 
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
							border=c(rep("RED",length(snames)),rep("BLUE",length(values)-length(snames)))
						)
					dev.off()
			}
			
			# generate the violin plot
			plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, overlap=overlap, weights=weights, filtered=filt.txt, suf="violin")
			tlog(5,"Plotting file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						vioplot(
							x=values, 
							names=nms,
	#						outline=FALSE,
							xlab="Window size",
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode," overlap=",overlap),
							border=c(rep("RED",length(snames)),rep("BLUE",length(values)-length(snames))),
							col=c(rep("PINK",length(snames)),rep("LIGHTBLUE",length(values)-length(snames)))
						)
					dev.off()
			}
			
			# generate distribution plots
			plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, overlap=overlap, weights=weights, filtered=filt.txt, suf="distrib")
			tlog(5,"Plotting file \"",plot.file,"\"")
			cols <- c(rep("BLACK",length(snames)), viridis(length(values)-length(snames)))
			lty <- c(ltys, rep(1,length(values)-length(snames)))
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
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
# compare: whether to compute the regular stats or to compare with reference graphs.
###############################################################################
generate.static.plots.multiple <- function(mode, char.det=NA, window.sizes, overlaps, weights, filtered, compare)
{	filt.txt <- if(filtered) "filtered" else "unfiltered"
	
	# setup measure name lists
	if(!is.na(weights))
	{	if(weights=="none")
		{	gmn <- names(GRAPH_MEASURES)[sapply(GRAPH_MEASURES, function(meas) !meas$weighted)]
			cmn <- names(GRAPHCOMP_MEASURES)[sapply(GRAPHCOMP_MEASURES, function(meas) !meas$weighted)]
		}
		else
		{	gmn <- names(GRAPH_MEASURES)[sapply(GRAPH_MEASURES, function(meas) meas$weighted)]
			cmn <- names(GRAPHCOMP_MEASURES)[sapply(GRAPHCOMP_MEASURES, function(meas) meas$weighted)]
		}
	}
	else
	{	gmn <- names(GRAPH_MEASURES)
		cmn <- names(GRAPHCOMP_MEASURES)
	}
	if(compare)
	{	if(filtered)
			mn <- cmn[sapply(cmn, function(meas.name) grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
		else
			mn <- cmn[sapply(cmn, function(meas.name) !grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
	}
	else
		mn <- gmn
	
	black.sfx <- c(SFX_STDEV) # remove all measures containing this suffix
	for(sfx in black.sfx)
		mn <- mn[!grepl(sfx, mn, fixed=TRUE)]
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))	# finally, don't remove values occurring just once
	
	# process each appropriate measure
	for(meas.name in mn)
	{	tlog(4,"Generating multiple plots for measure ",meas.name," (mode=",mode,")")
		
		# load the reference values (scene-based graph)
		if(is.na(weights) || weights=="none")
		{	seg.none.vals <- load.static.graph.stats.scenes(weights="none", measure=meas.name, filtered=filt.txt, compare=meas.name %in% cmn)
			seg.vals <- list()
			seg.vals[[1]] <- seg.none.vals
			snames <- c("Scenes")
			ltys <- c(2)
		}
		else
		{	seg.occ.vals <- load.static.graph.stats.scenes(weights="occurrences", measure=meas.name, filtered=filt.txt, compare=meas.name %in% cmn)
			seg.dur.vals <- load.static.graph.stats.scenes(weights="duration", measure=meas.name, filtered=filt.txt, compare=meas.name %in% cmn)
			seg.vals <- list()
			seg.vals[[1]] <- seg.occ.vals
			seg.vals[[2]] <- seg.dur.vals
			snames <- c("Occurrences","Duration")
			ltys <- c(2,3)
		}
		
		# retrieve the window.size data series
		tlog(5,"Gathering and plotting data by window.size")
		data <- list()
		for(i in 1:length(window.sizes))
		{	# the series corresponds to the values of the overlap
			window.size <- window.sizes[i]
			data[[i]] <- load.static.graph.stats.by.window(mode=mode, char.det=char.det, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name, weights=weights, filtered=filt.txt, compare=meas.name %in% cmn)
			data[[i]][is.infinite(data[[i]])] <- NA
		}
		# generate a plot containing each window size value as a series
		#cols <- get.palette(length(data))
		cols <- viridis(length(data))
		plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, window.size="", weights=weights, filtered=filt.txt, suf="series")
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
										min(c(unlist(data),unlist(seg.vals)),na.rm=TRUE)
									else
										ALL_MEASURES[[meas.name]]$bounds[1],
									if(is.na(ALL_MEASURES[[meas.name]]$bounds[2]))
										max(c(unlist(data),unlist(seg.vals)),na.rm=TRUE)
									else
										ALL_MEASURES[[meas.name]]$bounds[2]),
							xlab="Overlap",
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode)
						)
						# draw reference lines
						for(s in 1:length(seg.vals))
							abline(h=seg.vals[[s]], lty=ltys[s])
						# draw series
						for(d in 1:length(data))
						{	lines(x=overlaps[[d]],y=data[[d]],
								col=cols[d], lwd=2
							)
						}
						# add color legend (discrete)
						if(length(data)<=5)
						{	legend(
								x="topright", 
								fill=cols, 
								legend=window.sizes, 
								title="Window Size"
							)
						}
						# or continuous if too many series
						else
						{	#legend.gradient(
							#	pnts="topright",
							#	cols=viridis(25),
							#	limits=range(overlaps[[i]]),
							#	title="Overlap",
							#	#cex=0.8
							#)
							gradientLegend(
								range(overlaps[[i]]), 
								color=viridis(25),	#,direction=-1), 
								inside=TRUE
							)
						}
						# add line legend (if two reference values)
						if(length(seg.vals)>1)
						{	legend(
								x="bottomright",
								lty=ltys,
								legend=snames,
								title="Scene-Based"
							)
						}
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
			data[[i]] <- load.static.graph.stats.by.overlap(mode=mode, char.det=char.det, window.sizes=window.sizes[idx], overlap=overlap, measure=meas.name, weights=weights, filtered=filt.txt, compare=meas.name %in% cmn)
			data[[i]][is.infinite(data[[i]])] <- NA
			axis[[i]] <- window.sizes[idx]
		}
		# generate a plot representing each overlap value as a series
		#cols <- get.palette(length(data))
		cols <- viridis(length(data))
		plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, overlap="", weights=weights, filtered=filt.txt, suf="series")
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
										min(c(unlist(data),unlist(seg.vals)),na.rm=TRUE)
									else
										ALL_MEASURES[[meas.name]]$bounds[1],
									if(is.na(ALL_MEASURES[[meas.name]]$bounds[2]))
										max(c(unlist(data),unlist(seg.vals)),na.rm=TRUE)
									else
										ALL_MEASURES[[meas.name]]$bounds[2]),
							xlab="Window Size",
							ylab=ALL_MEASURES[[meas.name]]$cname,
							main=paste0("mode=",mode)
						)
						# draw reference lines
						for(s in 1:length(seg.vals))
							abline(h=seg.vals[[s]], lty=ltys[s])
						# draw series
						for(d in 1:length(data))
						{	lines(x=axis[[d]],y=data[[d]],
								col=cols[d], lwd=2
							)
						}
						# add color legend (discrete)
						if(length(data)<=5)
						{	legend(
								x="topright", 
								fill=cols, 
								legend=common.overlaps, 
								title="Overlap"
							)
						}
						# or continuous if too many series
						else
						{	#legend.gradient(
							#	pnts="topright",
							#	cols=viridis(25),
							#	limits=range(overlaps[[i]]),
							#	title="Overlap",
							#	#cex=0.8
							#)
							gradientLegend(
								range(overlaps[[i]]), 
								color=viridis(25), #,direction=-1), 
								inside=TRUE
							)
						}
						# add line legend
						if(length(seg.vals)>1)
						{	legend(
									x="bottomright",
									lty=ltys,
									legend=snames,
									title="Scene-Based"
							)
						}
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
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
# filtered: whether to consider the filtered version of the graph.
# compare: whether to compute the regular stats or to compare with reference graphs.
###############################################################################
generate.static.plots.corr <- function(mode, char.det=NA, window.sizes, overlaps, weights, filtered)
{	filt.txt <- if(filtered) "filtered" else "unfiltered"
	
	# setup measure name lists
	if(!is.na(weights))
	{	if(weights=="none")
		{	nmn <- names(NODE_MEASURES)[sapply(NODE_MEASURES, function(meas) !meas$weighted)]
			pmn <- names(NODEPAIR_MEASURES)[sapply(NODEPAIR_MEASURES, function(meas) !meas$weighted)]
			cmn <- names(NODECOMP_MEASURES)[sapply(NODECOMP_MEASURES, function(meas) !meas$weighted)]
		}
		else
		{	nmn <- names(NODE_MEASURES)[sapply(NODE_MEASURES, function(meas) meas$weighted)]
			pmn <- names(NODEPAIR_MEASURES)[sapply(NODEPAIR_MEASURES, function(meas) meas$weighted)]
			cmn <- names(NODECOMP_MEASURES)[sapply(NODECOMP_MEASURES, function(meas) meas$weighted)]
		}
	}
	if(filtered)
		cmn <- cmn[sapply(cmn, function(meas.name) grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
	else
		cmn <- cmn[sapply(cmn, function(meas.name) !grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
	#gmn <- c(nmn, pmn, cmn)
	gmn <- c(nmn, pmn)
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))
	
	# process each appropriate measure
	for(meas.name in gmn)
	{	tlog(4,"Generating rank correlation plots for measure ",meas.name," (mode=",mode,")")
		object <- "graph"
		
		# load the reference values (scene-based graph)
		seg.vals <- load.static.corr.scenes(weights=weights, measure=meas.name, filtered=filt.txt)
		if(is.na(weights) || weights=="none")
		{	colus <- c(COL_NONE_SPEAR)
			seg.vals <- seg.vals[colus]
			snames <- c("Unweighted")
			ltys <- c(2)
			ww <- "none"
			ylabs <- "Spearman Correlation with Scene-Based Unweighted Graph"
		}
		else
		{	colus <- c(COL_OCC_SPEAR, COL_DUR_SPEAR)
			seg.vals <- seg.vals[colus]
			snames <- c("Occurrences","Duration")
			ltys <- c(2, 3)
			ww <- c("duration", "occurrences")
			ylabs <- c("Spearman Correlation with Scene-Based Duration Graph", ylab <- "Spearman Correlation with Scene-Based Occurrences Graph")
		}
		
		# generate a plot containing each window size value as a series
		tlog(5,"Gathering and plotting data by window.size")
		for(w in 1:length(ww))
		{	tlog(6,"Comparing to graph weights=",ww[w])
			# retrieve the window.size data series
			data <- list()
			for(i in 1:length(window.sizes))
			{	# the series corresponds to the values of the overlap
				window.size <- window.sizes[i]
				tmp <- load.static.corr.by.window(mode=mode, char.det=char.det, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name, weights=weights, filtered=filt.txt)
				data[[i]] <- tmp[,colus[w]]
			}
			
			# generating the plot
			#cols <- get.palette(length(data))
			cols <- viridis(length(data))
			#plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, window.size="", weights=weights, filtered=filt.txt, suf=paste0("corr=",substr(ww[w],1,3)))
			fake.meas <- paste0("corr_",meas.name)
			ALL_MEASURES[[fake.meas]] <<- list(folder=ALL_MEASURES[[meas.name]]$folder, object="correlation")
			plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=fake.meas, window.size="", weights=weights, filtered=filt.txt, suf=paste0("corr=",substr(ww[w],1,3)))
			tlog(6,"Plotting file \"",plot.file,"\"")
			if(all(is.na(unlist(data))))
			{	msg <- paste0("WARNING: All values are NA for ", plot.file)
				tlog(7,msg)
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
								ylab=ylabs[w],
								main=paste0("mode=",mode)
							)
							# draw reference lines
							for(s in 1:length(seg.vals))
								abline(h=seg.vals[s], lty=ltys[s])
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
							if(!(is.na(weights) || weights=="none"))
							{	legend(
									x="bottomright",
									lty=ltys,
									legend=snames,
									title="Scene-Based"
								)
							}
						dev.off()
				}
			}
		}
		
		# generate a plot representing each overlap value as a series
		tlog(5,"Gathering and plotting data by overlap")
		for(w in 1:length(ww))
		{	tlog(6,"Comparing to graph weights=",ww[w])
			# retrieve the overlap data series
			data <- list()
			axis <- list()
			for(i in 1:length(common.overlaps))
			{	# the series corresponds to the values of the window sizes
				overlap <- common.overlaps[i]
				idx <- sapply(overlaps, function(vect) overlap %in% vect)
				tmp <- load.static.corr.by.overlap(mode=mode, char.det=char.det, window.sizes=window.sizes[idx], overlap=overlap, measure=meas.name, weights=weights, filtered=filt.txt)
				data[[i]] <- tmp[,colus[w]]
				axis[[i]] <- window.sizes[idx]
			}
			
			# generating the plot
			#cols <- get.palette(length(data))
			cols <- viridis(length(data))
			#plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, overlap="", weights=weights, filtered=filt.txt, suf=paste0("corr=",substr(ww[w],1,3)))
			fake.meas <- paste0("corr_",meas.name)
			ALL_MEASURES[[fake.meas]] <<- list(folder=ALL_MEASURES[[meas.name]]$folder, object="correlation")
			plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=fake.meas, overlap="", weights=weights, filtered=filt.txt, suf=paste0("corr=",substr(ww[w],1,3)))
			tlog(6,"Plotting file \"",plot.file,"\"")
			if(all(is.na(unlist(data))))
			{	msg <- paste0("WARNING: All values are NA for ", plot.file)
				tlog(7,msg)
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
								ylab=ylabs[w],
								main=paste0("mode=",mode)
							)
							# draw reference lines
							for(s in 1:length(seg.vals))
								abline(h=seg.vals[s], lty=ltys[s])
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
							if(!(is.na(weights) || weights=="none"))
							{	legend(
									x="bottomright",
									lty=ltys,
									legend=snames,
									title="Scene-Based"
								)
							}
						dev.off()
				}
			}
		}
	}
}




#############################################################################################
# Generates a bar plot comparing two node or node-pair measures. The reference measure is used as
# a baseline, and to order the nodes on the x-axis. The comparison measure is used to process
# the ranking difference with the reference measure, and the result appears as the bar heights.
# 
# mode: either "panel.window" or "page.window" (not "scenes").
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
# compare: whether to compute the regular stats or to compare with reference graphs.
#############################################################################################
generate.static.plots.ranks <- function(mode, char.det=NA, window.sizes, overlaps, weights, filtered, compare)
{	filt.txt <- if(filtered) "filtered" else "unfiltered"
	
	# setup measure name lists
	if(!is.na(weights))
	{	if(weights=="none")
		{	nmn <- names(NODE_MEASURES)[sapply(NODE_MEASURES, function(meas) !meas$weighted)]
			pmn <- names(NODEPAIR_MEASURES)[sapply(NODEPAIR_MEASURES, function(meas) !meas$weighted)]
		}
		else
		{	nmn <- names(NODE_MEASURES)[sapply(NODE_MEASURES, function(meas) meas$weighted)]
			pmn <- names(NODEPAIR_MEASURES)[sapply(NODEPAIR_MEASURES, function(meas) meas$weighted)]
		}
	}
	else
	{	nmn <- names(NODE_MEASURES)
		pmn <- names(NODEPAIR_MEASURES)
	}
	if(compare)
		mn <- c()
	else
		mn <- c(nmn, pmn)
	
	# identify common overlap values (over window sizes)
	common.overlaps <- sort(unique(unlist(overlaps)))
	
	# process each appropriate measure
	for(meas.name in mn)
	{	tlog(4,"Generating rank difference for measure ",meas.name," (mode=",mode,")")
		
		# load the reference values (scene-based graph)
		if(is.na(weights) || weights=="none")
		{	seg.none.vals <- load.static.nodelink.stats.scenes(weights="none", measure=meas.name, filtered=filt.txt, compare=FALSE)
			seg.vals <- list()
			seg.vals[[1]] <- seg.none.vals
			seg.none.ranks <- rank(seg.none.vals, ties.method="min")
			seg.ranks <- list()
			seg.ranks[[1]] <- seg.none.ranks
			ww <- "none"
			ylabs <- c("Rank difference with Scene-Based Graph")
		}
		else
		{	seg.occ.vals <- load.static.nodelink.stats.scenes(weights="occurrences", measure=meas.name, filtered=filt.txt, compare=FALSE)
			seg.dur.vals <- load.static.nodelink.stats.scenes(weights="duration", measure=meas.name, filtered=filt.txt, compare=FALSE)
			seg.vals <- list()
			seg.vals[[1]] <- seg.occ.vals
			seg.vals[[2]] <- seg.dur.vals
			seg.occ.ranks <- rank(seg.occ.vals, ties.method="min")
			seg.dur.ranks <- rank(seg.dur.vals, ties.method="min")
			seg.ranks <- list()
			seg.ranks[[1]] <- seg.occ.ranks
			seg.ranks[[2]] <- seg.dur.ranks
			ww <- c("duration", "occurrences")
			ylabs <- c("Rank difference with Scene-Based Duration Graph", "Rank difference with Scene-Based Occurrences Graph")
		}
		
		# generate a plot for each type of weight
		for(w in 1:length(ww))
		{	
			# loop over window size values
			for(i in 1:length(window.sizes))
			{	window.size <- window.sizes[i]
				lst.values <- load.static.nodelink.stats.by.window(mode=mode, char.det=char.det, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name, weights=weights, filtered=filt.txt, compare=FALSE)
				
				# loop over each corresponding overlap value
				for(j in 1:length(overlaps[[i]]))
				{	overlap <- overlaps[[i]][j]
					tlog(5,"Dealing with window.size=",window.size," and overlap=",overlap," vs. weights=",ww[w])
					values <- lst.values[[j]]
					ranks <- rank(values, ties.method="min")
					
					# compute the ranks
					diff <- ranks - seg.ranks[[w]]
					idx <- order(seg.vals[[w]], decreasing=TRUE)
					
					# compute the colors 
					colors <- heat.colors(max(ranks))
					
					# generate the plot
					plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, filtered=filt.txt, suf=paste0("ranks=",substr(ww[w],1,3)))
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
									ylab=ylabs[w],
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
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
# weights: either "occurrences" or "duration".
# filtered: whether to consider the filtered version of the graph.
# compare: whether to compute the regular stats or to compare with reference graphs.
###############################################################################
generate.static.plots.tfpn <- function(mode, char.det=NA, window.sizes=NA, overlaps=NA, weights, filtered, compare)
{	if(compare)
	{	filt.txt <- if(filtered) "filtered" else "unfiltered"
		sfx.filt <- if(filtered) SFX_FILTERED else ""
		
		# setup measure name lists
		if(!is.na(weights))
		{	if(weights=="none")
			{	ms <- rbind(
					c(paste0(MEAS_TRUEPOS, sfx.filt, SFX_TOTAL, SFX_DUR), paste0(MEAS_FALSEPOS, sfx.filt, SFX_TOTAL, SFX_DUR), paste0(MEAS_FALSENEG, sfx.filt, SFX_TOTAL, SFX_DUR)), 
					c(paste0(MEAS_TRUEPOS, sfx.filt, SFX_TOTAL, SFX_OCC), paste0(MEAS_FALSEPOS, sfx.filt, SFX_TOTAL, SFX_OCC), paste0(MEAS_FALSENEG, sfx.filt, SFX_TOTAL, SFX_OCC))
				)
				rownames(ms) <- c(
					paste0("tfpn", sfx.filt, SFX_TOTAL, SFX_DUR),
					paste0("tfpn", sfx.filt, SFX_TOTAL, SFX_OCC)
				)
				
			}
			else
			{	ms <- rbind(
					c(paste0(MEAS_TRUEPOS, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSEPOS, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSENEG, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_DUR)), 
					c(paste0(MEAS_TRUEPOS, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSEPOS, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSENEG, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR)), 
					c(paste0(MEAS_TRUEPOS, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSEPOS, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSENEG, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_OCC)), 
					c(paste0(MEAS_TRUEPOS, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSEPOS, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSENEG, sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC)) 
				)
				rownames(ms) <- c(
					paste0("tfpn", sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_DUR),
					paste0("tfpn", sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR),
					paste0("tfpn", sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_OCC),
					paste0("tfpn", sfx.filt, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC)
				)
			}
		}
		
		# identify common overlap values (over window sizes)
		common.overlaps <- sort(unique(unlist(overlaps)))	# finally, don't remove values occurring just once
		
		for(m in 1:nrow(ms))
		{	meas.name <- rownames(ms)[m]
			tlog(4,"Computing measure \"",meas.name,"\"")
			
			# load the reference values (scene-based graph)
			if(is.na(weights) || weights=="none")
			{	data0 <- cbind(
					sapply(1:ncol(ms), function(i) load.static.graph.stats.scenes(weights="none", measure=ms[m,i], filtered=filt.txt, compare=compare))
				)
				snames <- c("S")
			}
			else
			{	data0 <- cbind(
					sapply(1:ncol(ms), function(i) load.static.graph.stats.scenes(weights="occurrences", measure=ms[m,i], filtered=filt.txt, compare=compare)),
					sapply(1:ncol(ms), function(i) load.static.graph.stats.scenes(weights="duration", measure=ms[m,i], filtered=filt.txt, compare=compare))
				)
				snames <- c("SO","SD")
			}
			
			if(grepl(SFX_OCC, meas.name, fixed=TRUE))
				subf <- "occ"
			else if(grepl(SFX_DUR, meas.name, fixed=TRUE))
				subf <- "dur"
			if(grepl(SFX_NORM, meas.name, fixed=TRUE))
				subf <- paste0(subf,"_norm")
			else
				subf <- paste0(subf,"_raw")
			
			# generate a plot for each window size value
			for(i in 1:length(window.sizes))
			{	# the series corresponds to the values of the overlap
				window.size <- window.sizes[i]
				tlog(5,"Dealing with window.size=",window.size)
				
				# load values for estimations
				tmp <- sapply(1:ncol(ms), function(j) load.static.graph.stats.by.window(mode=mode, char.det=char.det, window.size=window.size, overlaps=overlaps[[i]], measure=ms[m,j], weights=weights, filtered=filt.txt, compare=compare))
				if(length(overlaps[[i]])>1)
					tmp <- t(tmp)
				data <- cbind(data0, tmp)
				nms <- c(snames, overlaps[[i]])
				
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
					
					plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=MEAS_TFPN_GRAPH, window.size=window.size, weights=weights, filtered=filt.txt, suf=paste0(subf,"_",md,"_barplot"))
					tlog(5,"Plotting file \"",plot.file,"\"")
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
								barplot(
									values,
									col=get.palette(3),	# TODO check that
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
				tmp <- sapply(1:ncol(ms), function(j) load.static.graph.stats.by.overlap(mode=mode, char.det=char.det, window.sizes=window.sizes[idx], overlap=overlap, measure=ms[m,j], weights=weights, filtered=filt.txt, compare=compare))
				if(length(window.sizes[idx])>1)
					tmp <- t(tmp)
				data <- cbind(data0, tmp)
				nms <- c(snames, window.sizes[idx])
				
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
					
					plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=MEAS_TFPN_GRAPH, overlap=overlap, weights=weights, filtered=filt.txt, suf=paste0(subf,"_",md,"_barplot"))
					tlog(5,"Plotting file \"",plot.file,"\"")
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
								barplot(
									values,
									col=get.palette(3),	# TODO check that
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
}




###############################################################################
# Generates the plots related to the statistics of static graphs.
#
# mode: either "panel.window" or "page.window" (not "scenes").
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.sizes: vector of values for this parameter.
# overlaps: list of vectors of values for this parameter. Each vector matches a
#           value of window.size.
# compare: whether to compute the regular stats or to compare with reference graphs.
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
generate.static.plots.all <- function(mode, char.det=NA, window.sizes, overlaps, compare)
{	
	for(weights in c("none","occurrences"))
	{	for(filtered in c(FALSE,TRUE))
		{	tlog(3,"Generating single plots for mode=",mode," weights=",weights," filtered=",filtered)
			generate.static.plots.single(mode=mode, char.det=char.det, window.sizes=window.sizes, overlaps=overlaps, weights=weights, filtered=filtered, compare=compare)
			
			tlog(3,"Generating multiple plots for mode=",mode," weights=",weights," filtered=",filtered)
			generate.static.plots.multiple(mode=mode, char.det=char.det, window.sizes=window.sizes, overlaps=overlaps, weights=weights, filtered=filtered, compare=compare)
			
			tlog(3,"Generating rank comparison plots for mode=",mode," weights=",weights," filtered=",filtered)
			generate.static.plots.ranks(mode=mode, char.det=char.det, window.sizes=window.sizes, overlaps=overlaps, weights=weights, filtered=filtered, compare=compare)
			
			tlog(3,"Generating comparison plots for mode=",mode," weights=",weights," filtered=",filtered)
			generate.static.plots.tfpn(mode=mode, char.det=char.det, window.sizes=window.sizes, overlaps=overlaps, weights=weights, filtered=filtered, compare=compare)
		}
	}
}




###############################################################################
# Main function for the generation of plots describing static graphs.
# The statistics must have been previously computed.
#
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# panel.params: panel-related parameters.
# page.params: page-related parameters.
###############################################################################
generate.static.plots.window <- function(char.det=char.det, panel.params, page.params)
{	tlog(1,"Generating plots for window-based static graphs")
	
	# retrieve parameters
	panel.window.sizes <- panel.params$window.sizes
	panel.overlaps <- panel.params$overlaps
	page.window.sizes <- page.params$window.sizes
	page.overlaps <- page.params$overlaps
	
	# panel-based windows
	tlog(2,"Generating plots for static graphs with panel-based windows")
	generate.static.plots.all(mode="panel.window", char.det=char.det, window.sizes=panel.window.sizes, overlaps=panel.overlaps, compare=FALSE)
	
	# page-based windows
	tlog(2,"Generating plots for static graphs with page-based windows")
	generate.static.plots.all(mode="page.window", char.det=char.det, window.sizes=page.window.sizes, overlaps=page.overlaps, compare=FALSE)
	
	tlog(1,"Generation of plots for window-based static graphs complete")	
}




###############################################################################
# Main function for the generation of plots comparing graphs.
# The statistics must have been previously computed.
#
# data: list of previously computed tables.
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# panel.params: panel-related parameters.
# page.params: page-related parameters.
###############################################################################
generate.static.plots.comparison <- function(data, char.det=char.det, panel.params, page.params)
{	tlog(1,"Generating plots for the comparison of static graphs")
	
	# retrieve parameters
	panel.window.sizes <- panel.params$window.sizes
	panel.overlaps <- panel.params$overlaps
	page.window.sizes <- page.params$window.sizes
	page.overlaps <- page.params$overlaps
	
	# panel-based windows
	tlog(3,"Generating plots for static graphs with panel-based windows")
	generate.static.plots.all(mode="panel.window", char.det=char.det, window.sizes=panel.window.sizes, overlaps=panel.overlaps, compare=TRUE)
	tlog(3,"Generating correlation plots for static graphs with panel-based windows")
	for(filtered in c(FALSE,TRUE))
	{	for(weights in c("none","occurrences"))
		{	tlog(2,"Processing filtered=",filtered," weights=",weights)
			generate.static.plots.corr(mode="panel.window", char.det=char.det, window.sizes=panel.window.sizes, overlaps=panel.overlaps, weights=weights, filtered=filtered)
		}
	}
	
	# page-based windows
	tlog(2,"Generating plots for static graphs with page-based windows")
	generate.static.plots.all(mode="page.window", char.det=char.det, window.sizes=page.window.sizes, overlaps=page.overlaps, compare=TRUE)
	tlog(2,"Generating correlation plots for static graphs with page-based windows")
	for(filtered in c(FALSE,TRUE))
	{	for(weights in c("none","occurrences"))
		{	tlog(2,"Processing filtered=",filtered," weights=",weights)
			generate.static.plots.corr(mode="page.window", char.det=char.det, window.sizes=page.window.sizes, overlaps=page.overlaps, weights=weights, filtered=filtered)
		}
	}
	
	tlog(1,"Generation of plots for the comparison of static graphs complete")	
}
