# This script contains functions related to the computation of descriptive
# statistics for the previously extracted window-based networks.
# 
# Vincent Labatut
# 02/2019
###############################################################################




###############################################################################
# Computes all preselected nodal comparison measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: an nxk table containing all computed values, where n is the number of
#          nodes and k the number of measures.
###############################################################################
compute.static.nodecomp.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	object <- "nodescomp"
	table.file <- get.path.stat.table(object=object, mode=mode, net.type="static", window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing nodal comparison measures for \"",table.file,"\"")
	
	if(!is.na(weights))
	{	if(weights=="none")
			nodecomp.meas <- NODECOMP_MEASURES[sapply(NODECOMP_MEASURES, function(meas) !meas$weighted)]
		else
			nodecomp.meas <- NODECOMP_MEASURES[sapply(NODECOMP_MEASURES, function(meas) meas$weighted)]
	}
	else
		nodecomp.meas <- NODECOMP_MEASURES
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	reset.flag <- FALSE
	if(file.exists(table.file))
	{	res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		reset.flag <- nrow(res.tab)!=gorder(g)
	}
	if(!file.exists(table.file) || reset.flag)
	{	res.tab <- matrix(NA, nrow=gorder(g), ncol=length(nodecomp.meas))
		colnames(res.tab) <- names(nodecomp.meas)
		rownames(res.tab) <- V(g)$name
	}
	
	# get filtered graph
	g.filtr <- delete_vertices(g, v=which(V(g)$Filter=="Discard"))
	idx.keep <- which(V(g)$Filter=="Keep")
	
	# compute each measure
	tlog(5,"Computing each nodal measure")
	for(m in 1:length(nodecomp.meas))
	{	meas.name <- names(nodecomp.meas)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(nodecomp.meas),")")
		
		# possibly add column if missing
		if(!(meas.name %in% colnames(res.tab)))
		{	res.tab <- cbind(res.tab, rep(NA, nrow(res.tab)))
			colnames(res.tab)[ncol(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!all(is.na(res.tab[,meas.name])) && !FORCE)
		{	tlog(7,"Measure already computed, using the existing values")
			values <- res.tab[,meas.name]
		}
		else
		{	# compute values
			measure <- NODECOMP_MEASURES[[m]]
			if(grepl(SFX_FILTERED, meas.name, fixed=TRUE))
			{	values <- rep(NA,gorder(g))
				values[idx.keep] <- measure$foo(graph=g.filtr)
			}
			else
				values <- measure$foo(graph=g)
			if(length(values)==0)
				values <- rep(NA,gorder(g))
			tlog(7,"Number of values: ",length(values))
			
			# update table
			res.tab[,meas.name] <- values
			
			# update file
			write.csv(x=res.tab, file=table.file, fileEncoding="UTF-8", row.names=TRUE)#, col.names=TRUE)
		}
		
		# plot
		plot.file <- get.path.stats.topo(net.type="static", mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, suf="histo") # TODO move that into the "comparison" folder
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				hist(
					values,
					col=MAIN_COLOR
				)
			dev.off()
		}
	}
	
	# plot the TP, FP and FN values for the main characters
	ms <- rbind(
		c(paste0(MEAS_TRUEPOS, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_DUR), paste0(MEAS_FALSENEG, SFX_DUR)), 
		c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_DUR)), 
		c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_DUR)), 
		c(paste0(MEAS_TRUEPOS, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_OCC), paste0(MEAS_FALSENEG, SFX_OCC)), 
		c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_OCC)), 
		c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_OCC)) 
	)
	rownames(ms) <- c(
		paste0("tfpn", SFX_DUR),
		paste0("tfpn", SFX_WEIGHT, SFX_DUR),
		paste0("tfpn", SFX_WEIGHT, SFX_NORM, SFX_DUR),
		paste0("tfpn", SFX_OCC),
		paste0("tfpn", SFX_WEIGHT, SFX_OCC),
		paste0("tfpn", SFX_WEIGHT, SFX_NORM, SFX_OCC)
	)
	
	idx.keep <- order(V(g)$Frequency, decreasing=TRUE)[1:min(gorder(g),10)]
	for(m in 1:nrow(ms))
	{	meas.name <- rownames(ms)[m]
		tlog(6,"Plotting measure \"",meas.name,"\"")
		
		mds1 <- c("freq", "prop")
		for(md1 in mds1)
		{	mds2 <- c("all","main")
			for(md2 in mds2)
			{	# all vs main
				if(md2=="main")
					idx <- idx.keep
				else
					idx <- 1:nrow(res.tab)
				
				# prop vs freq
				data <- t(res.tab[idx,ms[m,]])
				if(md1=="prop")
				{	data <- data/matrix(rep(colSums(data),3), ncol=ncol(data), byrow=TRUE)
					ylim <- c(0,1)
					ylab <- "Proportion"
				}
				else
				{	ylim <- c(0,max(colSums(data)))
					ylab <- "Frequency"
				}
				
				plot.file <- get.path.stats.comp(mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, suf=paste0(md2,"_",md1,"_barplot"))
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						barplot(
							data,
							col=get.palette(3),
							ylim=ylim,
							xlab="Character",
							ylab=ylab,
							legend.text=c("TP","FP","FN"),
							names.arg=V(g)$ShortName[idx], las=2,
							main=paste0("mode=",mode," window.size=",window.size," overlap=",overlap)
						)
					dev.off()
				}
			}
		}
	}
	
	tlog(4,"Computation of nodal comparison measures complete")
	return(res.tab)
}




###############################################################################
# Computes all preselected graph comparison measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
compute.static.graphcomp.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	object <- "graph-comp"
	table.file <- get.path.stat.table(object=object, mode=mode, net.type="static", window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing graph comparison measures")
	
	if(!is.na(weights))
	{	if(weights=="none")
			graphcomp.meas <- GRAPHCOMP_MEASURES[sapply(GRAPHCOMP_MEASURES, function(meas) !meas$weighted)]
		else
			graphcomp.meas <- GRAPHCOMP_MEASURES[sapply(GRAPHCOMP_MEASURES, function(meas) meas$weighted)]
	}
	else
		graphcomp.meas <- GRAPHCOMP_MEASURES
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA,nrow=length(names(graphcomp.meas)),ncol=1)
		rownames(res.tab) <- names(graphcomp.meas)
		colnames(res.tab) <- c("Value")
	}
	
	# get filtered graph
	g.filtr <- delete_vertices(g, v=which(V(g)$Filter=="Discard"))
	#idx.keep <- which(V(g)$Filter=="Keep")
	
	# compute each topological and comparison measure
	tlog(5,"Computing each graph comparison measure")
	measures <- graphcomp.meas
	for(m in 1:length(measures))
	{	meas.name <- names(measures)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(measures),")")
		
		# possibly add row if missing
		if(!(meas.name %in% rownames(res.tab)))
		{	res.tab <- rbind(res.tab, NA)
			rownames(res.tab)[nrow(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!is.na(res.tab[meas.name,1]) && !FORCE)
		{	tlog(7,"Measure already computed, using the existing values")
			value <- res.tab[meas.name,1]
		}
		else
		{	# compute value
			measure <- measures[[m]]
			if(grepl(SFX_FILTERED, meas.name, fixed=TRUE))
				value <- measure$foo(graph=g.filtr)
			else
				value <- measure$foo(graph=g)
			tlog(7,"Value: ",value)
			
			# update table
			res.tab[meas.name,1] <- value
			
			# update file
			write.csv(x=res.tab, file=table.file, fileEncoding="UTF-8", row.names=TRUE)#, col.names=FALSE)
		}
	}
	
	tlog(4,"Computation of graph comparison measures complete")
	return(res.tab)
}




###############################################################################
# Computes the correlation between the previously computed topological measures 
# for the specified static graph.
#
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
#
# returns: a kx4 table containing all computed values, where k is the number of measures,
#		   and the columns correspond to Spearman's correlation and the associated p-value,
#          relatively to the duration and occurrences scene-based networks.
###############################################################################
compute.static.correlations <- function(mode, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA)
{	table.file <- get.path.stat.table(object="corr", mode=mode, net.type="static", window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol)
	#TODO must adapt the object in the above call (not standard)
	tlog(4,"Computing rank correlation measures for \"",table.file,"\"")
	
	mn <- c(names(NODE_MEASURES), names(NODEPAIR_MEASURES)) # not links, as their number can vary from one graph to the other
#	if(!is.na(arc) && !is.na(vol))
#		mn <- c(mn, names(NODECOMP_MEASURES))
	cn <- c(COL_DUR_SPEAR, COL_DUR_PVAL, COL_OCC_SPEAR, COL_OCC_PVAL) 
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA,nrow=length(mn),ncol=length(cn))
		rownames(res.tab) <- mn
		colnames(res.tab) <- cn
	}
	
	# retrieve each measure
	tlog(5,"Computing each nodal/node-pair measure")
	for(m in 1:length(mn))
	{	meas.name <- mn[m]
		tlog(6,"Computing rank correlation for measure ",meas.name," (",m,"/",length(mn),")")
		
		# possibly add row if missing
		if(!(meas.name %in% rownames(res.tab)))
		{	res.tab <- rbind(res.tab, rep(NA, ncol(res.tab)))
			rownames(res.tab)[nrow(res.tab)] <- meas.name
		}
		
		# check if already computed
#		if(!all(is.na(res.tab[meas.name,])) && !FORCE)
#		{	tlog(7,"Measure already computed, using the existing values")
#			#value <- res.tab[meas.name,1]
#		}
#		else
		{	# get object
			if(meas.name %in% names(NODE_MEASURES))
				object <- "nodes"
			else if(meas.name %in% names(NODECOMP_MEASURES))
				object <- "nodescomp"
			else if(meas.name %in% names(NODEPAIR_MEASURES))
				object <- "nodepairs"
			else if(meas.name %in% names(LINK_MEASURES))
				object <- "links"
			
			# retrieve reference values
			vals.dur <- load.static.nodelink.stats.scenes(weights="duration", measure=meas.name, arc=arc, vol=vol, filtered="unfiltered")
			vals.occ <- load.static.nodelink.stats.scenes(weights="occurrences", measure=meas.name, arc=arc, vol=vol, filtered="unfiltered")
			
			# retrieve tested values
			tab.file <- get.path.stat.table(object=object, mode=mode, net.type="static", window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol)
			tmp.tab <- as.matrix(read.csv(tab.file, header=TRUE, check.names=FALSE, row.names=1))
			vals.cur <- tmp.tab[,meas.name]
			
			# compute correlations
			#corr <- cor.test(x=vals.dur, y=vals.cur, method="spearman")
			corr <- tryCatch(
				cor.test(x=vals.dur, y=vals.cur, method="spearman", exact=FALSE),
#				warning=function(w) 
#				{	tlog(7,"WARNING: ",w)
#				},
				error=function(e)
				{	msg <- paste0("ERROR: problem when computing Spearman for measure ",meas.name)
					tlog(7,msg)
					warning(msg)
					corr <- list(estimate=NA,p.value=NA)
					return(corr)
				}
			)
			if(meas.name %in% rownames(res.tab))
				res.tab[meas.name,c(COL_DUR_SPEAR,COL_DUR_PVAL)] <- c(corr$estimate, corr$p.value)
			else
			{	res.tab <- rbind(meas.name, c(corr$estimate, corr$p.value))
				rownames(res.tab)[nrow(res.tab)] <- meas.name
			}
			#corr <- cor.test(x=vals.occ, y=vals.cur, method="spearman")
			corr <- tryCatch(
				cor.test(x=vals.occ, y=vals.cur, method="spearman", exact=FALSE),
				error=function(e) 
				{	msg <- paste0("ERROR: problem when computing Spearman for measure ",meas.name)
					tlog(7,msg)
					warning(msg)
					corr <- list(estimate=NA,p.value=NA)
					return(corr)
				}
			)		
			if(meas.name %in% rownames(res.tab))
				res.tab[meas.name,c(COL_OCC_SPEAR,COL_OCC_PVAL)] <- c(corr$estimate, corr$p.value)
			else
			{	res.tab <- rbind(meas.name, c(corr$estimate, corr$p.value))
				rownames(res.tab)[nrow(res.tab)] <- meas.name
			}
			
			# update file
			write.csv(x=res.tab, file=table.file, fileEncoding="UTF-8", row.names=TRUE)#, col.names=TRUE)
		}
	}
	
	tlog(4,"Computation of rank correlation measures complete")
	return(res.tab)
}




###############################################################################
# Computes all rank correlation measures for the specified static graph.
#
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
###############################################################################
compute.all.static.corrs <- function(mode, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA)
{	graph.file <- get.path.data.graph(mode=mode, net.type="static", window.size=window.size, overlap=overlap, arc=arc, vol=vol, filtered=FALSE, pref="graph", ext=".graphml")
	tlog(3,"Computing all rank correlation measures for \"",graph.file,"\"")
	
	# read the graph file
	tlog(4,"Loading graph")
	g <- read.graphml.file(file=graph.file)
	if(!is.na(weights))
	{	if(weights=="occurrences")
			E(g)$weight <- E(g)$Occurrences
		else if(weights=="duration")
			E(g)$weight <- E(g)$Duration
	}
	
	# init cache
	cache <<- list()
	
	# compute its stats
	compute.static.correlations(mode=mode, window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol)
	
	tlog(3,"Computation of all rank correlation measures complete")
}




###############################################################################
# Main function for the computation of statistics describing static graphs.
# The graphs must have been previously extracted.
#
# panel.params: panel-related parameters.
# page.params: page-related parameters.
###############################################################################
compute.static.statistics.window <- function(panel.params, page.params)
{	tlog(1,"Computing statistics for window-based static graphs")
	
	# retrieve parameters
	panel.window.sizes <- panel.params$window.sizes
	panel.overlaps <- panel.params$overlaps
	page.window.sizes <- page.params$window.sizes
	page.overlaps <- page.params$overlaps
	
	# statistics for the panel window-based static graphs
	tlog(2,"Dealing with panel-based windows")
	#future_sapply(1:length(panel.window.sizes), function(i) >> cache interaction pb
	for(i in 1:length(panel.window.sizes))
	{	window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
		{	for(weights in c("none","occurrences"))
			{	for(filtered in c(FALSE,TRUE))
					compute.all.static.statistics(mode="panel.window", window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=FALSE)
			}
		}
	}#)
	
	# statistics for the page window-based static graphs
	tlog(2,"Dealing with page-based windows")
	#future_sapply(1:length(page.window.sizes), function(i) >> cache interaction pb
	for(i in 1:length(page.window.sizes))
	{	window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
		{	for(weights in c("none","occurrences"))
			{	for(filtered in c(FALSE,TRUE))
					compute.all.static.statistics(mode="page.window", window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=FALSE)
			}
		}
	}#)
	
	tlog(1,"Computation of statistics for window-based static graphs complete")	
}




###############################################################################
# Main function for the comparison of graphs. Statistics must have been computed
# beforehand.
#
# data: preprocessed data.
# panel.params: panel-related parameters.
# page.params: page-related parameters.
###############################################################################
compute.static.statistics.comparison <- function(data, panel.params, page.params)
{	tlog(1,"Computing comparisons for static graphs")
	
	# retrieve parameters
	panel.window.sizes <- panel.params$window.sizes
	panel.overlaps <- panel.params$overlaps
	page.window.sizes <- page.params$window.sizes
	page.overlaps <- page.params$overlaps
	
	#### scene-based graphs
	
	# comparison for the scene-based graphs
	for(weights in c("none","occurrences","duration"))
	{	compute.all.static.statistics(mode="scenes", weights=weights, filtered=FALSE, compare=TRUE)
		compute.all.static.corrs(mode="scenes", weights=weights)
	}
	
	# correlations only for each narrative arc
	arc.nbr <- nrow(data$arc.stats)
	for(arc in 1:arc.nbr)
	{	for(weights in c("none","occurrences","duration"))
			compute.all.static.corrs(mode="scenes", weights=weights, arc=arc)
	}
	# correlations only for each volume
	volume.nbr <- nrow(data$volume.stats)
	for(v in 1:volume.nbr)
	{	vol <- paste0(v,"_",data$volume.stats[v, COL_VOLUME])
		for(weights in c("none","occurrences","duration"))
			compute.all.static.corrs(mode="scenes", weights=weights, vol=vol)
	}
	
	
	#### window-based graphs
	
	# statistics for the panel window-based static graphs
	#future_sapply(1:length(panel.window.sizes), function(i) >> cache interaction pb
	for(i in 1:length(panel.window.sizes))
	{	window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
		{	compute.all.static.statistics(mode="panel.window", window.size=window.size, overlap=overlap, filtered=FALSE, compare=TRUE)
			compute.all.static.corrs(mode="panel.window", window.size=window.size, overlap=overlap)
		}
	}#)
	
	# statistics for the page window-based static graphs
	#future_sapply(1:length(page.window.sizes), function(i) >> cache interaction pb
	for(i in 1:length(page.window.sizes))
	{	window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
		{	compute.all.static.statistics(mode="page.window", window.size=window.size, overlap=overlap, filtered=FALSE, compare=TRUE)
			compute.all.static.corrs(mode="page.window", window.size=window.size, overlap=overlap)
		}
	}#)
	
	tlog(1,"Computation of comparisons for static graphs complete")	
}
