# This script contains functions related to the computation of descriptive
# statistics for the previously extracted scene-based networks.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# list used to cache certain (costly to compute) intermediary results
cache <- list()
# whether to force computation even if value already recorded
FORCE <- TRUE




###############################################################################
# Computes all preselected nodal topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# char.det: character detection mode ("implicit" or "explicit").
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration".
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether to consider the filtered version of the graph.
#
# returns: an nxk table containing all computed values, where n is the number of
#          nodes and k the number of measures.
###############################################################################
compute.static.node.statistics <- function(g, mode, char.det=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=FALSE)
{	object <- "nodes"
	filt.txt <- if(filtered) "filtered" else "unfiltered"
	table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filt.txt, compare=FALSE)
	tlog(4,"Computing nodal topological measures for \"",table.file,"\"")
	
	if(!is.na(weights))
	{	if(weights=="none")
			node.meas <- NODE_MEASURES[sapply(NODE_MEASURES, function(meas) !meas$weighted)]
		else
			node.meas <- NODE_MEASURES[sapply(NODE_MEASURES, function(meas) meas$weighted)]
	}
	else
		node.meas <- NODE_MEASURES
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	reset.flag <- FALSE
	if(file.exists(table.file))
	{	res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		reset.flag <- nrow(res.tab)!=gorder(g)
	}
	if(!file.exists(table.file) || reset.flag)
	{	res.tab <- matrix(NA, nrow=gorder(g), ncol=length(node.meas))
		colnames(res.tab) <- names(node.meas)
		rownames(res.tab) <- V(g)$name
	}
	
	# compute each measure
	tlog(5,"Computing each nodal measure")
	for(m in 1:length(node.meas))
	{	meas.name <- names(node.meas)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(node.meas),")")
		
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
			measure <- node.meas[[m]]
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
		plot.file <- get.path.stats.topo(net.type="static", mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filt.txt, suf="histo")
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
	
	tlog(4,"Computation of nodal topological measures complete")
	return(res.tab)
}




###############################################################################
# Computes all preselected node-pair topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# char.det: character detection mode ("implicit" or "explicit").
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether to consider the filtered version of the graph.
#
# returns: an n(n-1)/2 x k table containing all computed values, where n(n-1)/2 is the 
# 	       number of pairs of nodes and k the number of measures.
###############################################################################
compute.static.nodepair.statistics <- function(g, mode, char.det=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=FALSE)
{	object <- "nodepairs"
	filt.txt <- if(filtered) "filtered" else "unfiltered"
	table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filt.txt, compare=FALSE)
	tlog(4,"Computing node-pair topological measures for \"",table.file,"\"")
	
	if(!is.na(weights))
	{	if(weights=="none")
			nodepair.meas <- NODEPAIR_MEASURES[sapply(NODEPAIR_MEASURES, function(meas) !meas$weighted)]
		else
			nodepair.meas <- NODEPAIR_MEASURES[sapply(NODEPAIR_MEASURES, function(meas) meas$weighted)]
	}
	else
		nodepair.meas <- NODEPAIR_MEASURES
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	n <- gorder(g)
	reset.flag <- FALSE
	if(file.exists(table.file))
	{	res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		reset.flag <- nrow(res.tab)!=n*(n-1)/2
	}
	if(!file.exists(table.file) || reset.flag)
	{	res.tab <- matrix(NA, nrow=n*(n-1)/2, ncol=length(nodepair.meas))
		colnames(res.tab) <- names(nodepair.meas)
		idx <- t(combn(x=gorder(g), m=2))
		rownames(res.tab) <- apply(cbind(V(g)$name[idx[,1]],V(g)$name[idx[,2]]), 1, function(vect) paste(vect,collapse=" / "))
	}
	
	# compute each measure
	tlog(5,"Computing each node-pair measure")
	for(m in 1:length(nodepair.meas))
	{	meas.name <- names(nodepair.meas)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(nodepair.meas),")")
		
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
			measure <- nodepair.meas[[m]]
			values <- measure$foo(graph=g)
			if(length(values)==0)
				values <- rep(NA,n*(n-1)/2)
			tlog(7,"Number of values: ",length(values))
			
			# update table
			res.tab[,meas.name] <- values
			
			# update file
			write.csv(x=res.tab, file=table.file, fileEncoding="UTF-8", row.names=TRUE)#, col.names=TRUE)
		}
		
		# plot
		if(!all(is.na(values)))
		{	plot.file <- get.path.stats.topo(net.type="static", mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filt.txt, suf="histo")
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
	}
	
	tlog(4,"Computation of node-pair topological measures complete")
	return(res.tab)
}




###############################################################################
# Computes all preselected link topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# char.det: character detection mode ("implicit" or "explicit").
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether to consider the filtered version of the graph.
#
# returns: an mxk table containing all computed values, where m is the number of
#          links and k the number of measures.
###############################################################################
compute.static.link.statistics <- function(g, mode, char.det=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=FALSE)
{	object <- "links"
	filt.txt <- if(filtered) "filtered" else "unfiltered"
	table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filt.txt, compare=FALSE)
	tlog(4,"Computing link topological measures for \"",table.file,"\"")
	
	if(!is.na(weights))
	{	if(weights=="none")
			link.meas <- LINK_MEASURES[sapply(LINK_MEASURES, function(meas) !meas$weighted)]
		else
			link.meas <- LINK_MEASURES[sapply(LINK_MEASURES, function(meas) meas$weighted)]
	}
	else
		link.meas <- LINK_MEASURES
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	reset.flag <- FALSE
	if(file.exists(table.file))
	{	res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		reset.flag <- nrow(res.tab)!=gsize(g)
	}
	if(!file.exists(table.file) || reset.flag)
	{	res.tab <- matrix(NA, nrow=gsize(g), ncol=length(link.meas))
		colnames(res.tab) <- names(link.meas)
		rownames(res.tab) <- apply(ends(graph=g, es=E(g), names=TRUE), 1, function(vect) paste(vect,collapse=" / "))
	}
	
	# compute each measure
	tlog(5,"Computing each link measure")
	for(m in 1:length(link.meas))
	{	meas.name <- names(link.meas)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(link.meas),")")
		
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
			measure <- link.meas[[m]]
			values <- measure$foo(graph=g)
			if(length(values)==0)
				values <- rep(NA,gsize(g))
			tlog(7,"Number of values: ",length(values))
			
			# update table
			res.tab[,meas.name] <- values
			
			# update file
			write.csv(x=res.tab, file=table.file, fileEncoding="UTF-8", row.names=TRUE)#, col.names=TRUE)
		}
		
		# plot
		plot.file <- get.path.stats.topo(net.type="static", mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filt.txt, suf="histo")
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
	
	tlog(4,"Computation of link topological measures complete")
	return(res.tab)
}




###############################################################################
# Computes all preselected graph topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# char.det: character detection mode ("implicit" or "explicit").
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether to consider the filtered version of the graph.
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
compute.static.graph.statistics <- function(g, mode, char.det=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=FALSE)
{	object <- "graph"
	filt.txt <- if(filtered) "filtered" else "unfiltered"
	table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filt.txt, compare=FALSE)
	tlog(4,"Computing graph topological measures")
	
	if(!is.na(weights))
	{	if(weights=="none")
			graph.meas <- GRAPH_MEASURES[sapply(GRAPH_MEASURES, function(meas) !meas$weighted)]
		else
			graph.meas <- GRAPH_MEASURES[sapply(GRAPH_MEASURES, function(meas) meas$weighted)]
	}
	else
		graph.meas <- GRAPH_MEASURES
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA, nrow=length(names(graph.meas)), ncol=1)
		rownames(res.tab) <- names(graph.meas)
		colnames(res.tab) <- c("Value")
	}
	
	# compute each topological and comparison measure
	tlog(5,"Computing each graph topological measure")
	measures <- graph.meas
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
			value <- measure$foo(graph=g)
			tlog(7,"Value: ",value)
			
			# update table
			res.tab[meas.name,1] <- value
			
			# update file
			write.csv(x=res.tab, file=table.file, fileEncoding="UTF-8", row.names=TRUE)#, col.names=FALSE)
		}
	}
	
	tlog(4,"Computation of graph topological measures complete")
	return(res.tab)
}




###############################################################################
# Computes all preselected topological measures for the specified static graph.
#
# mode: either "scenes", "panel.window", or "page.window".
# char.det: character detection mode ("implicit" or "explicit").
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration".
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether to consider the filtered version of the graph.
# compare: whether to compute the regular stats or to compare with reference graphs.
###############################################################################
compute.static.all.statistics <- function(mode, char.det=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=FALSE, compare=FALSE)
{	graph.file <- get.path.data.graph(mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, arc=arc, vol=vol, filtered=filtered, pref="graph", ext=".graphml")
	tlog(3,"Computing all topological measures for \"",graph.file,"\"")
	
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
	
	# compute comparison measures
	if(compare)
	{	compute.static.nodecomp.statistics(g, mode=mode, char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, filtered=filtered)
		compute.static.graphcomp.statistics(g, mode=mode, char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, filtered=filtered)
	}
	# compute regular topological measures
	else
	{	compute.static.node.statistics(g, mode=mode, char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filtered)
		compute.static.nodepair.statistics(g, mode=mode, char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filtered)
		compute.static.link.statistics(g, mode=mode, char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filtered)
		compute.static.graph.statistics(g, mode=mode, char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filtered)
	}
	
	tlog(3,"Computation of all topological measures complete")
}




###############################################################################
# Main function for the computation of statistics describing static graphs.
# The graphs must have been previously extracted.
#
# data: preprocessed data.
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
###############################################################################
compute.static.statistics.base <- function(data, char.det=NA)
{	tlog(1,"Computing statistics for scene-based static graphs")
	
	# statistics for the scene-based graph
	for(weights in c("none","occurrences","duration"))
	{	for(filtered in c(FALSE,TRUE))
			compute.static.all.statistics(mode="scenes", char.det=char.det, weights=weights, filtered=filtered, compare=FALSE)
	}
	
	# same for each narrative arc
	arc.nbr <- nrow(data$arc.stats)
	for(arc in 1:arc.nbr)
	{	for(weights in c("none","occurrences"))
		{	for(filtered in c(FALSE,TRUE))
				compute.static.all.statistics(mode="scenes", char.det=char.det, weights=weights, arc=arc, filtered=filtered, compare=FALSE)
		}
	}
	
	# same for each volume
	volume.nbr <- nrow(data$volume.stats)
	for(v in 1:volume.nbr)
	{	vol <- paste0(v,"_",data$volume.stats[v, COL_VOLUME])
		for(weights in c("none","occurrences"))
		{	for(filtered in c(FALSE,TRUE))
				compute.static.all.statistics(mode="scenes", char.det=char.det, weights=weights, vol=vol, filtered=filtered, compare=FALSE)
		}
	}
	
	tlog(1,"Computation of statistics for scene-based static graphs complete")	
}
