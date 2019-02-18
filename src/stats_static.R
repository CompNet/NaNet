# This script contains functions related to the computation of descriptive
# statistics for the previously extracted networks.
# 
# Vincent Labatut
# 02/2019
###############################################################################



###############################################################################
# Computes all preselected nodal topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "segments", "panel.window" or "page.window".
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter values.
#
# returns: an nxk table containing all computed values, where n is the number of
#          nodes and k the number of measures.
###############################################################################
compute.static.node.statistics <- function(g, mode, window.size=NA, overlap=NA)
{	table.file <- get.statname.static(object="nodes", mode=mode, window.size=window.size, overlap=overlap)
	tlog(4,"Computing nodal topological measures for \"",table.file,"\"")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
	else
	{	res.tab <- matrix(NA,nrow=gorder(g),ncol=length(NODE_MEASURES))
		colnames(res.tab) <- names(NODE_MEASURES)
	}
	
	# compute each measure
	tlog(5,"Computing each nodal measure")
	for(m in 1:length(NODE_MEASURES))
	{	meas.name <- names(NODE_MEASURES)[m]
		tlog(6,"Computing measure ",meas.name)
		# compute values
		measure <- NODE_MEASURES[[m]]
		values <- measure$foo(graph=g)
		if(length(values)==0)
			values <- rep(NA,gorder(g))
		# update table
		res.tab[,meas.name] <- values
		# update file
		write.csv(x=res.tab, file=table.file, row.names=FALSE)#, col.names=TRUE)
	}
	
	tlog(4,"Computation of nodal topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected link topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "segments", "panel.window" or "page.window".
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter values.
#
# returns: an mxk table containing all computed values, where m is the number of
#          links and k the number of measures.
###############################################################################
compute.static.link.statistics <- function(g, mode, window.size=NA, overlap=NA)
{	table.file <- get.statname.static(object="links", mode=mode, window.size=window.size, overlap=overlap)
	tlog(4,"Computing link topological measures for \"",basename,"\"")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
	else
	{	res.tab <- matrix(NA,nrow=gsize(g),ncol=length(LINK_MEASURES))
		colnames(res.tab) <- names(LINK_MEASURES)
	}
	
	# compute each measure
	tlog(5,"Computing each link measure")
	for(m in 1:length(LINK_MEASURES))
	{	meas.name <- names(LINK_MEASURES)[m]
		tlog(6,"Computing measure ",meas.name)
		# compute values
		measure <- LINK_MEASURES[[m]]
		values <- measure$foo(graph=g)
		if(length(values)==0)
			values <- rep(NA,gsize(g))
		# update table
#		print(head(res.tab))
#		print(dim(res.tab))
#		print(length(values))
		res.tab[,meas.name] <- values
		# update file
		write.csv(x=res.tab, file=table.file, row.names=FALSE)#, col.names=TRUE)
	}
	
	tlog(4,"Computation of link topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected graph topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "segments", "panel.window" or "page.window".
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter values.
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
compute.static.graph.statistics <- function(g, mode, window.size=NA, overlap=NA)
{	table.file <- get.statname.static(object="graph", mode=mode, window.size=window.size, overlap=overlap)
	tlog(4,"Computing topological measures for \"",table.file,"\"")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA,nrow=length(GRAPH_MEASURES),ncol=1)
		rownames(res.tab) <- names(GRAPH_MEASURES)
	}
	
	# compute each measure
	tlog(5,"Computing each graph measure")
	for(m in 1:length(GRAPH_MEASURES))
	{	meas.name <- names(GRAPH_MEASURES)[m]
		tlog(6,"Computing measure ",meas.name)
		# compute values
		measure <- GRAPH_MEASURES[[m]]
		value <- measure$foo(graph=g)
		# update table
#		print(head(res.tab))
#		print(dim(res.tab))
#		print(length(value))
		res.tab[meas.name,1] <- value
		# update file
		write.csv(x=res.tab, file=table.file, row.names=TRUE)#, col.names=FALSE)
	}
	
	tlog(4,"Computation of topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected topological measures for the specified static graph.
#
# mode: either "segments", "panel.window" or "page.window".
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter values.
#
# returns: a list of 3 tables containing all computed values (nodes, links, graphs).
###############################################################################
compute.all.static.statistics <- function(mode, window.size=NA, overlap=NA)
{	basename <- get.basename.static(mode, window.size, overlap)
	tlog(3,"Computing graph topological measures for \"",basename,"\"")
	
	# read the graph file
	tlog(4,"Loading graph")
	graph.file <- paste0(basename,".graphml")
	g <- read.graph(file=graph.file, format="graphml")
	E(g)$weight <- E(g)$Occurrences
	
	# compute its stats
	node.stats <- compute.static.node.statistics(g, mode, window.size, overlap)
	link.stats <- compute.static.link.statistics(g, mode, window.size, overlap)
	graph.stats <- compute.static.graph.statistics(g, mode, window.size, overlap)
	
	tlog(3,"Computation of graph topological measures complete")
	res <- list(nodes=node.stats, links=link.stats, graph=graph.stats)
	return(res)
}



###############################################################################
# Main function for the computation of statistics describing static graphs.
# The graphs must have been previously extracted.
#
# panel.window.sizes: values for this parameter.
# panel.overlaps: values for this parameter, specified for of the above parameter values.
# page.window.sizes: same for page-based windows instead of panel-based.
# page.overlaps: same.
###############################################################################
compute.static.statistics <- function(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Computing statistics for static graphs")
	
	# statistics for the segment-based graph
	compute.all.static.statistics(mode="segments")
	
	# statistics for the panel window-based static graphs
	for(i in 1:length(panel.window.sizes))
	{	window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
			compute.all.static.statistics(mode="panel.window", window.size, overlap)
	}
	
	# statistics for the page window-based static graphs
	for(i in 1:length(page.window.sizes))
	{	window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
			compute.all.static.statistics(mode="page.window", window.size, overlap)
	}
	tlog(1,"Computation of statistics for static graphs complete")	
}
