# This script contains functions related to the generation of plots representing
# the previously computed statistics.
# 
# Vincent Labatut
# 02/2019
###############################################################################


###############################################################################
###############################################################################
load.graph.stats <- function(window.sizes, overlaps)
{	
	for(i in 1:length(window.sizes))
	{	window.size <- window.sizes[i]
		for(overlap in overlaps[[i]])
		{	table.file <- get.statname.static(object="graph", mode, window.size, overlap)
			tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		}
	}
	
	
	

	
}

###############################################################################
# Computes all preselected nodal topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# basename: name and path of the filename containing the graph.
#
# returns: an nxk table containing all computed values, where n is the number of
#          nodes and k the number of measures.
###############################################################################
generate.static.node.plots <- function(g, basename)
{	tlog(4,"Computing nodal topological measures for \"",basename,"\"")
	
	# read or create the table containing the computed values
	table.file <- paste0(basename,"_meas_node.csv")
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
			vamues <- rep(NA,gorder(g))
		# update table
		res.tab[,meas.name] <- values
		# update file
		write.csv(x=res.tab, file=table.file, row.names=FALSE, col.names=TRUE)
	}
	
	tlog(4,"Computation of nodal topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected link topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# basename: name and path of the filename containing the graph.
#
# returns: an mxk table containing all computed values, where m is the number of
#          links and k the number of measures.
###############################################################################
generate.static.link.plots <- function(g, basename)
{	tlog(4,"Computing link topological measures for \"",basename,"\"")
	
	# read or create the table containing the computed values
	table.file <- paste0(basename,"_meas_link.csv")
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
			vamues <- rep(NA,gsize(g))
		# update table
#		print(head(res.tab))
#		print(dim(res.tab))
#		print(length(values))
		res.tab[,meas.name] <- values
		# update file
		write.csv(x=res.tab, file=table.file, row.names=FALSE, col.names=TRUE)
	}
	
	tlog(4,"Computation of link topological measures complete")
	return(res.tab)
}



###############################################################################
# Generates the plots related to the graph-related statistics of static graphs.
#
# g: graph whose statistics must be computed.
# basename: name and path of the filename containing the graph.
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
generate.static.graph.plots <- function(g, basename)
{	tlog(4,"Computing topological measures for \"",basename,"\"")
	
	# read or create the table containing the computed values
	table.file <- paste0(basename,"_meas_graph.csv")
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
		write.csv(x=res.tab, file=table.file, row.names=TRUE, col.names=FALSE)
	}
	
	tlog(4,"Computation of topological measures complete")
	return(res.tab)
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
	
	# statistics for the segment-based graph
	generate.all.static.plots(get.basename.static.segments())
	
	# statistics for the panel window-based static graphs
	for(i in 1:length(panel.window.sizes))
	{	window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
			generate.all.static.plots(get.basename.static.panel.window(window.size, overlap))
	}
	
	# statistics for the page window-based static graphs
	for(i in 1:length(page.window.sizes))
	{	window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
			generate.all.static.plots(get.basename.static.page.window(window.size, overlap))
	}
	tlog(1,"Generation of plots for static graphs complete")	
}
