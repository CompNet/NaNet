# This script contains functions related to the computation of descriptive
# statistics for the previously extracted networks.
# 
# Vincent Labatut
# 02/2019
###############################################################################

# list used to cache certain (costly to compute) intermediary results
cache <- list()



###############################################################################
# Computes all preselected nodal topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "segments", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="segments").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="segments").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: an nxk table containing all computed values, where n is the number of
#          nodes and k the number of measures.
###############################################################################
compute.static.node.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	table.file <- get.statname.static(object="nodes", mode=mode, window.size=window.size, overlap=overlap, weights=weights)
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
# Computes all preselected node-pair topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "segments", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="segments").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="segments").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: an n(n-1)/2 x k table containing all computed values, where n(n-1)/2 is the 
# 	       number of pairs of nodes and k the number of measures.
###############################################################################
compute.static.nodepair.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	table.file <- get.statname.static(object="nodepairs", mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing node-pair topological measures for \"",table.file,"\"")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	n <- gorder(g)
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
	else
	{	res.tab <- matrix(NA,nrow=n*(n-1)/2,ncol=length(NODEPAIR_MEASURES))
		colnames(res.tab) <- names(NODEPAIR_MEASURES)
	}
	
	# compute each measure
	tlog(5,"Computing each node-pair measure")
	for(m in 1:length(NODEPAIR_MEASURES))
	{	meas.name <- names(NODEPAIR_MEASURES)[m]
		tlog(6,"Computing measure ",meas.name)
		# compute values
		measure <- NODEPAIR_MEASURES[[m]]
		values <- measure$foo(graph=g)
		if(length(values)==0)
			values <- rep(NA,n*(n-1)/2)
		# update table
		res.tab[,meas.name] <- values
		# update file
		write.csv(x=res.tab, file=table.file, row.names=FALSE)#, col.names=TRUE)
	}
	
	tlog(4,"Computation of node-pair topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected link topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "segments", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="segments").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="segments").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: an mxk table containing all computed values, where m is the number of
#          links and k the number of measures.
###############################################################################
compute.static.link.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	table.file <- get.statname.static(object="links", mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing link topological measures for \"",table.file,"\"")
	
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
# mode: either "segments", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="segments").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="segments").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
compute.static.graph.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	table.file <- get.statname.static(object="graph", mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing graph topological measures for \"",table.file,"\"")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA,nrow=length(c(names(GRAPH_MEASURES),names(COMP_MEASURES))),ncol=1)
		rownames(res.tab) <- c(names(GRAPH_MEASURES),names(COMP_MEASURES))
	}
	
	# compute each topological and comparison measure
	tlog(5,"Computing each graph measure")
	measures <- c(GRAPH_MEASURES, COMP_MEASURES)
	for(m in 1:length(measures))
	{	meas.name <- names(measures)[m]
		tlog(6,"Computing measure ",meas.name)
		# compute value
		measure <- measures[[m]]
		value <- measure$foo(graph=g)
		# update table
#		print(head(res.tab))
#		print(dim(res.tab))
#		print(length(value))
		res.tab[meas.name,1] <- value
		tlog(7,"Value: ",value)
		# update file
		write.csv(x=res.tab, file=table.file, row.names=TRUE)#, col.names=FALSE)
	}
	
	tlog(4,"Computation of graph topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected nodal topological measures for the specified static graph.
#
# mode: either "segments", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="segments").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="segments").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: a kx4 table containing all computed values, where k is the number of measures,
#		   and the columns correspond to Spearman's correlation and the associated p-value,
#          relatively to the duration and occurrences segment-based networks.
###############################################################################
compute.static.correlations <- function(mode, window.size=NA, overlap=NA, weights=NA)
{	table.file <- get.statname.static(object="corr", mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing rank correlation measures for \"",table.file,"\"")
	
	mn <- c(names(NODE_MEASURES),names(NODEPAIR_MEASURES)) # not links, as their number can vary from one graph to the other
	cn <- c(COL_SPEAR_DUR, COL_PVAL_DUR, COL_SPEAR_OCC, COL_PVAL_OCC) 
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA,nrow=length(mn),ncol=length(cn))
		rownames(res.tab) <- mn
		colnames(res.tab) <- cn
	}
	
	# compute each measure
	tlog(5,"Computing each nodal/node-pair measure")
	for(m in 1:length(mn))
	{	meas.name <- mn[m]
		tlog(6,"Computing rank correlation for measure ",meas.name)
		# get object
		if(meas.name %in% names(NODE_MEASURES))
			object <- "nodes"
		else if(meas.name %in% names(NODEPAIR_MEASURES))
			object <- "nodepairs"
		else if(meas.name %in% names(LINK_MEASURES))
			object <- "links"
		# retrieve reference values
		vals.dur <- load.static.nodelink.stats.segments(object=object, measure=meas.name, weights="duration")
		vals.occ <- load.static.nodelink.stats.segments(object=object, measure=meas.name, weights="occurrences")
		# retrieve tested values
		tab.file <- get.statname.static(object=object, mode=mode, window.size=window.size, overlap=overlap, weights=weights)
print(tab.file)		
		tmp.tab <- as.matrix(read.csv(tab.file, header=TRUE, check.names=FALSE))
		vals.cur <- tmp.tab[,meas.name]
		# compute correlations
#print(vals.dur)
#print(vals.cur)
print(length(vals.dur))
print(length(vals.cur))
#		corr <- cor.test(x=vals.dur, y=vals.cur, method="spearman")
		corr <- tryCatch(
			cor.test(x=vals.dur, y=vals.cur, method="spearman"),
			error=function(e)
			{	msg <- paste0("WARNING: problem when computing Spearman for measure ",meas.name)
				tlog(7,msg)
				warning(msg)
				corr <- list(estimate=NA,p.value=NA)
				return(corr)
			}
		)		
		res.tab[meas.name,COL_SPEAR_DUR] <- corr$estimate
		res.tab[meas.name,COL_PVAL_DUR] <- corr$p.value
#print(vals.occ)
#print(vals.cur)
print(length(vals.occ))
print(length(vals.cur))
#		corr <- cor.test(x=vals.occ, y=vals.cur, method="spearman")
		corr <- tryCatch(
			cor.test(x=vals.occ, y=vals.cur, method="spearman"),
			error=function(e) 
			{	msg <- paste0("WARNING: problem when computing Spearman for measure ",meas.name)
				tlog(7,msg)
				warning(msg)
				corr <- list(estimate=NA,p.value=NA)
				return(corr)
			}
		)		
		res.tab[meas.name,COL_SPEAR_OCC] <- corr$estimate
		res.tab[meas.name,COL_PVAL_OCC] <- corr$p.value
		# update file
		write.csv(x=res.tab, file=table.file, row.names=TRUE)#, col.names=TRUE)
	}
	
	tlog(4,"Computation of rank correlation measures complete")
	return(res.tab)
}



###############################################################################
# Computes all rank correlation measures for the specified static graph.
#
# mode: either "segments", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="segments").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="segments").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
###############################################################################
compute.all.static.corrs <- function(mode, window.size=NA, overlap=NA, weights=NA)
{	graph.file <- get.graphname.static(mode, window.size, overlap)
	tlog(3,"Computing all rank correlation measures for \"",graph.file,"\"")
	
	# read the graph file
	tlog(4,"Loading graph")
	g <- read.graph(file=graph.file, format="graphml")
	if(!is.na(weights))
	{	if(weights=="occurrences")
			E(g)$weight <- E(g)$Occurrences
		else if(weights=="duration")
			E(g)$weight <- E(g)$Duration
	}
	else
		E(g)$weight <- E(g)$Occurrences
	
	# init cache
	cache <<- list()
	
	# compute its stats
	compute.static.correlations(mode, window.size, overlap, weights)
	
	tlog(3,"Computation of all rank correlation measures complete")
}



###############################################################################
# Computes all preselected topological measures for the specified static graph.
#
# mode: either "segments", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="segments").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="segments").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
###############################################################################
compute.all.static.statistics <- function(mode, window.size=NA, overlap=NA, weights=NA)
{	graph.file <- get.graphname.static(mode, window.size, overlap)
	tlog(3,"Computing all topological measures for \"",graph.file,"\"")
	
	# read the graph file
	tlog(4,"Loading graph")
	g <- read.graph(file=graph.file, format="graphml")
	if(!is.na(weights))
	{	if(weights=="occurrences")
			E(g)$weight <- E(g)$Occurrences
		else if(weights=="duration")
			E(g)$weight <- E(g)$Duration
	}
	else
		E(g)$weight <- E(g)$Occurrences
	
	# init cache
	cache <<- list()
	
	# compute its stats
	compute.static.node.statistics(g, mode, window.size, overlap, weights)
	compute.static.nodepair.statistics(g, mode, window.size, overlap, weights)
	compute.static.link.statistics(g, mode, window.size, overlap, weights)
	compute.static.graph.statistics(g, mode, window.size, overlap, weights)
#	compute.static.correlations(mode, window.size, overlap, weights)
	
	tlog(3,"Computation of all topological measures complete")
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
	
#	# statistics for the segment-based graph
#	for(weights in c("occurrences","duration"))
#		compute.all.static.statistics(mode="segments", weights=weights)
#	for(weights in c("occurrences","duration"))
#		compute.all.static.corrs(mode="segments", weights=weights)
	
	# statistics for the panel window-based static graphs
	for(i in 1:length(panel.window.sizes))
#	foreach(i=1:length(panel.window.sizes)) %dopar% 
	{	source("src/define_imports.R")
		
		window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
		{	compute.all.static.statistics(mode="panel.window", window.size, overlap)
			#compute.all.static.corrs(mode="panel.window", window.size, overlap)
		}
	}
	
#	# statistics for the page window-based static graphs
#	for(i in 1:length(page.window.sizes))
##	foreach(i=1:length(page.window.sizes)) %dopar% 
#	{	source("src/define_imports.R")
#		
#		window.size <- page.window.sizes[i]
#		for(overlap in page.overlaps[[i]])
#		{	compute.all.static.statistics(mode="page.window", window.size, overlap)
#			compute.all.static.corrs(mode="page.window", window.size, overlap)
#		}
#	}
	tlog(1,"Computation of statistics for static graphs complete")	
}
