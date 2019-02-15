# This script contains methods computing 
# node/link-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
GRAPH_MEASURES[["node-count"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	vcount(graph=graph)
	}
)
GRAPH_MEASURES[["link-count"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	ecount(graph=graph)
	}
)
LINK_MEASURES[["link-weights"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	E(graph)$weight
	}
)
GRAPH_MEASURES[["density"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	graph.density(graph=graph, loops=FALSE)
	}
)
