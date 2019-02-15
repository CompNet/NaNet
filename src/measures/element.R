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
GRAPH_MEASURES[["density"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	graph.density(graph=graph, loops=FALSE)
	}
)

# link weights
LINK_MEASURES[["link-weight"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	E(graph)$weight
	}
)
GRAPH_MEASURES[["link-weight-average"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	mean(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["link-weight-stdev"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sd(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["link-weight-min"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	max(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["link-weight-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	min(E(graph)$weight,na.rm=TRUE)
	}
)
