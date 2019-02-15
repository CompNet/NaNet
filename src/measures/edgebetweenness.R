# This script contains methods computing 
# edgebetweeness-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
LINK_MEASURES[["edgebetweenness"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
	}
)
GRAPH_MEASURES[["edgebetweenness-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["edgebetweenness-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["edgebetweenness-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["edgebetweenness-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
		max(values,na.rm=TRUE)
	}
)



# weighted variants
LINK_MEASURES[["edgebetweenness-weighted"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
	}
)
GRAPH_MEASURES[["edgebetweenness-weighted-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["edgebetweenness-weighted-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["edgebetweenness-weighted-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["edgebetweenness-weighted-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
		max(values,na.rm=TRUE)
	}
)
