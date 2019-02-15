# This script contains methods computing 
# community-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
GRAPH_MEASURES[["modularity"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=TRUE)
		modularity(com)
	}
)
GRAPH_MEASURES[["community-number"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		length(communities(com))
	}
)
GRAPH_MEASURES[["community-size-average"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		mean(sizes)
	}
)
GRAPH_MEASURES[["community-size-stdev"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		sd(sizes)
	}
)
GRAPH_MEASURES[["community-size-min"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		min(sizes)
	}
)
GRAPH_MEASURES[["community-size-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		max(sizes)
	}
)



# weighted variants
GRAPH_MEASURES[["modularity"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=TRUE)
		modularity(com)
	}
)
GRAPH_MEASURES[["community-number"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		length(communities(com))
	}
)
GRAPH_MEASURES[["community-size-average"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		mean(sizes)
	}
)
GRAPH_MEASURES[["community-size-stdev"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		sd(sizes)
	}
)
GRAPH_MEASURES[["community-size-min"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		min(sizes)
	}
)
GRAPH_MEASURES[["community-size-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		max(sizes)
	}
)
