# This script contains methods computing 
# community-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
GRAPH_MEASURES[[MEAS_MODULARITY]] <- list( #modularity
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=TRUE)
		modularity(com)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_NBR)]] <- list( #community-number
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		length(communities(com))
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_SIZE,SFX_AVG)]] <- list( #community-size-average
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		mean(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_SIZE,SFX_STDEV)]] <- list( #community-size-stdev
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		sd(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_SIZE,SFX_MIN)]] <- list( #community-size-min
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		min(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_SIZE,SFX_MAX)]] <- list( #community-size-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		max(sizes)
	}
)



# weighted variants
GRAPH_MEASURES[[paste0(MEAS_MODULARITY,SFX_WEIGHT)]] <- list( #modularity-weighted
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=TRUE)
		modularity(com)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_NBR)]] <- list( #community-weighted-number
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		length(communities(com))
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_SIZE,SFX_AVG)]] <- list( #community-weighted-size-average
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		mean(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_SIZE,SFX_STDEV)]] <- list( #community-weighted-size-stdev
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		sd(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_SIZE,SFX_MIN)]] <- list( #community-weighted-size-min
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		min(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_SIZE,SFX_MAX)]] <- list( #community-weighted-size-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	com <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=E(graph)$weight, modularity=FALSE)
		sizes <- sapply(communities(com),length)
		max(sizes)
	}
)
