# This script contains methods computing 
# edgebetweeness-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
LINK_MEASURES[[MEAS_EDGEBETWNS]] <- list( #edgebetweenness
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_AVG)]] <- list( #edgebetweenness-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_STDEV)]] <- list( #edgebetweenness-stdev
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_MIN)]] <- list( #edgebetweenness-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_MAX)]] <- list( #edgebetweenness-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=NULL)
		max(values,na.rm=TRUE)
	}
)



# weighted variants
LINK_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT)]] <- list( #edgebetweenness-weighted
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT,SFX_AVG)]] <- list( #edgebetweenness-weighted-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT,SFX_STDEV)]] <- list( #edgebetweenness-weighted-stdev
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT,SFX_MIN)]] <- list( #edgebetweenness-weighted-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT,SFX_MAX)]] <- list( #edgebetweenness-weighted-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- edge_betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight)
		max(values,na.rm=TRUE)
	}
)
