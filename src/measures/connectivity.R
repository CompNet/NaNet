# This script contains methods computing 
# connectivity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# link connectivity
GRAPH_MEASURES[["link-connectivity-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- edge_connectivity(graph=graph, source=NULL, target=NULL)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["link-connectivity-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- edge_connectivity(graph=graph, source=NULL, target=NULL)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["link-connectivity-min"]] <- list(		# aka adhesion
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- edge_connectivity(graph=graph, source=NULL, target=NULL)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["link-connectivity-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- edge_connectivity(graph=graph, source=NULL, target=NULL)
		max(values,na.rm=TRUE)
	}
)



# node connectivity
GRAPH_MEASURES[["node-connectivity-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- vertex_connectivity(graph=graph, source=NULL, target=NULL)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["node-connectivity-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- vertex_connectivity(graph=graph, source=NULL, target=NULL)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["node-connectivity-min"]] <- list(		# aka cohesion
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- vertex_connectivity(graph=graph, source=NULL, target=NULL)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["node-connectivity-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- vertex_connectivity(graph=graph, source=NULL, target=NULL)
		max(values,na.rm=TRUE)
	}
)
