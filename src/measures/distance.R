# This script contains methods computing 
# distance-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# unweighted variants
GRAPH_MEASURES[["distance-average"]] <- list(
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	mean_distance(graph=graph, directed=FALSE, unconnected=TRUE)
	}
)
GRAPH_MEASURES[["distance-stdev"]] <- list(
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["girth"]] <- list(		# cycle of minimal length
	type=integer(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	girth(graph=graph, circle=FALSE)$girth
	}
)



# weighted variants
GRAPH_MEASURES[["distance-weighted-average"]] <- list(
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["distance-weighted-stdev"]] <- list(
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		sd(values,na.rm=TRUE)
	}
)
