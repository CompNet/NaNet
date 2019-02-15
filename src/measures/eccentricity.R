# This script contains methods computing 
# eccentricity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
NODE_MEASURES[["eccentricity"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eccentricity(graph=graph, mode="all")
	}
)
GRAPH_MEASURES[["eccentricity-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eccentricity-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eccentricity-min"]] <- list(		# aka radius
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eccentricity-max"]] <- list(	# aka diameter, or maximal distance
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eccentricity-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
