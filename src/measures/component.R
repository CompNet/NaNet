# This script contains methods computing 
# component-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
GRAPH_MEASURES[["component-number"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		length(sizes)
	}
)
GRAPH_MEASURES[["component-size-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		mean(sizes,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["component-size-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		sd(sizes)
	}
)
GRAPH_MEASURES[["component-size-min"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		min(sizes)
	}
)
GRAPH_MEASURES[["component-size-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		max(sizes)
	}
)
