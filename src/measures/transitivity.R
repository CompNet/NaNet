# This script contains methods computing 
# transitivity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
NODE_MEASURES[["transitivity-local"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
	}
)
GRAPH_MEASURES[["transitivity-local-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["transitivity-local-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["transitivity-local-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["transitivity-local-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["transitivity-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[["transitivity-global"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	transitivity(graph=graph, type="globalundirected", weights=NULL, isolates="zero")
	}
)



# weighted variants
NODE_MEASURES[["transitivity-weighted-local"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
	}
)
GRAPH_MEASURES[["transitivity-weighted-local-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["transitivity-weighted-local-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["transitivity-weighted-local-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["transitivity-weighted-local-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["transitivity-weighted-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
