# This script contains methods computing 
# degree-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# degree variants
NODE_MEASURES[["degree"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	degree(graph=graph, mode="all", normalized=FALSE)
	}
)
GRAPH_MEASURES[["degree-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["degree-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["degree-min"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["degree-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["degree-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	assortativity_degree(graph=graph, directed=FALSE)
	}
)
GRAPH_MEASURES[["degree-centralization"]] <- list(
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_degree(graph=graph, mode="all", normalized=FALSE)$centralization
	}
)



# normalized variants
NODE_MEASURES[["degree-norm"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	degree(graph=graph, mode="all", normalized=TRUE)
	}
)
GRAPH_MEASURES[["degree-norm-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["degree-norm-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["degree-norm-min"]] <- list(
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["degree-norm-max"]] <- list(
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["degree-norm-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	assortativity_degree(graph=graph, directed=TRUE)
	}
)
GRAPH_MEASURES[["degree-norm-centralization"]] <- list(
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_degree(graph=graph, mode="all", normalized=TRUE)$centralization
	}
)



# strength variants
NODE_MEASURES[["strength"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	strength(graph=graph, mode="all", weights=E(graph)$weight)
	}
)
GRAPH_MEASURES[["strength-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["strength-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["strength-min"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["strength-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["strength-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
