# This script contains methods computing 
# closeness-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
NODE_MEASURES[["closeness"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
	}
)
GRAPH_MEASURES[["closeness-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[["closeness-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centr_clo(graph=graph, mode="all", normalized=FALSE)$centralization
	}
)



# normalized variants
NODE_MEASURES[["closeness-norm"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
	}
)
GRAPH_MEASURES[["closeness-norm-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-norm-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-norm-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-norm-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-norm-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[["closeness-norm-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centr_clo(graph=graph, mode="all", normalized=TRUE)$centralization
	}
)



# weighted variants
NODE_MEASURES[["closeness-weighted"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
	}
)
GRAPH_MEASURES[["closeness-weighted-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-weighted-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-weighted-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-weighted-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-weighted-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)



# normalized weighted variants
NODE_MEASURES[["closeness-weighted-norm"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
	}
)
GRAPH_MEASURES[["closeness-weighted-norm-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-weighted-norm-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-weighted-norm-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-weighted-norm-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["closeness-weighted-norm-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
