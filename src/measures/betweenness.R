# This script contains methods computing 
# betweenness-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
NODE_MEASURES[["betweenness"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
	}
)
GRAPH_MEASURES[["betweenness-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[["betweenness-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centr_betw(graph=graph, directed=FALSE, normalized=FALSE)$centralization
	}
)



# normalized variants
NODE_MEASURES[["betweenness-norm"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-norm-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-norm-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-norm-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-norm-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-norm-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[["betweenness-norm-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centr_betw(graph=graph, directed=FALSE, normalized=TRUE)$centralization
	}
)



# weighted variants
NODE_MEASURES[["betweenness-weighted"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)



# normalized weighted variants
NODE_MEASURES[["betweenness-weighted-norm"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-norm-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-norm-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-norm-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-norm-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["betweenness-weighted-norm-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
