# This script contains methods computing 
# spectral measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
NODE_MEASURES[["eigenvector"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)
	}
)
GRAPH_MEASURES[["eigenvector-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[["eigenvector-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_eigen(graph=graph, directed=FALSE, scale=FALSE, normalized=TRUE)
	}
)



# normalized variants
NODE_MEASURES[["eigenvector-norm"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)
	}
)
GRAPH_MEASURES[["eigenvector-norm-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_eigen(graph=graph, directed=FALSE, scale=TRUE, normalized=TRUE)
	}
)



# weighted variants
NODE_MEASURES[["eigenvector-weighted"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)
	}
)
GRAPH_MEASURES[["eigenvector-weighted-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-weighted-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-weighted-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-weighted-weighted-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-weighted-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)



# weighted variants
NODE_MEASURES[["eigenvector-weighted-norm"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-average"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)
			mean(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-stdev"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)
			sd(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-min"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)
			min(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-weighted-max"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)
			max(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-assortativity"]] <- list(
		type=numeric(),
		bounds=c(-1,1),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)
			assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
		}
)
