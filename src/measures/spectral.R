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
	{	eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
	}
)
GRAPH_MEASURES[["eigenvector-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[["eigenvector-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_eigen(graph=graph, directed=FALSE, scale=FALSE, normalized=TRUE)$centralization
	}
)



# normalized variants
NODE_MEASURES[["eigenvector-norm"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
	}
)
GRAPH_MEASURES[["eigenvector-norm-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[["eigenvector-norm-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_eigen(graph=graph, directed=FALSE, scale=TRUE, normalized=TRUE)$centralization
	}
)



# weighted variants
NODE_MEASURES[["eigenvector-weighted"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
	}
)
GRAPH_MEASURES[["eigenvector-weighted-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-weighted-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-weighted-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-weighted-weighted-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[["eigenvector-weighted-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)



# weighted variants
NODE_MEASURES[["eigenvector-weighted-norm"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-average"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			mean(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-stdev"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			sd(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-min"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			min(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-weighted-max"]] <- list(
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			max(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[["eigenvector-weighted-norm-assortativity"]] <- list(
		type=numeric(),
		bounds=c(-1,1),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
		}
)
