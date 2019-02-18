# This script contains methods computing 
# spectral measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
NODE_MEASURES[[MEAS_EIGENCNTR]] <- list( #eigenvector
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_AVG)]] <- list( #eigenvector-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_STDEV)]] <- list( #eigenvector-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_MIN)]] <- list( #eigenvector-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_MAX)]] <- list( #eigenvector-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_ASSORT)]] <- list( #eigenvector-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_CENTRZ)]] <- list( #eigenvector-centralization
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_eigen(graph=graph, directed=FALSE, scale=FALSE, normalized=TRUE)$centralization
	}
)



# normalized variants
NODE_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM)]] <- list( #eigenvector-norm
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_AVG)]] <- list( #eigenvector-norm-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_STDEV)]] <- list( #eigenvector-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_MIN)]] <- list( #eigenvector-norm-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_MAX)]] <- list( #eigenvector-norm-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_ASSORT)]] <- list( #eigenvector-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NULL)$vector
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_CENTRZ)]] <- list( #eigenvector-norm-centralization
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_eigen(graph=graph, directed=FALSE, scale=TRUE, normalized=TRUE)$centralization
	}
)



# weighted variants
NODE_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT)]] <- list( #eigenvector-weighted
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_AVG)]] <- list( #eigenvector-weighted-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_STDEV)]] <- list( #eigenvector-weighted-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_MIN)]] <- list( #eigenvector-weighted-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_WEIGHT,SFX_MAX)]] <- list( #eigenvector-weighted-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_ASSORT)]] <- list( #eigenvector-weighted-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)



# weighted variants
NODE_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM)]] <- list( #eigenvector-weighted-norm
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
		}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_AVG)]] <- list( #eigenvector-weighted-norm-average
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			mean(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_STDEV)]] <- list( #eigenvector-weighted-norm-stdev
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			sd(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_MIN)]] <- list( #eigenvector-weighted-norm-min
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			min(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_WEIGHT,SFX_MAX)]] <- list( #eigenvector-weighted-norm-weighted-max
		type=numeric(),
		bounds=c(0,NA),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			max(values,na.rm=TRUE)
		}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_ASSORT)]] <- list( #eigenvector-weighted-norm-assortativity
		type=numeric(),
		bounds=c(-1,1),
		foo=function(graph) 
		{	values <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
			assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
		}
)
