# This script contains methods computing 
# spectral measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.spectral <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_EIGENCNTR)
			res <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=NA)$vector
		else if(name==paste0(MEAS_EIGENCNTR,SFX_NORM))
			res <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=NA)$vector
		else if(name==paste0(MEAS_EIGENCNTR,SFX_WEIGHT))
			res <- eigen_centrality(graph=graph, directed=FALSE, scale=FALSE, weights=E(graph)$weight)$vector
		else if(name==paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM))
			res <- eigen_centrality(graph=graph, directed=FALSE, scale=TRUE, weights=E(graph)$weight)$vector
		cache[[name]] <<- res
	}
}



# basic variants
NODE_MEASURES[[MEAS_EIGENCNTR]] <- list( #eigenvector
	type=numeric(),
	bounds=c(0,NA),
	cname="Eigencentrality",
	foo=function(graph) 
	{	compute.spectral(MEAS_EIGENCNTR, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_AVG)]] <- list( #eigenvector-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(MEAS_EIGENCNTR, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_STDEV)]] <- list( #eigenvector-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(MEAS_EIGENCNTR, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_MIN)]] <- list( #eigenvector-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Minimal Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(MEAS_EIGENCNTR, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_MAX)]] <- list( #eigenvector-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(MEAS_EIGENCNTR, graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_ASSORT)]] <- list( #eigenvector-assortativity
	type=numeric(),
	bounds=c(-1,1),
	cname="Eigencentrality Assortativity",
	foo=function(graph) 
	{	values <- compute.spectral(MEAS_EIGENCNTR, graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_CENTRZ)]] <- list( #eigenvector-centralization
	type=numeric(),
	bounds=c(0,NA),
	cname="Eigencentrality Centralization",
	foo=function(graph) 
	{	centr_eigen(graph=graph, directed=FALSE, scale=FALSE, normalized=TRUE)$centralization
	}
)



# normalized variants
NODE_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM)]] <- list( #eigenvector-norm
	type=numeric(),
	bounds=c(0,NA),
	cname="Normalized Eigencentrality",
	foo=function(graph) 
	{	compute.spectral(paste0(MEAS_EIGENCNTR,SFX_NORM), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_AVG)]] <- list( #eigenvector-norm-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Normalized Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_NORM), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_STDEV)]] <- list( #eigenvector-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Normalized Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_NORM), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_MIN)]] <- list( #eigenvector-norm-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Mininmal Normalized Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_NORM), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_MAX)]] <- list( #eigenvector-norm-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Normalized Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_NORM), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_ASSORT)]] <- list( #eigenvector-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	cname="Normalized Eigencentrality Assortativity",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_NORM), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_NORM,SFX_CENTRZ)]] <- list( #eigenvector-norm-centralization
	type=numeric(),
	bounds=c(0,1),
	cname="Normalized Eigencentrality Centralization",
	foo=function(graph) 
	{	centr_eigen(graph=graph, directed=FALSE, scale=TRUE, normalized=TRUE)$centralization
	}
)



# weighted variants
NODE_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT)]] <- list( #eigenvector-weighted
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Eigencentrality",
	foo=function(graph) 
	{	compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_AVG)]] <- list( #eigenvector-weighted-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Weighted Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_STDEV)]] <- list( #eigenvector-weighted-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Weighted Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_MIN)]] <- list( #eigenvector-weighted-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Minimal Weighted Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_WEIGHT,SFX_MAX)]] <- list( #eigenvector-weighted-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Weighted Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_ASSORT)]] <- list( #eigenvector-weighted-assortativity
	type=numeric(),
	bounds=c(-1,1),
	cname="Weighted Eigencentrality Assortativity",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_CENTRZ)]] <- list( #eigenvector-weighted-centralization
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Eigencentrality Centralization",
	foo=function(graph)
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT), graph)
		centralize(scores=values, normalized=FALSE)
	}
)



# weighted variants
NODE_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM)]] <- list( #eigenvector-weighted-norm
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Normalized Eigencentrality",
	foo=function(graph) 
	{	compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_AVG)]] <- list( #eigenvector-weighted-norm-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Weighted Normalized Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_STDEV)]] <- list( #eigenvector-weighted-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Weighted Normalized Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_MIN)]] <- list( #eigenvector-weighted-norm-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Minimal Weighted Normalized Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_WEIGHT,SFX_MAX)]] <- list( #eigenvector-weighted-norm-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Weighted Normalized Eigencentrality",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM,SFX_ASSORT)]] <- list( #eigenvector-weighted-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	cname="Weighted Normalized Eigencentrality Assortativity",
	foo=function(graph) 
	{	values <- compute.spectral(paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_NORM), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
