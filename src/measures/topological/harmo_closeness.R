# This script contains methods computing 
# harmonic closeness-related measures.
# 
# Vincent Labatut
# 05/2021
###############################################################################
# cache function
compute.harmo.closeness <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	# compute the measure
		if(name==MEAS_HARMO_CLOSENESS)
			res <- harmonic_centrality(x=graph, mode="all", weights=NA)
		else if(name==paste0(MEAS_HARMO_CLOSENESS,SFX_NORM))
			res <- harmonic_centrality(x=graph, mode="all", weights=NA)/(length(which(igraph::degree(graph)>0))-1)
		else if(name==paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT))
			res <- harmonic_centrality(x=graph, mode="all", weights=reverse.weights(E(graph)$weight))
		else if(name==paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM))
			res <- harmonic_centrality(x=graph, mode="all", weights=reverse.weights(E(graph)$weight))/(length(which(igraph::degree(graph)>0))-1)
		
		# possibly clean
		res[is.nan(res)] <- NA
		
		cache[[name]] <<- res
	}
}



# basic variants
NODE_MEASURES[[MEAS_HARMO_CLOSENESS]] <- list( #harmo-closeness
	type=numeric(),
	bounds=c(0,NA),
	cname="Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="nodes",
	foo=function(graph) 
	{	compute.harmo.closeness(MEAS_HARMO_CLOSENESS, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_AVG)]] <- list( #harmo-closeness-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(MEAS_HARMO_CLOSENESS, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_STDEV)]] <- list( #harmo-closeness-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(MEAS_HARMO_CLOSENESS, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_MIN)]] <- list( #harmo-closeness-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Minimal Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(MEAS_HARMO_CLOSENESS, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_MAX)]] <- list( #harmo-closeness-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(MEAS_HARMO_CLOSENESS, graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_ASSORT)]] <- list( #harmo-closeness-assortativity
	type=numeric(),
	bounds=c(-1,1),
	cname="Harmonic Closeness Assortativity",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(MEAS_HARMO_CLOSENESS, graph)
		idx <- which(!is.na(values))
		assortativity(graph=induced_subgraph(graph,idx), types1=values[idx], types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_CENTRZ)]] <- list( #harmo-closeness-centralization
	type=numeric(),
	bounds=c(0,NA),
	cname="Harmonic Closeness Centralization",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph)
	{	values <- compute.harmo.closeness(MEAS_HARMO_CLOSENESS, graph)
		idx <- which(!is.na(values))
		centralize(scores=values[idx], normalized=FALSE)
	}
)



# normalized variants
NODE_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_NORM)]] <- list( #harmo-closeness-norm
	type=numeric(),
	bounds=c(0,1),
	cname="Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="nodes",
	foo=function(graph) 
	{	compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_NORM), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_NORM,SFX_AVG)]] <- list( #harmo-closeness-norm-average
	type=numeric(),
	bounds=c(0,1),
	cname="Average Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_NORM), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_NORM,SFX_STDEV)]] <- list( #harmo-closeness-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_NORM), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_NORM,SFX_MIN)]] <- list( #harmo-closeness-norm-min
	type=numeric(),
	bounds=c(0,1),
	cname="Minimal Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_NORM), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_NORM,SFX_MAX)]] <- list( #harmo-closeness-norm-max
	type=numeric(),
	bounds=c(0,1),
	cname="Maximal Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_NORM), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_NORM,SFX_ASSORT)]] <- list( #harmo-closeness-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	cname="Normalized Harmonic Closeness Assortativity",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_NORM), graph)
		idx <- which(!is.na(values))
		assortativity(graph=induced_subgraph(graph,idx), types1=values[idx], types2=NULL, directed=FALSE)
	}
)



# weighted variants
NODE_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT)]] <- list( #harmo-closeness-weighted
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="nodes",
	foo=function(graph) 
	{	compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_AVG)]] <- list( #harmo-closeness-weighted-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Weighted Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_STDEV)]] <- list( #harmo-closeness-weighted-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Weighted Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_MIN)]] <- list( #harmo-closeness-weighted-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Minimal Weighted Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_MAX)]] <- list( #harmo-closeness-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Weighted Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_ASSORT)]] <- list( #harmo-closeness-weighted-assortativity
	type=numeric(),
	bounds=c(-1,1),
	cname="Weighted Harmonic Closeness Assortativity",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT), graph)
		idx <- which(!is.na(values))
		assortativity(graph=induced_subgraph(graph,idx), types1=values[idx], types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_CENTRZ)]] <- list( #harmo-closeness-weighted-centralization
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Harmonic Closeness Centralization",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph)
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT), graph)
		idx <- which(!is.na(values))
		centralize(scores=values[idx], normalized=FALSE)
	}
)



# normalized weighted variants
NODE_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM)]] <- list( #harmo-closeness-weighted-norm
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="nodes",
	foo=function(graph) 
	{	compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_AVG)]] <- list( #harmo-closeness-weighted-norm-average
	type=numeric(),
	bounds=c(0,1),
	cname="Average Weighted Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_STDEV)]] <- list( #harmo-closeness-weighted-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Weighted Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_MIN)]] <- list( #harmo-closeness-weighted-norm-min
	type=numeric(),
	bounds=c(0,1),
	cname="Minimal Weighted Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_MAX)]] <- list( #harmo-closeness-weighted-norm-max
	type=numeric(),
	bounds=c(0,1),
	cname="Maximal Weighted Normalized Harmonic Closeness",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_ASSORT)]] <- list( #harmo-closeness-weighted-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	cname="Weighted Normalized Harmonic Closeness Assortativity",
	folder=MEAS_HARMO_CLOSENESS,
	object="graph",
	foo=function(graph) 
	{	values <- compute.harmo.closeness(paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		idx <- which(!is.na(values))
		assortativity(graph=induced_subgraph(graph,idx), types1=values[idx], types2=NULL, directed=FALSE)
	}
)
