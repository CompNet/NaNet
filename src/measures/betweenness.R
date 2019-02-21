# This script contains methods computing 
# betweenness-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.betweenness <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_BETWEENNESS)
			res <- betweenness(graph=graph, directed=FALSE, weights=NA, normalized=FALSE)
		else if(name==paste0(MEAS_BETWEENNESS,SFX_NORM))
			res <- betweenness(graph=graph, directed=FALSE, weights=NA, normalized=TRUE)
		else if(name==paste0(MEAS_BETWEENNESS,SFX_WEIGHT))
			res <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		else if(name==paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM))
			res <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		cache[[name]] <<- res
	}
}



# basic variants
NODE_MEASURES[[MEAS_BETWEENNESS]] <- list( #betweenness
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	compute.betweenness(MEAS_BETWEENNESS, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_AVG)]] <- list( #betweenness-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.betweenness(MEAS_BETWEENNESS, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_STDEV)]] <- list( #betweenness-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- compute.betweenness(MEAS_BETWEENNESS, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_MIN)]] <- list( #betweenness-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- compute.betweenness(MEAS_BETWEENNESS, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_MAX)]] <- list( #betweenness-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- compute.betweenness(MEAS_BETWEENNESS, graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_ASSORT)]] <- list( #betweenness-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- compute.betweenness(MEAS_BETWEENNESS, graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_CENTRZ)]] <- list( #betweenness-centralization
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	centr_betw(graph=graph, directed=FALSE, normalized=FALSE)$centralization
	}
)



# normalized variants
NODE_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM)]] <- list( #betweenness-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_NORM), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_AVG)]] <- list( #betweenness-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_NORM), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_STDEV)]] <- list( #betweenness-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_NORM), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_MIN)]] <- list( #betweenness-norm-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_NORM), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_MAX)]] <- list( #betweenness-norm-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_NORM), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_ASSORT)]] <- list( #betweenness-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_NORM), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_CENTRZ)]] <- list( #betweenness-norm-centralization
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centr_betw(graph=graph, directed=FALSE, normalized=TRUE)$centralization
	}
)



# weighted variants
NODE_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT)]] <- list( #betweenness-weighted
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_AVG)]] <- list( #betweenness-weighted-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_STDEV)]] <- list( #betweenness-weighted-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_MIN)]] <- list( #betweenness-weighted-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_MAX)]] <- list( #betweenness-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_ASSORT)]] <- list( #betweenness-weighted-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)



# normalized weighted variants
NODE_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM)]] <- list( #betweenness-weighted-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_AVG)]] <- list( #betweenness-weighted-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_STDEV)]] <- list( #betweenness-weighted-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_MIN)]] <- list( #betweenness-weighted-norm-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_MAX)]] <- list( #betweenness-weighted-norm-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_ASSORT)]] <- list( #betweenness-weighted-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- compute.betweenness(paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
