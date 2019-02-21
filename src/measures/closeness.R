# This script contains methods computing 
# closeness-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.closeness <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_CLOSENESS)
			res <- closeness(graph=graph, mode="all", weights=NA, normalized=FALSE)
		else if(name==paste0(MEAS_CLOSENESS,SFX_NORM))
			res <- closeness(graph=graph, mode="all", weights=NA, normalized=TRUE)
		else if(name==paste0(MEAS_CLOSENESS,SFX_WEIGHT))
			res <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		else if(name==paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM))
			res <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		cache[[name]] <<- res
	}
}



# basic variants
NODE_MEASURES[[MEAS_CLOSENESS]] <- list( #closeness
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	compute.closeness(MEAS_CLOSENESS, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_AVG)]] <- list( #closeness-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(MEAS_CLOSENESS, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_STDEV)]] <- list( #closeness-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(MEAS_CLOSENESS, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_MIN)]] <- list( #closeness-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(MEAS_CLOSENESS, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_MAX)]] <- list( #closeness-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(MEAS_CLOSENESS, graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_ASSORT)]] <- list( #closeness-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- compute.closeness(MEAS_CLOSENESS, graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_CENTRZ)]] <- list( #closeness-centralization
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	centr_clo(graph=graph, mode="all", normalized=FALSE)$centralization
	}
)



# normalized variants
NODE_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM)]] <- list( #closeness-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	compute.closeness(paste0(MEAS_CLOSENESS,SFX_NORM), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_AVG)]] <- list( #closeness-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_NORM), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_STDEV)]] <- list( #closeness-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_NORM), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_MIN)]] <- list( #closeness-norm-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_NORM), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_MAX)]] <- list( #closeness-norm-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_NORM), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_ASSORT)]] <- list( #closeness-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_NORM), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_CENTRZ)]] <- list( #closeness-norm-centralization
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centr_clo(graph=graph, mode="all", normalized=TRUE)$centralization
	}
)



# weighted variants
NODE_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT)]] <- list( #closeness-weighted
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_AVG)]] <- list( #closeness-weighted-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_STDEV)]] <- list( #closeness-weighted-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_MIN)]] <- list( #closeness-weighted-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_MAX)]] <- list( #closeness-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_ASSORT)]] <- list( #closeness-weighted-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)



# normalized weighted variants
NODE_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM)]] <- list( #closeness-weighted-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_AVG)]] <- list( #closeness-weighted-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_STDEV)]] <- list( #closeness-weighted-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_MIN)]] <- list( #closeness-weighted-norm-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_MAX)]] <- list( #closeness-weighted-norm-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_ASSORT)]] <- list( #closeness-weighted-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- compute.closeness(paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
