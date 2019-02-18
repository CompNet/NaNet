# This script contains methods computing 
# closeness-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
NODE_MEASURES[[MEAS_CLOSENESS]] <- list( #closeness
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_AVG)]] <- list( #closeness-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_STDEV)]] <- list( #closeness-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_MIN)]] <- list( #closeness-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_MAX)]] <- list( #closeness-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_ASSORT)]] <- list( #closeness-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=FALSE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_CENTRZ)]] <- list( #closeness-centralization
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centr_clo(graph=graph, mode="all", normalized=FALSE)$centralization
	}
)



# normalized variants
NODE_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM)]] <- list( #closeness-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_AVG)]] <- list( #closeness-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_STDEV)]] <- list( #closeness-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_MIN)]] <- list( #closeness-norm-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_MAX)]] <- list( #closeness-norm-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_NORM,SFX_ASSORT)]] <- list( #closeness-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=NULL, normalized=TRUE)
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
	{	closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_AVG)]] <- list( #closeness-weighted-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_STDEV)]] <- list( #closeness-weighted-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_MIN)]] <- list( #closeness-weighted-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_MAX)]] <- list( #closeness-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_ASSORT)]] <- list( #closeness-weighted-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=FALSE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)



# normalized weighted variants
NODE_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM)]] <- list( #closeness-weighted-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_AVG)]] <- list( #closeness-weighted-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_STDEV)]] <- list( #closeness-weighted-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_MIN)]] <- list( #closeness-weighted-norm-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_MAX)]] <- list( #closeness-weighted-norm-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_NORM,SFX_ASSORT)]] <- list( #closeness-weighted-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- closeness(graph=graph, mode="all", weights=E(graph)$weight, normalized=TRUE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
