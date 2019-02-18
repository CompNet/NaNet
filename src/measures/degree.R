# This script contains methods computing 
# degree-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# degree variants
NODE_MEASURES[[paste0(MEAS_DEGREE)]] <- list( #degree
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	degree(graph=graph, mode="all", normalized=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_AVG)]] <- list( #degree-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_STDEV)]] <- list( #degree-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_MIN)]] <- list( #degree-min
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_MAX)]] <- list( #degree-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_ASSORT)]] <- list( #degree-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	assortativity_degree(graph=graph, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_CENTRZ)]] <- list( #degree-centralization
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_degree(graph=graph, mode="all", normalized=FALSE)$centralization
	}
)



# normalized variants
NODE_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM)]] <- list( #degree-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	degree(graph=graph, mode="all", normalized=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_AVG)]] <- list( #degree-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_STDEV)]] <- list( #degree-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_MIN)]] <- list( #degree-norm-min
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_MAX)]] <- list( #degree-norm-max
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- degree(graph=graph, mode="all", normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_ASSORT)]] <- list( #degree-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	assortativity_degree(graph=graph, directed=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_CENTRZ)]] <- list( #degree-norm-centralization
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_degree(graph=graph, mode="all", normalized=TRUE)$centralization
	}
)



# strength variants
NODE_MEASURES[[paste0(MEAS_STRENGTH)]] <- list( #strength
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	strength(graph=graph, mode="all", weights=E(graph)$weight)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_AVG)]] <- list( #strength-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_STDEV)]] <- list( #strength-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_MIN)]] <- list( #strength-min
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_MAX)]] <- list( #strength-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_ASSORT)]] <- list( #strength-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
