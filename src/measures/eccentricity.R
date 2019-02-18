# This script contains methods computing 
# eccentricity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
NODE_MEASURES[[paste0(MEAS_ECCENTRICITY)]] <- list( #eccentricity
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	eccentricity(graph=graph, mode="all")
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_AVG)]] <- list( #eccentricity-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_STDEV)]] <- list( #eccentricity-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_MIN)]] <- list( #eccentricity-min
	# aka radius
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_MAX)]] <- list( #eccentricity-max
	# aka diameter, or maximal distance
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_ASSORT)]] <- list( #eccentricity-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- eccentricity(graph=graph, mode="all")
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
