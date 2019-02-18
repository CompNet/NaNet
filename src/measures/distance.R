# This script contains methods computing 
# distance-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# unweighted variants
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_AVG)]] <- list( #distance-average
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	mean_distance(graph=graph, directed=FALSE, unconnected=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_STDEV)]] <- list( #distance-stdev
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_MIN)]] <- list( #distance-min
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_MAX)]] <- list( #distance-max
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		max(values,na.rm=TRUE)
	}
)
#GRAPH_MEASURES[["girth"]] <- list(		# cycle of minimal length
#	type=integer(),
#	bounds=c(1,NA),
#	foo=function(graph) 
#	{	girth(graph=graph, circle=FALSE)$girth
#	}
#)



# weighted variants
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_WEIGHT,SFX_AVG)]] <- list( #distance-weighted-average
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_WEIGHT,SFX_STDEV)]] <- list( #distance-weighted-stdev
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_WEIGHT,SFX_MIN)]] <- list( #distance-weighted-min
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_WEIGHT,SFX_MAX)]] <- list( #distance-weighted-max
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	values <- distances(graph=graph, mode="all", weights=E(graph)$weight)
		max(values,na.rm=TRUE)
	}
)
