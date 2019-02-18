# This script contains methods computing 
# component-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_NBR)]] <- list( #component-number
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		length(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_SIZE,SFX_AVG)]] <- list( #component-size-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		mean(sizes,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_SIZE,SFX_STDEV)]] <- list( #component-size-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		sd(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_SIZE,SFX_MIN)]] <- list( #component-size-min
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		min(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_SIZE,SFX_MAX)]] <- list( #component-size-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sizes <- components(graph=graph, mode="weak")$csize
		max(sizes)
	}
)
