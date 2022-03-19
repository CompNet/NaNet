# This script contains methods computing 
# component-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.components <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_COMPONENT)
			res <- components(graph=graph, mode="weak")$csize
		cache[[name]] <<- res
	}
}


# basic variants
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_NBR)]] <- list( #component-number
	type=integer(),
	bounds=c(0,NA),
	cname="Component Number",
	folder=MEAS_COMPONENT,
	object="graph",
	foo=function(graph) 
	{	sizes <- compute.components(MEAS_COMPONENT, graph)
		length(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_SIZE,SFX_AVG)]] <- list( #component-size-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Component Size",
	folder=MEAS_COMPONENT,
	object="graph",
	foo=function(graph) 
	{	sizes <- compute.components(MEAS_COMPONENT, graph)
		mean(sizes,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_SIZE,SFX_STDEV)]] <- list( #component-size-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Component Sizes",
	folder=MEAS_COMPONENT,
	object="graph",
	foo=function(graph) 
	{	sizes <- compute.components(MEAS_COMPONENT, graph)
		sd(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_SIZE,SFX_MIN)]] <- list( #component-size-min
	type=integer(),
	bounds=c(0,NA),
	cname="Minimal Component Size",
	folder=MEAS_COMPONENT,
	object="graph",
	foo=function(graph) 
	{	sizes <- compute.components(MEAS_COMPONENT, graph)
		min(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMPONENT,SFX_SIZE,SFX_MAX)]] <- list( #component-size-max
	type=integer(),
	bounds=c(0,NA),
	cname="Maximal Component Size",
	folder=MEAS_COMPONENT,
	object="graph",
	foo=function(graph) 
	{	sizes <- compute.components(MEAS_COMPONENT, graph)
		max(sizes)
	}
)
