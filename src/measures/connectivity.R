# This script contains methods computing 
# connectivity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.connectivity <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_LK_CONNECT)
			res <- edge_connectivity(graph=graph, source=NULL, target=NULL)
		else if(name==MEAS_ND_CONNECT)
			res <- vertex_connectivity(graph=graph, source=NULL, target=NULL)
		cache[[name]] <<- res
	}
}



# link connectivity
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_AVG)]] <- list( #link-connectivity-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_LK_CONNECT, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_STDEV)]] <- list( #link-connectivity-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_LK_CONNECT, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_MIN)]] <- list( #link-connectivity-min
	# aka adhesion
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_LK_CONNECT, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_MAX)]] <- list( #link-connectivity-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_LK_CONNECT, graph)
		max(values,na.rm=TRUE)
	}
)



# node connectivity
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_AVG)]] <- list( #node-connectivity-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_ND_CONNECT, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_STDEV)]] <- list( #node-connectivity-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_ND_CONNECT, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_MIN)]] <- list( #node-connectivity-min
	# aka cohesion
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_ND_CONNECT, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_MAX)]] <- list( #node-connectivity-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_ND_CONNECT, graph)
		max(values,na.rm=TRUE)
	}
)
