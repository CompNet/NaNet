# This script contains methods computing 
# connectivity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# link connectivity
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_AVG)]] <- list( #link-connectivity-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- edge_connectivity(graph=graph, source=NULL, target=NULL)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_STDEV)]] <- list( #link-connectivity-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- edge_connectivity(graph=graph, source=NULL, target=NULL)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_MIN)]] <- list( #link-connectivity-min
	# aka adhesion
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- edge_connectivity(graph=graph, source=NULL, target=NULL)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_MAX)]] <- list( #link-connectivity-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- edge_connectivity(graph=graph, source=NULL, target=NULL)
		max(values,na.rm=TRUE)
	}
)



# node connectivity
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_AVG)]] <- list( #node-connectivity-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- vertex_connectivity(graph=graph, source=NULL, target=NULL)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_STDEV)]] <- list( #node-connectivity-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- vertex_connectivity(graph=graph, source=NULL, target=NULL)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_MIN)]] <- list( #node-connectivity-min
	# aka cohesion
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- vertex_connectivity(graph=graph, source=NULL, target=NULL)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_MAX)]] <- list( #node-connectivity-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- vertex_connectivity(graph=graph, source=NULL, target=NULL)
		max(values,na.rm=TRUE)
	}
)
