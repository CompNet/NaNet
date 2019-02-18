# This script contains methods computing 
# node/link-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
GRAPH_MEASURES[[paste0(MEAS_NODE,SFX_NBR)]] <- list( #node-number
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	vcount(graph=graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINK,SFX_NBR)]] <- list( #link-number
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	ecount(graph=graph)
	}
)
GRAPH_MEASURES[[MEAS_DENSITY]] <- list( #density
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	graph.density(graph=graph, loops=FALSE)
	}
)

# link weights
LINK_MEASURES[[MEAS_LINKWEIGHT]] <- list( #linkweight
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	E(graph)$weight
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_AVG)]] <- list( #linkweight-average
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	mean(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_STDEV)]] <- list( #linkweight-stdev
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	sd(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_MIN)]] <- list( #linkweight-min
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	max(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_MAX)]] <- list( #linkweight-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	min(E(graph)$weight,na.rm=TRUE)
	}
)
