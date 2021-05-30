# This script contains methods computing 
# node/link-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
GRAPH_MEASURES[[paste0(MEAS_NODE,SFX_NBR)]] <- list( #node-number
	type=integer(),
	bounds=c(0,NA),
	cname="Node Number",
	foo=function(graph) 
	{	vcount(graph=graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINK,SFX_NBR)]] <- list( #link-number
	type=integer(),
	bounds=c(0,NA),
	cname="Link Number",
	foo=function(graph) 
	{	ecount(graph=graph)
	}
)
GRAPH_MEASURES[[MEAS_DENSITY]] <- list( #density
	type=numeric(),
	bounds=c(0,1),
	cname="Density",
	foo=function(graph) 
	{	graph.density(graph=graph, loops=FALSE)
	}
)

# link weights
NODEPAIR_MEASURES[[MEAS_LINKWEIGHT]] <- list( #linkweight
	type=integer(),
	bounds=c(0,NA),
	cname="Link Weights",
	foo=function(graph) 
	{	#E(graph)$weight
		values <- as_adjacency_matrix(graph=graph, type="upper", attr="weight", names=FALSE)
		values <- values[upper.tri(values,diag=FALSE)]
		return(values)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_AVG)]] <- list( #linkweight-average
	type=integer(),
	bounds=c(0,NA),
	cname="Average Link Weight",
	foo=function(graph) 
	{	mean(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_STDEV)]] <- list( #linkweight-stdev
	type=integer(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Link Weights",
	foo=function(graph) 
	{	sd(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_MIN)]] <- list( #linkweight-min
	type=integer(),
	bounds=c(0,NA),
	cname="Minimal Link Weight",
	foo=function(graph) 
	{	min(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_MAX)]] <- list( #linkweight-max
	type=integer(),
	bounds=c(0,NA),
	cname="Maximal Link Weight",
	foo=function(graph) 
	{	max(E(graph)$weight,na.rm=TRUE)
	}
)
