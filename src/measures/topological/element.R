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
	folder="basic",
	object="graph",
	foo=function(graph) 
	{	vcount(graph=graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINK,SFX_NBR)]] <- list( #link-number
	type=integer(),
	bounds=c(0,NA),
	cname="Link Number",
	folder="basic",
	object="graph",
	foo=function(graph) 
	{	ecount(graph=graph)
	}
)
GRAPH_MEASURES[[MEAS_DENSITY]] <- list( #density
	type=numeric(),
	bounds=c(0,1),
	cname="Density",
	folder="basic",
	object="graph",
	foo=function(graph) 
	{	graph.density(graph=graph, loops=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DENSITY,SFX_WEIGHT)]] <- list( #density-weighted
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Density",
	folder="basic",
	object="graph",
	foo=function(graph) 
	{	# we assume the edge weights are normalized in [0;1]
		n <- gorder(graph)
		sum(E(graph)$weight)/(n*(n-1)/2)
	}
)

# link weights
LINK_MEASURES[[MEAS_LINKWEIGHT]] <- list( #linkweight
	type=integer(),
	bounds=c(0,NA),
	cname="Link Weights",
	folder="basic",
	object="links",
	foo=function(graph) 
	{	#values <- as_adjacency_matrix(graph=graph, type="upper", attr="weight", names=FALSE)
		#values <- values[upper.tri(values,diag=FALSE)]
		#return(values)
		E(graph)$weight
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_AVG)]] <- list( #linkweight-average
	type=integer(),
	bounds=c(0,NA),
	cname="Average Link Weight",
	folder="basic",
	object="graph",
	foo=function(graph) 
	{	mean(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_STDEV)]] <- list( #linkweight-stdev
	type=integer(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Link Weights",
	folder="basic",
	object="graph",
	foo=function(graph) 
	{	sd(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_MIN)]] <- list( #linkweight-min
	type=integer(),
	bounds=c(0,NA),
	cname="Minimal Link Weight",
	folder="basic",
	object="graph",
	foo=function(graph) 
	{	min(E(graph)$weight,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LINKWEIGHT,SFX_MAX)]] <- list( #linkweight-max
	type=integer(),
	bounds=c(0,NA),
	cname="Maximal Link Weight",
	folder="basic",
	object="graph",
	foo=function(graph) 
	{	max(E(graph)$weight,na.rm=TRUE)
	}
)
