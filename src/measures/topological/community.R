# This script contains methods computing 
# community-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.communities <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_MODULARITY)
			res <- cluster_infomap(graph=graph, e.weights=NULL, v.weights=NULL, modularity=TRUE)
		else if(name==paste0(MEAS_MODULARITY,SFX_WEIGHT))
			res <- cluster_infomap(graph=graph, e.weights=E(graph)$weight, v.weights=NULL, modularity=TRUE)
		cache[[name]] <<- res
	}
}



# basic variants
GRAPH_MEASURES[[MEAS_MODULARITY]] <- list( #modularity
	type=numeric(),
	bounds=c(0,1),
	cname="Modularity",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(MEAS_MODULARITY, graph)
		modularity(com)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_NBR)]] <- list( #community-number
	type=integer(),
	bounds=c(0,NA),
	cname="Community Number",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(MEAS_MODULARITY, graph)
		length(communities(com))
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_SIZE,SFX_AVG)]] <- list( #community-size-average
	type=integer(),
	bounds=c(0,NA),
	cname="Average Community Size",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(MEAS_MODULARITY, graph)
		sizes <- sapply(communities(com),length)
		mean(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_SIZE,SFX_STDEV)]] <- list( #community-size-stdev
	type=integer(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Community Sizes",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(MEAS_MODULARITY, graph)
		sizes <- sapply(communities(com),length)
		sd(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_SIZE,SFX_MIN)]] <- list( #community-size-min
	type=integer(),
	bounds=c(0,NA),
	cname="Minimal Community Size",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(MEAS_MODULARITY, graph)
		sizes <- sapply(communities(com),length)
		min(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_SIZE,SFX_MAX)]] <- list( #community-size-max
	type=integer(),
	bounds=c(0,NA),
	cname="Maximal Community Size",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(MEAS_MODULARITY, graph)
		sizes <- sapply(communities(com),length)
		max(sizes)
	}
)



# weighted variants
GRAPH_MEASURES[[paste0(MEAS_MODULARITY,SFX_WEIGHT)]] <- list( #modularity-weighted
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Modularity",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(paste0(MEAS_MODULARITY,SFX_WEIGHT), graph)
		modularity(com)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_NBR)]] <- list( #community-weighted-number
	type=integer(),
	bounds=c(0,NA),
	cname="Weighted Community Number",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(paste0(MEAS_MODULARITY,SFX_WEIGHT), graph)
		length(communities(com))
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_SIZE,SFX_AVG)]] <- list( #community-weighted-size-average
	type=integer(),
	bounds=c(0,NA),
	cname="Average Weighted Community Size",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(paste0(MEAS_MODULARITY,SFX_WEIGHT), graph)
		sizes <- sapply(communities(com),length)
		mean(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_SIZE,SFX_STDEV)]] <- list( #community-weighted-size-stdev
	type=integer(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Weighted Community Sizes",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(paste0(MEAS_MODULARITY,SFX_WEIGHT), graph)
		sizes <- sapply(communities(com),length)
		sd(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_SIZE,SFX_MIN)]] <- list( #community-weighted-size-min
	type=integer(),
	bounds=c(0,NA),
	cname="Minimal Weighted Community Size",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(paste0(MEAS_MODULARITY,SFX_WEIGHT), graph)
		sizes <- sapply(communities(com),length)
		min(sizes)
	}
)
GRAPH_MEASURES[[paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_SIZE,SFX_MAX)]] <- list( #community-weighted-size-max
	type=integer(),
	bounds=c(0,NA),
	cname="Maximal Weighted Community Size",
	folder=MEAS_COMMUNITY,
	object="graph",
	foo=function(graph)
	{	com <- compute.communities(paste0(MEAS_MODULARITY,SFX_WEIGHT), graph)
		sizes <- sapply(communities(com),length)
		max(sizes)
	}
)
