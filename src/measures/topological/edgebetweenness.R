# This script contains methods computing 
# edgebetweeness-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.edgebetweenness <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_EDGEBETWNS)
			res <- edge_betweenness(graph=graph, directed=FALSE, weights=NA)
		else if(name==paste0(MEAS_EDGEBETWNS,SFX_WEIGHT))
			res <- edge_betweenness(graph=graph, directed=FALSE, weights=reverse.weights(E(graph)$weight))
		cache[[name]] <<- res
	}
}

# basic variants
LINK_MEASURES[[MEAS_EDGEBETWNS]] <- list( #edgebetweenness
	type=numeric(),
	bounds=c(0,NA),
	cname="Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="links",
	weighted=FALSE,
	foo=function(graph) 
	{	compute.edgebetweenness(MEAS_EDGEBETWNS, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_AVG)]] <- list( #edgebetweenness-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.edgebetweenness(MEAS_EDGEBETWNS, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_STDEV)]] <- list( #edgebetweenness-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.edgebetweenness(MEAS_EDGEBETWNS, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_MIN)]] <- list( #edgebetweenness-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.edgebetweenness(MEAS_EDGEBETWNS, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_MAX)]] <- list( #edgebetweenness-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.edgebetweenness(MEAS_EDGEBETWNS, graph)
		max(values,na.rm=TRUE)
	}
)



# weighted variants
LINK_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT)]] <- list( #edgebetweenness-weighted
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="links",
	weighted=TRUE,
	foo=function(graph) 
	{	compute.edgebetweenness(paste0(MEAS_EDGEBETWNS,SFX_WEIGHT), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT,SFX_AVG)]] <- list( #edgebetweenness-weighted-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Weighted Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="graph",
	weighted=TRUE,
	foo=function(graph) 
	{	values <- compute.edgebetweenness(paste0(MEAS_EDGEBETWNS,SFX_WEIGHT), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT,SFX_STDEV)]] <- list( #edgebetweenness-weighted-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Weighted Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="graph",
	weighted=TRUE,
	foo=function(graph) 
	{	values <- compute.edgebetweenness(paste0(MEAS_EDGEBETWNS,SFX_WEIGHT), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT,SFX_MIN)]] <- list( #edgebetweenness-weighted-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Minimal Weighted Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="graph",
	weighted=TRUE,
	foo=function(graph) 
	{	values <- compute.edgebetweenness(paste0(MEAS_EDGEBETWNS,SFX_WEIGHT), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_EDGEBETWNS,SFX_WEIGHT,SFX_MAX)]] <- list( #edgebetweenness-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Weighted Edge-Betweenness",
	folder=MEAS_EDGEBETWNS,
	object="graph",
	weighted=TRUE,
	foo=function(graph) 
	{	values <- compute.edgebetweenness(paste0(MEAS_EDGEBETWNS,SFX_WEIGHT), graph)
		max(values,na.rm=TRUE)
	}
)
