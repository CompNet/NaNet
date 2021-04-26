# This script contains methods computing 
# measures allowing to compare networks in various ways.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
get.ref.graph <- function(name)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	graph.file <- get.graphname.static(mode="scenes")
		res <- read.graph(file=graph.file, format="graphml")
		if(name==SFX_DUR)
			E(res)$weight <- E(res)$Duration
		else if(name==SFX_OCC)
			E(res)$weight <- E(res)$Occurrences
		cache[[name]] <<- res
	}
}



# Euclidean distances
graph.euclidean.distance <- function(graph1, graph2, weighted)
{	if(weighted)
		attr <- "weight"
	else
		attr <- NULL
	
	# vector representing the first graph
	x1 <- as_adjacency_matrix(graph=graph1, type="upper", attr=attr, names=FALSE)
	x1 <- x1[upper.tri(x1,diag=F)]
	# vector representing the second graph
	x2 <- as_adjacency_matrix(graph=graph2, type="upper", attr=attr, names=FALSE)
	x2 <- x2[upper.tri(x2,diag=F)]
	# Euclidean distance between these vectors
	res <- sqrt(sum((x1-x2)^2))
	
	return(res)
}
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN, SFX_DUR)]] <- list( #Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE)
	}
)
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN, SFX_OCC)]] <- list( #Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE)
	}
)
