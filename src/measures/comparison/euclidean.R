# This script contains methods computing 
# measures allowing to compare networks in various ways.
# 
# Vincent Labatut
# 05/2021
###############################################################################
# Euclidean distance
graph.euclidean.distance <- function(graph1, graph2, weighted, normalized)
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
	# normalization
	if(normalized)
	{	x1 <- x1 / max(x1)
		x2 <- x2 / max(x2)
	}
	# Euclidean distance between these vectors
	res <- sqrt(sum((x1-x2)^2))
	
	return(res)
}
# cache function
compute.euclidean.distance <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	# durations
		if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		# occurrences
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		cache[[name]] <<- res
	}
}



GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_DUR)]] <- list( #Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_DUR), graph)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_DUR), graph)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized Euclidean Distance with Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_OCC)]] <- list( #Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_OCC), graph)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_OCC), graph)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized Euclidean Distance with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph)
		return(values)
	}
)
