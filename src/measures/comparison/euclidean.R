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
compute.euclidean.distance <- function(name, graph, filtered)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	# durations
		if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_DUR) || name==paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, filtered)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_DUR) || name==paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, filtered)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_DUR) || name==paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, filtered)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		# occurrences
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_OCC) || name==paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, filtered)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_OCC) || name==paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, filtered)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_OCC) || name==paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, filtered)
			res <- graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		cache[[name]] <<- res
	}
}



# graph
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_DUR)]] <- list( #Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Scene-Based Duration Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	weighted=FALSE,
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_DUR), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Scene-Based Duration Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_DUR), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized Euclidean Distance with Scene-Based Duration Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_OCC)]] <- list( #Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Scene-Based Occurrences Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	weighted=FALSE,
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_OCC), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Scene-Based Occurrences Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_OCC), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized Euclidean Distance with Scene-Based Occurrences Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, filtered=FALSE)
		return(values)
	}
)



# graph filtered
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_DUR)]] <- list( #Euclidean distance with filtered duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Filtered Scene-Based Duration Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_DUR), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted Euclidean distance with filtered duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Filtered Scene-Based Duration Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_DUR), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized Euclidean distance with filtered duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized Euclidean Distance with Filtered Scene-Based Duration Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_OCC)]] <- list( #Euclidean distance with filtered occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Filtered Scene-Based Occurrences Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_OCC), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted Euclidean distance with filtered occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Filtered Scene-Based Occurrences Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_OCC), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized Euclidean distance with filtered occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized Euclidean Distance with Filtered Scene-Based Occurrences Graph",
	folder=MEAS_EUCLIDEAN_DIST,
	object="graph",
	foo=function(graph)
	{	values <- compute.euclidean.distance(paste0(MEAS_EUCLIDEAN_DIST, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, filtered=TRUE)
		return(values)
	}
)
