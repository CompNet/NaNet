# This script contains methods computing 
# measures allowing to compare networks in various ways.
# 
# Vincent Labatut
# 05/2021
###############################################################################
# recall measure
graph.recall.measure <- function(graph1, graph2, weighted, normalized)
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
	# recall of the first relative to the second
	res <- sum(apply(cbind(x1,x2), 1, min)) / sum(x2)
	
	return(res)
}
# cache function
compute.recall.measure <- function(name, graph, reduced)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	# durations
		if(name==paste0(MEAS_RECALL, SFX_DUR) || name==paste0(MEAS_RECALL, SFX_REDUCED, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, reduced)
			res <- graph.recall.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_RECALL, SFX_WEIGHT, SFX_DUR) || name==paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, reduced)
			res <- graph.recall.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_RECALL, SFX_WEIGHT, SFX_NORM, SFX_DUR) || name==paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, reduced)
			res <- graph.recall.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		# occurrences
		else if(name==paste0(MEAS_RECALL, SFX_OCC) || name==paste0(MEAS_RECALL, SFX_REDUCED, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, reduced)
			res <- graph.recall.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_RECALL, SFX_WEIGHT, SFX_OCC) || name==paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, reduced)
			res <- graph.recall.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_RECALL, SFX_WEIGHT, SFX_NORM, SFX_OCC) || name==paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, reduced)
			res <- graph.recall.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		cache[[name]] <<- res
	}
}



# graph
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_DUR)]] <- list( #recall with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Recall with Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_DUR), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted recall with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Recall with Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_WEIGHT, SFX_DUR), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized recall with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Recall with Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_OCC)]] <- list( #recall with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Recall with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_OCC), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted recall with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Recall with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_WEIGHT, SFX_OCC), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized recall with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Recall with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, reduced=FALSE)
		return(values)
	}
)



# graph reduced
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_REDUCED, SFX_DUR)]] <- list( #recall with reduced duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Recall with Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_REDUCED, SFX_DUR), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted recall with reduced duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Recall with Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_DUR), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized recall with reduced duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Recall with Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_REDUCED, SFX_OCC)]] <- list( #recall with reduced occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Recall with Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_REDUCED, SFX_OCC), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted recall with reduced occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Recall with Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_OCC), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized recall with reduced occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Recall with Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.recall.measure(paste0(MEAS_RECALL, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, reduced=TRUE)
		return(values)
	}
)
