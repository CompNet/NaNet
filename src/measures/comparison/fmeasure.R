# This script contains methods computing 
# measures allowing to compare networks in various ways.
# 
# Vincent Labatut
# 05/2021
###############################################################################
# F-measure
graph.f.measure <- function(graph1, graph2, weighted, normalized)
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
	# fmeasure of the first relative to the second
	pre <- graph.precision.measure(graph1, graph2, weighted, normalized)
	rec <- graph.recall.measure(graph1, graph2, weighted, normalized)
	res <- 2*pre*rec / (pre+rec)
	
	return(res)
}
# cache function
compute.f.measure <- function(name, graph, reduced)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	# durations
		if(name==paste0(MEAS_FMEASURE, SFX_DUR) || name==paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, reduced)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_DUR) || name==paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, reduced)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_DUR) || name==paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, reduced)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		# occurrences
		else if(name==paste0(MEAS_FMEASURE, SFX_OCC) || name==paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, reduced)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_OCC) || name==paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, reduced)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_OCC) || name==paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, reduced)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		cache[[name]] <<- res
	}
}



# graph
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_DUR)]] <- list( #F-measure with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="F-measure with Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_DUR), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted F-measure with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_DUR), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized F-measure with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_OCC)]] <- list( #F-measure with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="F-measure with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_OCC), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted F-measure with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_OCC), graph, reduced=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized F-measure with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, reduced=FALSE)
		return(values)
	}
)



# graph reduced
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_DUR)]] <- list( #F-measure with reduced duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="F-measure with Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_DUR), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted F-measure with reduced duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_DUR), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized F-measure with reduced duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_OCC)]] <- list( #F-measure with reduced occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="F-measure with Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_OCC), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted F-measure with reduced occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_OCC), graph, reduced=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized F-measure with reduced occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, reduced=TRUE)
		return(values)
	}
)
