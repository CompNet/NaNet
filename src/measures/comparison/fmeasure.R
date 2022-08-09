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
compute.f.measure <- function(name, graph, filtered)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	# durations
		if(name==paste0(MEAS_FMEASURE, SFX_DUR) || name==paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, filtered)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_DUR) || name==paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, filtered)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_DUR) || name==paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, filtered)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		# occurrences
		else if(name==paste0(MEAS_FMEASURE, SFX_OCC) || name==paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, filtered)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_OCC) || name==paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, filtered)
			res <- graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_OCC) || name==paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, filtered)
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
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=FALSE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_DUR), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted F-measure with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Scene-Based Duration Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_DUR), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized F-measure with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Scene-Based Duration Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_OCC)]] <- list( #F-measure with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="F-measure with Scene-Based Occurrences Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=FALSE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_OCC), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted F-measure with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Scene-Based Occurrences Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_OCC), graph, filtered=FALSE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized F-measure with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Scene-Based Occurrences Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, filtered=FALSE)
		return(values)
	}
)



# graph filtered
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_DUR)]] <- list( #F-measure with filtered duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="F-measure with Filtered Scene-Based Duration Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=FALSE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_DUR), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted F-measure with filtered duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Filtered Scene-Based Duration Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_DUR), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized F-measure with filtered duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Filtered Scene-Based Duration Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_OCC)]] <- list( #F-measure with filtered occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="F-measure with Filtered Scene-Based Occurrences Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=FALSE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_OCC), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted F-measure with filtered occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Filtered Scene-Based Occurrences Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_OCC), graph, filtered=TRUE)
		return(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized F-measure with filtered occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Filtered Scene-Based Occurrences Graph",
	folder=MEAS_FMEASURE,
	object="graphcomp",
	weighted=TRUE,
	foo=function(graph)
	{	values <- compute.f.measure(paste0(MEAS_FMEASURE, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, filtered=TRUE)
		return(values)
	}
)
