# This script contains methods computing 
# distance-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.distance <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_DISTANCE)
			res <- distances(graph=graph, mode="all", weights=NA)
		else if(name==paste0(MEAS_DISTANCE,SFX_WEIGHT))
			res <- distances(graph=graph, mode="all", weights=reverse.weights(E(graph)$weight))
		res <- res[upper.tri(res,diag=FALSE)]
		cache[[name]] <<- res
	}
}



# unweighted variants
NODEPAIR_MEASURES[[MEAS_DISTANCE]] <- list( #distance
	type=numeric(),
	bounds=c(0,NA),
	cname="Distance",
	folder=MEAS_DISTANCE,
	object="nodepairs",
	weighted=FALSE,
	foo=function(graph) 
	{	compute.distance(MEAS_DISTANCE, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_AVG)]] <- list( #distance-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Distance",
	folder=MEAS_DISTANCE,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.distance(MEAS_DISTANCE, graph)
		values <- values[!is.infinite(values)]
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_STDEV)]] <- list( #distance-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Distance",
	folder=MEAS_DISTANCE,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.distance(MEAS_DISTANCE, graph)
		values <- values[!is.infinite(values)]
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_MIN)]] <- list( #distance-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Minimal Distance",
	folder=MEAS_DISTANCE,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.distance(MEAS_DISTANCE, graph)
		values <- values[!is.infinite(values)]
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_MAX)]] <- list( #distance-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Distance (or Diameter)",
	folder=MEAS_DISTANCE,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.distance(MEAS_DISTANCE, graph)
		values <- values[!is.infinite(values)]
		max(values,na.rm=TRUE)
	}
)
#GRAPH_MEASURES[["girth"]] <- list(		# cycle of minimal length
#	type=integer(),
#	bounds=c(1,NA),
#	folder=MEAS_DISTANCE,
#	object="graph",
#	foo=function(graph) 
#	{	girth(graph=graph, circle=FALSE)$girth
#	}
#)



# weighted variants
NODEPAIR_MEASURES[[paste0(MEAS_DISTANCE,SFX_WEIGHT)]] <- list( #distance-weighted
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Distance",
	folder=MEAS_DISTANCE,
	object="nodepairs",
	weighted=TRUE,
	foo=function(graph) 
	{	compute.distance(paste0(MEAS_DISTANCE,SFX_WEIGHT), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_WEIGHT,SFX_AVG)]] <- list( #distance-weighted-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Weighted Distance",
	folder=MEAS_DISTANCE,
	object="graph",
	weighted=TRUE,
	foo=function(graph) 
	{	values <- compute.distance(paste0(MEAS_DISTANCE,SFX_WEIGHT), graph)
		values <- values[!is.infinite(values)]
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_WEIGHT,SFX_STDEV)]] <- list( #distance-weighted-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Weighted Distance",
	folder=MEAS_DISTANCE,
	object="graph",
	weighted=TRUE,
	foo=function(graph) 
	{	values <- compute.distance(paste0(MEAS_DISTANCE,SFX_WEIGHT), graph)
		values <- values[!is.infinite(values)]
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_WEIGHT,SFX_MIN)]] <- list( #distance-weighted-min
	type=numeric(),
	bounds=c(0,NA),
	cname="Minimal Weighted Distance",
	folder=MEAS_DISTANCE,
	object="graph",
	weighted=TRUE,
	foo=function(graph) 
	{	values <- compute.distance(paste0(MEAS_DISTANCE,SFX_WEIGHT), graph)
		values <- values[!is.infinite(values)]
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DISTANCE,SFX_WEIGHT,SFX_MAX)]] <- list( #distance-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	cname="Maximal Weighted Distance (or Weighted Diameter)",
	folder=MEAS_DISTANCE,
	object="graph",
	weighted=TRUE,
	foo=function(graph) 
	{	values <- compute.distance(paste0(MEAS_DISTANCE,SFX_WEIGHT), graph)
		values <- values[!is.infinite(values)]
		max(values,na.rm=TRUE)
	}
)
