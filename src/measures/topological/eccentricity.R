# This script contains methods computing 
# eccentricity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.eccentricity <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_ECCENTRICITY)
			res <- eccentricity(graph=graph, mode="all")
		cache[[name]] <<- res
	}
}



# basic variants
NODE_MEASURES[[paste0(MEAS_ECCENTRICITY)]] <- list( #eccentricity
	type=numeric(),
	bounds=c(0,NA),
	cname="Eccentricity",
	folder=MEAS_ECCENTRICITY,
	object="nodes",
	weighted=FALSE,
	foo=function(graph) 
	{	compute.eccentricity(MEAS_ECCENTRICITY, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_AVG)]] <- list( #eccentricity-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Eccentricity",
	folder=MEAS_ECCENTRICITY,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.eccentricity(MEAS_ECCENTRICITY, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_STDEV)]] <- list( #eccentricity-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Eccentricity",
	folder=MEAS_ECCENTRICITY,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.eccentricity(MEAS_ECCENTRICITY, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_MIN)]] <- list( #eccentricity-min
	# aka radius
	type=integer(),
	bounds=c(0,NA),
	cname="Minimal Eccentricity (or Radius)",
	folder=MEAS_ECCENTRICITY,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.eccentricity(MEAS_ECCENTRICITY, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_MAX)]] <- list( #eccentricity-max
	# aka diameter, or maximal distance
	type=integer(),
	bounds=c(0,NA),
	cname="Maximal Eccentricity (or Diameter)",
	folder=MEAS_ECCENTRICITY,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.eccentricity(MEAS_ECCENTRICITY, graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ECCENTRICITY,SFX_ASSORT)]] <- list( #eccentricity-assortativity
	type=numeric(),
	bounds=c(-1,1),
	cname="Eccentricity Assortativity",
	folder=MEAS_ECCENTRICITY,
	object="graph",
	weighted=FALSE,
	foo=function(graph) 
	{	values <- compute.eccentricity(MEAS_ECCENTRICITY, graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
