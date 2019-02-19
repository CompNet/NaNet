# This script contains methods computing 
# transitivity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.transitivity <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==paste0(MEAS_TRANSITIVITY,SFX_LOCAL))
			res <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		else if(name==paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL))
			res <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		cache[[name]] <<- res
	}
}



# basic variants
NODE_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL)]] <- list( #transitivity-local
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_LOCAL), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL,SFX_AVG)]] <- list( #transitivity-local-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_LOCAL), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL,SFX_STDEV)]] <- list( #transitivity-local-stdev
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_LOCAL), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL,SFX_MIN)]] <- list( #transitivity-local-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_LOCAL), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL,SFX_MAX)]] <- list( #transitivity-local-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_LOCAL), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_ASSORT)]] <- list( #transitivity-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_LOCAL), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_GLOBAL)]] <- list( #transitivity-global
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	transitivity(graph=graph, type="globalundirected", weights=NULL, isolates="zero")
	}
)



# weighted variants
NODE_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL)]] <- list( #transitivity-weighted-local
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL,SFX_AVG)]] <- list( #transitivity-weighted-local-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL,SFX_STDEV)]] <- list( #transitivity-weighted-local-stdev
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL,SFX_MIN)]] <- list( #transitivity-weighted-local-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL,SFX_MAX)]] <- list( #transitivity-weighted-local-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_ASSORT)]] <- list( #transitivity-weighted-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- compute.transitivity(paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL), graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
