# This script contains methods computing 
# degree-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.degree <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_DEGREE)
			res <- degree(graph=graph, mode="all", normalized=FALSE)
		else if(name==paste0(MEAS_DEGREE,SFX_NORM))
			res <- degree(graph=graph, mode="all", normalized=TRUE)
		else if(name==MEAS_STRENGTH)
			res <- strength(graph=graph, mode="all", weights=E(graph)$weight)
		cache[[name]] <<- res
	}
}



# degree variants
NODE_MEASURES[[paste0(MEAS_DEGREE)]] <- list( #degree
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	compute.degree(MEAS_DEGREE, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_AVG)]] <- list( #degree-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.degree(MEAS_DEGREE, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_STDEV)]] <- list( #degree-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.degree(MEAS_DEGREE, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_MIN)]] <- list( #degree-min
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.degree(MEAS_DEGREE, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_MAX)]] <- list( #degree-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.degree(MEAS_DEGREE, graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_ASSORT)]] <- list( #degree-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	assortativity_degree(graph=graph, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_CENTRZ)]] <- list( #degree-centralization
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_degree(graph=graph, mode="all", normalized=FALSE)$centralization
	}
)



# normalized variants
NODE_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM)]] <- list( #degree-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	compute.degree(paste0(MEAS_DEGREE,SFX_NORM), graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_AVG)]] <- list( #degree-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.degree(paste0(MEAS_DEGREE,SFX_NORM), graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_STDEV)]] <- list( #degree-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.degree(paste0(MEAS_DEGREE,SFX_NORM), graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_MIN)]] <- list( #degree-norm-min
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.degree(paste0(MEAS_DEGREE,SFX_NORM), graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_MAX)]] <- list( #degree-norm-max
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- compute.degree(paste0(MEAS_DEGREE,SFX_NORM), graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_ASSORT)]] <- list( #degree-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	assortativity_degree(graph=graph, directed=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_DEGREE,SFX_NORM,SFX_CENTRZ)]] <- list( #degree-norm-centralization
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centr_degree(graph=graph, mode="all", normalized=TRUE)$centralization
	}
)



# strength variants
NODE_MEASURES[[MEAS_STRENGTH]] <- list( #strength
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	compute.degree(MEAS_STRENGTH, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_AVG)]] <- list( #strength-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.degree(MEAS_STRENGTH, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_STDEV)]] <- list( #strength-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.degree(MEAS_STRENGTH, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_MIN)]] <- list( #strength-min
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.degree(MEAS_STRENGTH, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_MAX)]] <- list( #strength-max
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- compute.degree(MEAS_STRENGTH, graph)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_STRENGTH,SFX_ASSORT)]] <- list( #strength-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- compute.degree(MEAS_STRENGTH, graph)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
