# This script contains methods computing 
# transitivity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
NODE_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL)]] <- list( #transitivity-local
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL,SFX_AVG)]] <- list( #transitivity-local-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL,SFX_STDEV)]] <- list( #transitivity-local-stdev
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL,SFX_MIN)]] <- list( #transitivity-local-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_LOCAL,SFX_MAX)]] <- list( #transitivity-local-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_ASSORT)]] <- list( #transitivity-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="localundirected", weights=NULL, isolates="zero")
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
	{	transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL,SFX_AVG)]] <- list( #transitivity-weighted-local-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL,SFX_STDEV)]] <- list( #transitivity-weighted-local-stdev
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL,SFX_MIN)]] <- list( #transitivity-weighted-local-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL,SFX_MAX)]] <- list( #transitivity-weighted-local-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_ASSORT)]] <- list( #transitivity-weighted-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	values <- transitivity(graph=graph, type="weighted", weights=E(graph)$weight, isolates="zero")
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
