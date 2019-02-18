# This script contains methods computing 
# betweenness-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# basic variants
NODE_MEASURES[[MEAS_BETWEENNESS]] <- list( #betweenness
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_AVG)]] <- list( #betweenness-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_STDEV)]] <- list( #betweenness-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_MIN)]] <- list( #betweenness-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_MAX)]] <- list( #betweenness-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_ASSORT)]] <- list( #betweenness-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=FALSE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_CENTRZ)]] <- list( #betweenness-centralization
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centr_betw(graph=graph, directed=FALSE, normalized=FALSE)$centralization
	}
)



# normalized variants
NODE_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM)]] <- list( #betweenness-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_AVG)]] <- list( #betweenness-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_STDEV)]] <- list( #betweenness-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_MIN)]] <- list( #betweenness-norm-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_MAX)]] <- list( #betweenness-norm-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_ASSORT)]] <- list( #betweenness-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=NULL, normalized=TRUE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_NORM,SFX_CENTRZ)]] <- list( #betweenness-norm-centralization
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	centr_betw(graph=graph, directed=FALSE, normalized=TRUE)$centralization
	}
)



# weighted variants
NODE_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT)]] <- list( #betweenness-weighted
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_AVG)]] <- list( #betweenness-weighted-average
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_STDEV)]] <- list( #betweenness-weighted-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_MIN)]] <- list( #betweenness-weighted-min
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_MAX)]] <- list( #betweenness-weighted-max
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_ASSORT)]] <- list( #betweenness-weighted-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=FALSE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)



# normalized weighted variants
NODE_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM)]] <- list( #betweenness-weighted-norm
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_AVG)]] <- list( #betweenness-weighted-norm-average
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_STDEV)]] <- list( #betweenness-weighted-norm-stdev
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_MIN)]] <- list( #betweenness-weighted-norm-min
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_MAX)]] <- list( #betweenness-weighted-norm-max
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		max(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_NORM,SFX_ASSORT)]] <- list( #betweenness-weighted-norm-assortativity
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph)
	{	values <- betweenness(graph=graph, directed=FALSE, weights=E(graph)$weight, normalized=TRUE)
		assortativity(graph=graph, types1=values, types2=NULL, directed=FALSE)
	}
)
