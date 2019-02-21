# This script contains methods computing 
# connectivity-related measures.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
compute.connectivity <- function(name, graph)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	if(name==MEAS_LK_CONNECT)
			foo <- edge_connectivity
		else if(name==MEAS_ND_CONNECT)
			foo <- vertex_connectivity
		res <- c()
		for(i in 1:(gorder(graph)-1))
		{	for(j in (i+1):gorder(graph))
			{	if(name==MEAS_ND_CONNECT && are.connected(graph,i,j))
					value <- NA
				else
					value <- foo(graph=graph, source=i, target=j)
				res <- c(res, value)
			}
if(i %% 50 == 0)
	cat(".")
		}
		cache[[name]] <<- res
	}
}



# link connectivity
NODEPAIR_MEASURES[[MEAS_LK_CONNECT]] <- list( #link-connectivity
	type=numeric(),
	bounds=c(0,NA),
	cname="Link-Connectivity",
	foo=function(graph) 
	{	compute.connectivity(MEAS_LK_CONNECT, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_AVG)]] <- list( #link-connectivity-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Link-Connectivity",
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_LK_CONNECT, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_STDEV)]] <- list( #link-connectivity-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Link-Connectivity",
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_LK_CONNECT, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_MIN)]] <- list( #link-connectivity-min
	# aka adhesion
	type=integer(),
	bounds=c(0,NA),
	cname="Minimal Link-Connectivity (or Adhesion)",
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_LK_CONNECT, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_LK_CONNECT,SFX_MAX)]] <- list( #link-connectivity-max
	type=integer(),
	bounds=c(0,NA),
	cname="Maximal Link-Connectivity",
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_LK_CONNECT, graph)
		max(values,na.rm=TRUE)
	}
)



# node connectivity
NODEPAIR_MEASURES[[MEAS_ND_CONNECT]] <- list( #node-connectivity
	type=numeric(),
	bounds=c(0,NA),
	cname="Node-Connectivity",
	foo=function(graph) 
	{	compute.connectivity(MEAS_ND_CONNECT, graph)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_AVG)]] <- list( #node-connectivity-average
	type=numeric(),
	bounds=c(0,NA),
	cname="Average Node-Connectivity",
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_ND_CONNECT, graph)
		mean(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_STDEV)]] <- list( #node-connectivity-stdev
	type=numeric(),
	bounds=c(0,NA),
	cname="Standard Deviation of the Node-Connectivity",
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_ND_CONNECT, graph)
		sd(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_MIN)]] <- list( #node-connectivity-min
	# aka cohesion
	type=integer(),
	bounds=c(0,NA),
	cname="Minimal Node-Connectivity (or Cohesion)",
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_ND_CONNECT, graph)
		min(values,na.rm=TRUE)
	}
)
GRAPH_MEASURES[[paste0(MEAS_ND_CONNECT,SFX_MAX)]] <- list( #node-connectivity-max
	type=integer(),
	bounds=c(0,NA),
	cname="Maximal Node-Connectivity",
	foo=function(graph) 
	{	values <- compute.connectivity(MEAS_ND_CONNECT, graph)
		max(values,na.rm=TRUE)
	}
)
