# This script contains methods computing 
# measures allowing to compare networks in various ways.
# 
# Vincent Labatut
# 05/2021
###############################################################################
# false negatives
graph.falsenegative.measure <- function(graph1, graph2, weighted, normalized)
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
	# false negatives for the first relative to the second
	res <- x2 - apply(cbind(x1,x2), 1, min)
	
	return(res)
}
# cache function
compute.falsenegative <- function(name, graph, reduced)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	# durations
		if(name==paste0(MEAS_FALSENEG, SFX_DUR) || name==paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, reduced)
			res <- graph.falsenegative.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_DUR) || name==paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, reduced)
			res <- graph.falsenegative.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_DUR) || name==paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR))
		{	ref <- get.ref.graph(SFX_DUR, reduced)
			res <- graph.falsenegative.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		# occurrences
		else if(name==paste0(MEAS_FALSENEG, SFX_OCC) || name==paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, reduced)
			res <- graph.falsenegative.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_OCC) || name==paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, reduced)
			res <- graph.falsenegative.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
		}
		else if(name==paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_OCC) || name==paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC))
		{	ref <- get.ref.graph(SFX_OCC, reduced)
			res <- graph.falsenegative.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
		}
		cache[[name]] <<- res
	}
}



# nodal
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_DUR)]] <- list( #false negative compared to duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="False Negatives relative to Scene-Based Duration Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_DUR), graph, reduced=FALSE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted false negative compared to duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted False Negatives relative to Scene-Based Duration Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_DUR), graph, reduced=FALSE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized false negative compared to duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized False Negatives relative to Scene-Based Duration Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, reduced=FALSE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_OCC)]] <- list( #false negative compared to occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="False Negatives relative to Scene-Based Occurrences Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_OCC), graph, reduced=FALSE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted false negative compared to occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted False Negatives relative to Scene-Based Occurrences Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_OCC), graph, reduced=FALSE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized false negative compared to occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized False Negatives relative to Scene-Based Occurrences Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, reduced=FALSE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)



# nodal reduced
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_DUR)]] <- list( #false negative compared to reduced duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="False Negatives relative to Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_DUR), graph, reduced=TRUE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted false negative compared to reduced duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted False Negatives relative to Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_DUR), graph, reduced=TRUE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized false negative compared to reduced duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized False Negatives relative to Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, reduced=TRUE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_OCC)]] <- list( #false negative compared to reduced occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="False Negatives relative to Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_OCC), graph, reduced=TRUE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted false negative compared to reduced occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted False Negatives relative to Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_OCC), graph, reduced=TRUE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)
NODECOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized false negative compared to reduced occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized False Negatives relative to Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	tmp <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, reduced=TRUE)
		m <- matrix(0, nrow=gorder(graph), ncol=gorder(graph))
		m[upper.tri(m,diag=F)] <- tmp
		m <- m + t(m)
		values <- rowSums(m)
		return(values)
	}
)



# total
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_DUR)]] <- list( #false negative compared to duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="False Negatives relative to Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_DUR), graph, reduced=FALSE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted false negative compared to duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted False Negatives relative to Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_DUR), graph, reduced=FALSE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized false negative compared to duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized False Negatives relative to Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, reduced=FALSE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_OCC)]] <- list( #false negative compared to occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="False Negatives relative to Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_OCC), graph, reduced=FALSE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted false negative compared to occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted False Negatives relative to Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_OCC), graph, reduced=FALSE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized false negative compared to occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized False Negatives relative to Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, reduced=FALSE)
		sum(values)
	}
)



# total reduced
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_TOTAL, SFX_DUR)]] <- list( #false negative compared to reduced duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="False Negatives relative to Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_DUR), graph, reduced=TRUE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_TOTAL, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted false negative compared to reduced duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted False Negatives relative to Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_DUR), graph, reduced=TRUE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized false negative compared to reduced duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized False Negatives relative to Reduced Scene-Based Duration Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_DUR), graph, reduced=TRUE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_TOTAL, SFX_OCC)]] <- list( #false negative compared to reduced occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="False Negatives relative to Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_OCC), graph, reduced=TRUE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_TOTAL, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted false negative compared to reduced occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted False Negatives relative to Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_OCC), graph, reduced=TRUE)
		sum(values)
	}
)
GRAPHCOMP_MEASURES[[paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized false negative compared to reduced occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized False Negatives relative to Reduced Scene-Based Occurrences Graph",
	foo=function(graph)
	{	values <- compute.falsenegative(paste0(MEAS_FALSENEG, SFX_REDUCED, SFX_WEIGHT, SFX_NORM, SFX_OCC), graph, reduced=TRUE)
		sum(values)
	}
)
