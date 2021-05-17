# This script contains methods computing 
# measures allowing to compare networks in various ways.
# 
# Vincent Labatut
# 02/2019
###############################################################################
# cache function
get.ref.graph <- function(name)
{	if(length(cache[[name]])>0)
		res <- cache[[name]]
	else
	{	graph.file <- get.graphname.static(mode="scenes")
		res <- read.graph(file=graph.file, format="graphml")
		if(name==SFX_DUR)
			E(res)$weight <- E(res)$Duration
		else if(name==SFX_OCC)
			E(res)$weight <- E(res)$Occurrences
		cache[[name]] <<- res
	}
}



# Euclidean distance
graph.euclidean.distance <- function(graph1, graph2, weighted, normalized)
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
	# Euclidean distance between these vectors
	res <- sqrt(sum((x1-x2)^2))
	
	return(res)
}
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_DUR)]] <- list( #Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized Euclidean distance with duration graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized Euclidean Distance with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_OCC)]] <- list( #Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Euclidean Distance with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted Euclidean Distance with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_EUCLIDEAN_DIST, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized Euclidean distance with occurrences graph
	type=numeric(),
	bounds=c(0,NA),
	cname="Weighted normalized Euclidean Distance with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.euclidean.distance(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)



# cosine similarity
graph.cosine.similarity <- function(graph1, graph2, weighted, normalized)
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
	# cosine similarity between these vectors
	res <- sum(x1*x2) / (sqrt(sum(x1^2)) * sqrt(sum(x2^2)))
	
	return(res)
}
COMP_MEASURES[[paste0(MEAS_COSINE_SIM, SFX_DUR)]] <- list( #Cosine similarity with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Cosine Similarity with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.cosine.similarity(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_COSINE_SIM, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted Cosine similarity with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Cosine Similarity with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.cosine.similarity(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_COSINE_SIM, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized Cosine similarity with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Cosine Similarity with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.cosine.similarity(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)
COMP_MEASURES[[paste0(MEAS_COSINE_SIM, SFX_OCC)]] <- list( #Cosine similarity with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Cosine Similarity with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.cosine.similarity(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_COSINE_SIM, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted Cosine similarity with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Cosine Similarity with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.cosine.similarity(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_COSINE_SIM, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized Cosine similarity with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Cosine Similarity with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.cosine.similarity(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)



# Jaccard's coefficient
graph.jaccard.similarity <- function(graph1, graph2, weighted, normalized)
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
	# jaccard's similarity between these vectors
	res <- sum(apply(cbind(x1,x2), 1, min)) / sum(apply(cbind(x1,x2), 1, max))
	
	return(res)
}
COMP_MEASURES[[paste0(MEAS_JACCARD_SIM, SFX_DUR)]] <- list( #Jaccard's similarity with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Jaccard Similarity with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.jaccard.similarity(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_JACCARD_SIM, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted Jaccard's similarity with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Jaccard Similarity with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.jaccard.similarity(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_JACCARD_SIM, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized Jaccard's similarity with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Jaccard Similarity with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.jaccard.similarity(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)
COMP_MEASURES[[paste0(MEAS_JACCARD_SIM, SFX_OCC)]] <- list( #Jaccard's similarity with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Jaccard Similarity with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.jaccard.similarity(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_JACCARD_SIM, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted Jaccard's similarity with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Jaccard Similarity with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.jaccard.similarity(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_JACCARD_SIM, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized Jaccard's similarity with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Jaccard Similarity with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.jaccard.similarity(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)



# precision measure
graph.precision.measure <- function(graph1, graph2, weighted, normalized)
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
	# precision of the first relative to the second
	res <- sum(apply(cbind(x1,x2), 1, min)) / sum(x1)
	
	return(res)
}
COMP_MEASURES[[paste0(MEAS_PRECISION, SFX_DUR)]] <- list( #precision with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Precision with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.precision.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_PRECISION, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted precision with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Precision with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.precision.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_PRECISION, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized precision with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Precision with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.precision.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)
COMP_MEASURES[[paste0(MEAS_PRECISION, SFX_OCC)]] <- list( #precision with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Precision with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.precision.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_PRECISION, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted precision with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Precision with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.precision.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_PRECISION, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized precision with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Precision with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.precision.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)



# recall measure
graph.recall.measure <- function(graph1, graph2, weighted, normalized)
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
	# recall of the first relative to the second
	res <- sum(apply(cbind(x1,x2), 1, min)) / sum(x2)
	
	return(res)
}
COMP_MEASURES[[paste0(MEAS_RECALL, SFX_DUR)]] <- list( #recall with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Recall with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.recall.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_RECALL, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted recall with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Recall with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.recall.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_RECALL, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized recall with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Recall with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.recall.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)
COMP_MEASURES[[paste0(MEAS_RECALL, SFX_OCC)]] <- list( #recall with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Recall with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.recall.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_RECALL, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted recall with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted Recall with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.recall.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_RECALL, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized recall with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized Recall with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.recall.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)



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
COMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_DUR)]] <- list( #F-measure with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="F-measure with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.f.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_DUR)]] <- list( #Weighted F-measure with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_DUR)]] <- list( #Weighted normalized F-measure with duration graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Scene-Based Duration Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_DUR)
		graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)
COMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_OCC)]] <- list( #F-measure with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="F-measure with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.f.measure(graph1=graph, graph2=ref, weighted=FALSE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_OCC)]] <- list( #Weighted F-measure with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted F-measure with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=FALSE)
	}
)
COMP_MEASURES[[paste0(MEAS_FMEASURE, SFX_WEIGHT, SFX_NORM, SFX_OCC)]] <- list( #Weighted normalized F-measure with occurrences graph
	type=numeric(),
	bounds=c(0,1),
	cname="Weighted normalized F-measure with Scene-Based Occurrences Graph",
	foo=function(graph)
	{	ref <- get.ref.graph(SFX_OCC)
		graph.f.measure(graph1=graph, graph2=ref, weighted=TRUE, normalized=TRUE)
	}
)
