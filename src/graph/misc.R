#############################################################################################
# Functions used to handle various graph-related tasks.
# 
# 05/2021 Vincent Labatut
#
# source("src/graph/misc.R")
#############################################################################################
source("src/graph/generation.R")
source("src/graph/rewiring.R")
source("src/graph/topomeas.R")
source("src/graph/smallworldness.R")




#############################################################################################
# Returns the weakly connected largest component of the specified graph, and possibly the
# indices of the concerned vertices in the original graph.
#
# graph: graph to process.
# indices: if TRUE, the function returns a list with both the component and the vertex indices.
# 
# returns: the subgraph corresponding to the largest component, and possibly the ids of the
#          corresponding vertices in the original graph.
#############################################################################################
get.largest.component <- function(g, indices=FALSE)
{	comps <- components(graph=g, mode="weak")
	largest.comp.idx <- which.max(comps$csize)
	idx <- which(comps$membership==largest.comp.idx)
	largest.comp <- induced_subgraph(g, v=idx)
	
	if(indices)
		result <- list(comp=largest.comp, indices=idx)
	else
		result <- largest.comp
	
	return(result)
}




#############################################################################################
# Reverse the weights of the specified graph: if it is a cost, it becomes a capacity, and
# vice-versa. The bounds stay the same.
#
# This is usefull when you have capacities but want to compute a shortest path: igraph
# expects a cost, and not a capacity.
#
# g: weighted graph.
#
# returns: same graph, but reversed weights.
#############################################################################################
reverse.graph.weights <- function(g)
{	w <- E(g)$weight
	w <- reverse.weights(w)
	E(g)$weight <- w
	return(g)
}




#############################################################################################
# Reverse the specified weights, as explained in reverse.graph.weights, except this function
# directly works with weights instead of graphs.
#
# weights: weigts to reverse.
#
# returns: reversed weights.
#############################################################################################
reverse.weights <- function(w)
{	w <- (w-min(w))/(max(w)-min(w)) * (-1) * (max(w)-min(w)) + max(w)
	return(w)
}
