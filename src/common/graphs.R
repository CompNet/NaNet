#############################################################################################
# Functions used to handle various graph-related tasks.
# 
# 05/2021 Vincent Labatut
#############################################################################################




#############################################################################################
# Reverse the weights of the specified graph: if it is a cost, it becomes a capacity, and
# vice-versa. The bounds stay the same.
#
# This is usefull when you have capacities but want to compute a shortest path: igraph
# expects a cost, and not a capacity.
#
# g: weigted graph.
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
