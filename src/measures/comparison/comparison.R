# This script contains methods computing 
# measures allowing to compare networks in various ways.
# 
# Vincent Labatut
# 02/2019
###############################################################################


###############################################################################
# Returns the required reference graph, with the appropriate weights.
# 
# weights: desired weights (durations or occurrences).
# reduced: whether to focus on the most important nodes, or not.
#
# returns: the reference graph.
###############################################################################
get.ref.graph <- function(weights, reduced=FALSE)
{	# load the graph
	graph.file <- get.path.graph.file(mode="scenes")
	res <- read.graph(file=graph.file, format="graphml")

	# possibly remove certain nodes
	if(reduced)
	{	idx.filtr <- which(V(res)$Frequency>2)
		g.filtr <- induced_subgraph(res, v=idx.filtr)
#		tmp <- get.largest.component(g.filtr, indices=TRUE)
#		idx.cmp <- idx.filtr[tmp$indices]
#		res <- tmp$comp
res <- g.filtr
	}
	
	# fix the weights
	if(weights==SFX_DUR)
		E(res)$weight <- E(res)$Duration
	else if(weights==SFX_OCC)
		E(res)$weight <- E(res)$Occurrences
	
	return(res)
}
