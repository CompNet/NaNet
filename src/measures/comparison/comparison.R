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
# filtered: whether to focus on the most important nodes, or not.
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
#
# returns: the reference graph.
###############################################################################
get.ref.graph <- function(weights, filtered=FALSE)
{	# load the graph
	graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
	res <- read.graphml.file(file=graph.file)

	# possibly remove certain nodes
	if(filtered)
	{	
#		idx.filtr <- which(V(res)$Frequency>2)
#		g.filtr <- induced_subgraph(res, v=idx.filtr)
##		tmp <- get.largest.component(g.filtr, indices=TRUE)
##		idx.cmp <- idx.filtr[tmp$indices]
##		res <- tmp$comp

		g.filtr <- delete_vertices(graph=res, v=which(V(res)$Filter=="Discard"))
		
		res <- g.filtr
	}
	
	# fix the weights
	if(weights==SFX_DUR)
		E(res)$weight <- E(res)$Duration
	else if(weights==SFX_OCC)
		E(res)$weight <- E(res)$Occurrences
	
	return(res)
}
