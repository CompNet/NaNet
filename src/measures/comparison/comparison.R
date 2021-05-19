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
###############################################################################
get.ref.graph <- function(weights)
{	# load the graph
	graph.file <- get.path.graph.file(mode="scenes")
	res <- read.graph(file=graph.file, format="graphml")
	
	# fix the weights
	if(weights==SFX_DUR)
		E(res)$weight <- E(res)$Duration
	else if(weights==SFX_OCC)
		E(res)$weight <- E(res)$Occurrences
	
	return(res)
}
