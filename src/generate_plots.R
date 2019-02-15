# This script contains functions to generate plots representing the previously
# computed topological measures. 
# 
# Vincent Labatut
# 02/2019
###############################################################################



# graph measures with average/stdev/min/max variants
asmm.group <- c(
		"betweenness", "betweenness-norm", "betweenness-weighted", "betweenness-weighted-norm",
		"closeness", "closeness-norm", "closeness-weighted", "closeness-weighted-norm",
		"community-size", "community-weighted-size", "component-size",
		"link-connectivity", "node-connectivity",
		"degree", "degree-norm", "strength",
		"distance", "distance-weighted",
		"eccentricity", "link-weight",
		"edgebetweenness", "edgebetweenness-weighted",
		"eigenvector", "eigenvector-norm", "eigenvector-weighted", "eigenvector-weighted-norm",
		"transitivity-local", "transitivity-weighted-local"
)
asmm.suffixes <- c("-average","-stdev","-min","-max")
# graph measures based on assortativity
assort.groups <- list(
	c("betweenness", "betweenness-norm", "betweenness-weighted", "betweenness-weighted-norm"),
	c("closeness", "closeness-norm", "closeness-weighted", "closeness-weighted-norm"),
	c("degree", "degree-norm", "strength"),
	c("eccentricity"),
	c("eigenvector", "eigenvector-norm", "eigenvector-weighted", "eigenvector-weighted-norm"),
	c("transitivity-local", "transitivity-weighted-local")
)
assort.suffix <- "-assortativity"
# graph measures based on centralization
ctrlztn.group <- list(
	c("betweenness", "betweenness-norm"),
	c("closeness", "closeness-norm"),
	c("degree", "degree-norm"),
	c("eigenvector", "eigenvector-norm")
)
ctrlztn.suffix <- "-centralization"
# single graph measures
single.group <- c("modularity", "community-number", "modularity-weighted", "community-weighted-number", "component-number",
		"node-count", "link-count", "density", "transitivity-global"
)








###############################################################################
# xxxx
#
# g: xxxx.
#
# returns: xxxx
###############################################################################
compute.static.node.statistics <- function(g, basename)
{	tlog(4,"xxx")
	
	tlog(1,"")	
}
