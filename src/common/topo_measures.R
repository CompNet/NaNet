# This scripts contains all measure-related constants.
# 
# Vincent Labatut
# 02/2019
###############################################################################


###############################################################################
# topological measure names
MEAS_BETWEENNESS <- "betweenness"
MEAS_CLOSENESS <- "closeness"
MEAS_HARMO_CLOSENESS <- "harmo-closeness"
MEAS_MODULARITY <- "modularity"
MEAS_COMMUNITY <- "community"
MEAS_COMPONENT <- "component"
MEAS_LK_CONNECT <- "link-connectivity"
MEAS_ND_CONNECT <- "node-connectivity"
MEAS_DEGREE <- "degree"
MEAS_STRENGTH <- "strength"
MEAS_DISTANCE <- "distance"
MEAS_ECCENTRICITY <- "eccentricity"
MEAS_EDGEBETWNS <- "edgebetweenness"
MEAS_NODE <- "node"
MEAS_LINK <- "link"
MEAS_DENSITY <- "density"
MEAS_LINKWEIGHT <- "linkweight"
MEAS_EIGENCNTR <- "eigenvector"
MEAS_TRANSITIVITY <- "transitivity"
# comparison measure names
MEAS_COSINE_SIM <- "cosine"
MEAS_EUCLIDEAN_DIST <- "euclidean"
MEAS_JACCARD_SIM <- "jaccard"
MEAS_PRECISION <- "precision"
MEAS_RECALL <- "recall"
MEAS_FMEASURE <- "fmeasure"
MEAS_FALSENEG <- "falsenegative"
MEAS_FALSEPOS <- "falsepositive"
MEAS_TRUEPOS <- "truepositive"



###############################################################################
# define suffixes for topological measure names
SFX_NORM <- "-norm"
SFX_WEIGHT <- "-weighted"
SFX_NBR <- "-number"
SFX_SIZE <- "-size"
SFX_MIN <- "-min"
SFX_MAX <- "-max"
SFX_AVG <- "-average"
SFX_STDEV <- "-stdev"
SFX_ASSORT <- "-assortativity"
SFX_CENTRZ <- "-centralization"
SFX_LOCAL <- "-local"
SFX_GLOBAL <- "-global"
# for comparison measures
SFX_DUR <- "-duration"
SFX_OCC <- "-occurrences"
SFX_REDUCED <- "-reduced"
SFX_TOTAL <- "-total"


###############################################################################
# define/load the topological measures
NODE_MEASURES <- list()
NODEPAIR_MEASURES <- list()
LINK_MEASURES <- list()
GRAPH_MEASURES <- list()
source("src/measures/topological/betweenness.R")
source("src/measures/topological/closeness.R")
source("src/measures/topological/community.R")
source("src/measures/topological/component.R")
##source("src/measures/topological/connectivity.R")
source("src/measures/topological/degree.R")
source("src/measures/topological/distance.R")
source("src/measures/topological/eccentricity.R")
source("src/measures/topological/edgebetweenness.R")
source("src/measures/topological/element.R")
source("src/measures/topological/harmo_closeness.R")
source("src/measures/topological/spectral.R")
source("src/measures/topological/transitivity.R")
# comparison measures
GRAPHCOMP_MEASURES <- list()
NODECOMP_MEASURES <- list()
source("src/measures/comparison/comparison.R")
source("src/measures/comparison/euclidean.R")
source("src/measures/comparison/cosine.R")
source("src/measures/comparison/falseneg.R")
source("src/measures/comparison/falsepos.R")
source("src/measures/comparison/fmeasure.R")
source("src/measures/comparison/jaccard.R")
source("src/measures/comparison/precision.R")
source("src/measures/comparison/recall.R")
source("src/measures/comparison/truepos.R")
ALL_MEASURES <- c(NODE_MEASURES, NODEPAIR_MEASURES, LINK_MEASURES, GRAPH_MEASURES, GRAPHCOMP_MEASURES, NODECOMP_MEASURES)


###############################################################################
# graph measures with average, standard-deviation, min and max variants
#ASMM_MEASURES <- c()
#for(meas.name in c(names(NODE_MEASURES),names(LINK_MEASURES)))
#{	#print(meas.name)
#	if(paste0(meas.name,SFX_AVG) %in% names(GRAPH_MEASURES) && 
#			paste0(meas.name,SFX_STDEV) %in% names(GRAPH_MEASURES) && 
#			paste0(meas.name,SFX_MIN) %in% names(GRAPH_MEASURES) && 
#			paste0(meas.name,SFX_MAX) %in% names(GRAPH_MEASURES))
#	{	#print(meas.name)
#		ASMM_MEASURES <- c(ASMM_MEASURES, meas.name)
#	}
#}

