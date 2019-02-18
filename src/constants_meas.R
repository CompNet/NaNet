# This scripts contains all measure-related constants.
# 
# Vincent Labatut
# 02/2019
###############################################################################


###############################################################################
# define measure names
MEAS_BETWEENNESS <- "betweenness"
MEAS_CLOSENESS <- "closeness"
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


###############################################################################
# define/load the topological measures
NODE_MEASURES <- list()
LINK_MEASURES <- list()
GRAPH_MEASURES <- list()
source("src/measures/betweenness.R")
source("src/measures/closeness.R")
source("src/measures/community.R")
source("src/measures/component.R")
source("src/measures/connectivity.R")
source("src/measures/degree.R")
source("src/measures/distance.R")
source("src/measures/eccentricity.R")
source("src/measures/edgebetweenness.R")
source("src/measures/element.R")
source("src/measures/spectral.R")
source("src/measures/transitivity.R")
ALL_MEASURES <- c(NODE_MEASURES, LINK_MEASURES, GRAPH_MEASURES)


###############################################################################
# graph measures with average, standard-deviation, min and max variants
ASMM_MEASURES <- c()
for(meas.name in c(names(NODE_MEASURES),names(LINK_MEASURES)))
{	#print(meas.name)
	if(paste0(meas.name,SFX_AVG) %in% names(GRAPH_MEASURES) && 
			paste0(meas.name,SFX_STDEV) %in% names(GRAPH_MEASURES) && 
			paste0(meas.name,SFX_MIN) %in% names(GRAPH_MEASURES) && 
			paste0(meas.name,SFX_MAX) %in% names(GRAPH_MEASURES))
	{	#print(meas.name)
		ASMM_MEASURES <- c(ASMM_MEASURES, meas.name)
	}
}

