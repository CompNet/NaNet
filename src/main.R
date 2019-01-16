# TODO: Add comment
# 
# Vincent Labatut
# 11/2018
###############################################################################
library("igraph")


###############################################################################
# init folder path
#DATA_FOLDER <- file.path("data","Test")
DATA_FOLDER <- file.path("data","Ralph_Azham")


###############################################################################
# load auxiliary functions
source("src/file_constants.R")
source("src/extraction.R")
source("src/logging.R")


###############################################################################
# extract all networks
extract.graphs()


###############################################################################

