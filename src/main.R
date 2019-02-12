# TODO: Add comment
# 
# Vincent Labatut
# 11/2018
###############################################################################
library("igraph")


###############################################################################
# init folder path
DATA_FOLDER <- file.path("data","Test")
#DATA_FOLDER <- file.path("data","Ralph_Azham")


###############################################################################
# load auxiliary functions
source("src/file_constants.R")
source("src/table_constants.R")
source("src/read_raw.R")
source("src/extract_graphs.R")
source("src/logging.R")


###############################################################################
# read raw data
data <- read.raw.data()


###############################################################################
# extract all networks
extract.graphs(data)


###############################################################################
# compute stats

