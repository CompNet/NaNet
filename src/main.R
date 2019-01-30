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
source("src/access_raw.R")
source("src/extract_graphs.R")
source("src/logging.R")


###############################################################################
# read raw data
data <- read.raw.data()


###############################################################################
# extract all networks
extract.graphs(data)


###############################################################################

# TODO
# - a volume does not necessarily starts at page 1
# - add volume to interactions.txt & pages.csv
#   >> volume file not needed anymore (can be built from pages.csv 
#   >> automatically generate volumes.csv
# - check:
#   - That a panel is not out of a page
#   - That a page is not out of a volume
