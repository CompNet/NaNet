#############################################################################################
# Just loads the scripts necessary to process the dataset. This script is needed when
# using foreach (parallel processing), since each worker must load all these dependencies.
# 
# 02/2019 Vincent Labatut
#############################################################################################
library("igraph")
library("foreach")
library("doParallel")


###############################################################################
# init folder path
#SERIES <- "Test"
#SERIES <- "Ralph_Azham"
SERIES <- "Thorgal"
DATA_FOLDER <- file.path("data",SERIES)


###############################################################################
# load auxiliary functions
source("src/logging.R")
source("src/read_raw.R")

source("src/constants/file_system.R")
source("src/constants/table_cols.R")
source("src/constants/topo_measures.R")

source("src/static/extract_nets.R")
source("src/static/compute_stats.R")
source("src/static/plot_stats.R")
