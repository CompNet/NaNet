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
#DATA_FOLDER <- file.path("data","Test")
DATA_FOLDER <- file.path("data","Ralph_Azham")


###############################################################################
# load auxiliary functions
source("src/constants_file.R")
source("src/constants_table.R")
source("src/constants_meas.R")
source("src/read_raw.R")
source("src/extract_static.R")
source("src/stats_static.R")
source("src/plot_static.R")
source("src/logging.R")
