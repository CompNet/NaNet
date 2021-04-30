#############################################################################################
# Just loads the scripts necessary to process the dataset. This script is needed when
# using foreach (parallel processing), since each worker must load all these dependencies.
# 
# 02/2019 Vincent Labatut
#############################################################################################
# load libraries

library("igraph")
#library("foreach")
library("future.apply")
library("doParallel")
library("vioplot")




#############################################################################################
# handle warnings

options(warn=1)			# as they happen
#options(warn=2)				# as errors
#options(error=recover)		# debug

plan(multisession)




###############################################################################
# init folder path

#SERIES <- "Test"
#SERIES <- "Ralph_Azham"
SERIES <- "Thorgal"
DATA_FOLDER <- file.path("data",SERIES)




###############################################################################
# load auxiliary functions

source("src/read_raw.R")

source("src/common/logging.R")
source("src/common/file_system.R")
source("src/common/table_cols.R")
source("src/common/topo_measures.R")

source("src/static/extract_nets.R")
source("src/static/compute_stats.R")
source("src/static/plot_stats.R")
