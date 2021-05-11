#############################################################################################
# Just loads the scripts necessary to process the dataset. This script is needed when
# using foreach (parallel processing), since each worker must load all these dependencies.
# 
# 02/2019 Vincent Labatut
#############################################################################################
# load libraries

library("igraph")
library("CINNA")
#library("foreach")
library("future.apply")
library("doParallel")
library("vioplot")
library("ggplot2")
library("ggExtra")
library("viridis")
library("poweRlaw")
library("qgraph")




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

source("src/common/logging.R")
source("src/common/colors.R")
source("src/common/distr_test.R")
source("src/common/graphs.R")
source("src/common/stats.R")
source("src/common/file_system.R")
source("src/common/table_cols.R")
source("src/common/plot_graphs.R")
source("src/common/topo_measures.R")

source("src/corpus/read_data.R")
source("src/corpus/compute_stats.R")

source("src/static/extract_nets.R")
source("src/static/compute_stats.R")
source("src/static/plot_nets.R")
source("src/static/plot_stats.R")
