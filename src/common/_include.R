#############################################################################################
# Just loads the scripts necessary to process the dataset. This script is needed when
# using foreach (parallel processing), since each worker must load all these dependencies.
# 
# 02/2019 Vincent Labatut
#
# source("src/common/_include.R")
#############################################################################################
# load libraries
source <- function(file) {eval(parse(file, encoding="UTF-8"),envir=.GlobalEnv)}	# allows solving encoding problems on Windows systems

library("igraph")			# graph analysis
library("CINNA")
#library("foreach")
library("future.apply")		# parallel processing
library("doParallel")		# parallel processing
library("vioplot")			# violin plots
library("ggplot2")			# plots
library("ggExtra")			# plugin of the previous
library("viridis")			# color palette
library("poweRlaw")			# power law testing
library("qgraph")			# graph layouts
#library("Rfast")			# not used
library("data.table")
library("latex2exp")		# include latex in plots
library("minpack.lm")		# nonlinear regression
library("sfsmisc")			# plot log axes
library("cluster")			# silhouette measure
library("SDMTools")
library("perm")				# permutation test
library("plotfunctions")	# gradient legend in plots



#############################################################################################
# handle warnings

#options(warn=1)			# as they happen
#options(warn=2)				# as errors
options(error=recover)		# debug

#plan(multisession)




###############################################################################
# init folder path

#SERIES <- "Test"
#SERIES <- "Ralph_Azham"
SERIES <- "Thorgal"
DATA_FOLDER <- file.path("data",SERIES)




###############################################################################
# plot options
PLOT_FORMAT_PDF <- ".pdf"
PLOT_FORMAT_PNG <- ".png"
PLOT_FORMAT <- c(
	PLOT_FORMAT_PDF,
	PLOT_FORMAT_PNG
)




###############################################################################
# load auxiliary functions

source("src/common/logging.R")
source("src/common/strings.R")
source("src/common/file_system.R")
source("src/common/table_cols.R")

source("src/common/graphs/_graphs_misc.R")
source("src/common/plots/_plot_misc.R")
source("src/common/stats/_stats_misc.R")

source("src/measures/measures.R")

source("src/corpus/read_data.R")
source("src/corpus/compute_stats.R")
source("src/corpus/plot_stats.R")

source("src/static/extract_graphs_scene.R")
source("src/static/extract_graphs_window.R")
source("src/static/compute_stats_base.R")
source("src/static/compute_stats_comp.R")
source("src/static/plot_nets.R")
source("src/static/plot_stats_base.R")
source("src/static/plot_stats_comp.R")

source("src/dynamic/narr_smooth.R")
