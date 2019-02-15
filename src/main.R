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
panel.window.sizes <- c(1,2,5,10,15,20,25,30)
panel.overlaps <- list(
	0, 0:1, 0:4,
	seq(0,9,2), seq(0,14,2), seq(0,19,2),
	seq(0,24,5), seq(0,29,5)
)
page.window.sizes <- c(1,2,5,10,15,20)
page.overlaps <- list(
	0, 0:1, 0:4,
	seq(0,9,2), seq(0,14,2), seq(0,19,2)
)


###############################################################################
# load auxiliary functions
source("src/file_constants.R")
source("src/table_constants.R")
source("src/read_raw.R")
source("src/extract_static.R")
source("src/stats_static.R")
source("src/logging.R")


###############################################################################
# read raw data
#data <- read.raw.data()


###############################################################################
# extract static networks
#extract.static.graphs(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)


###############################################################################
# compute stats
compute.static.statistics(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
