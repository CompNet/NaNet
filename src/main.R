# Main script to extract network, compute their topological measures, and generate
# the corresponding plots.
# 
# Vincent Labatut
# 11/2018
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
###############################################################################
library("igraph")


###############################################################################
# init folder path
#DATA_FOLDER <- file.path("data","Test")
DATA_FOLDER <- file.path("data","Ralph_Azham")


###############################################################################
panel.window.sizes <- c(1,2,5,10,15,20,25,30)
panel.overlaps <- list(
	c(0), 										#1
	c(0,1), 									#2
	c(0,1,2,3,4), 								#5
	c(0,1,2,3,4,5,7,9), 						#10
	c(0,1,2,3,4,5,7,9,11,14), 					#15
	c(0,1,2,3,4,5,7,9,11,14,16,19), 			#20
	c(0,1,2,3,4,5,7,9,11,14,16,19,21,24), 		#25
	c(0,1,2,3,4,5,7,9,11,14,16,19,21,24,26,29)	#30
)
page.window.sizes <- c(1,2,5,10,15,20)
page.overlaps <- list(
	c(0), 										#1
	c(0,1), 									#2
	c(0,1,2,3,4), 								#5
	c(0,1,2,3,4,5,7,9), 						#10
	c(0,1,2,3,4,5,7,9,11,14), 					#15
	c(0,1,2,3,4,5,7,9,11,14,16,19) 				#20
)



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


###############################################################################
# read raw data
#data <- read.raw.data()


###############################################################################
# extract static networks
#extract.static.graphs(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)


###############################################################################
# compute stats
compute.static.statistics(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)


###############################################################################
# generate plots
#generate.static.plots(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)

