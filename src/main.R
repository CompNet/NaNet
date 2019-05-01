# Main script to extract network, compute their topological measures, and generate
# the corresponding plots.
# 
# Vincent Labatut
# 11/2018
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# setwd("D:/Users/Vincent/eclipse/workspaces/Networks/NaNet")
###############################################################################
source("src/define_imports.R")
start.rec.log(text=SERIES)


###############################################################################
# setup parameters
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


##################### configure parallel processing
#cn <- detectCores(all.tests=TRUE)
#if(!is.na(cn))
#	cl <- makeCluster(cn)		# automatically use all the available processors
#else
cl <- makeCluster(4)		# manually set the number of processors to use
registerDoParallel(cl)


###############################################################################
# read raw data
data <- read.raw.data()


###############################################################################
# extract static networks
#extract.static.graphs(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)


###############################################################################
# compute stats
compute.static.statistics(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)


###############################################################################
# generate plots
generate.static.plots(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)


###############################################################################
# stop parallel processing
stopCluster(cl)

# maybe different number of nodes between parameter sets?
# >> systematically put all nodes?
# >> but then, how to deal with disconnected nets?
