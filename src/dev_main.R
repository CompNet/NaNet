# Main script to extract the networks, compute their topological measures, and 
# generate the corresponding plots.
# 
# Vincent Labatut
# 11/2018
#
# setwd("~/vlabatut/Eclipse/workspaces/Networks/NaNet")
# setwd("D:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/dev_main.R")
###############################################################################




###############################################################################
# setup parameters
#SERIES <- "Test"
#SERIES <- "Ralph_Azham"
SERIES <- "Thorgal"

# test
#panel.params <- list(
#	window.sizes=c(1, 2, 3),
#	overlaps=list(c(0), c(0,1), c(0,1,2))
#)
#page.params <- list(
#	window.sizes=c(1,2,3),
#	overlaps=list(c(0), c(0,1), c(0,1,2))
#)

# regular values
#panel.params <- list(
#	window.sizes=c(1,2,5,10,15,20,25,30),
#	overlaps=list(
#		c(0), 										#1
#		c(0,1), 									#2
#		c(0,1,2,3,4), 								#5
#		c(0,1,2,3,4,5,7,9), 						#10
#		c(0,1,2,3,4,5,7,9,11,14), 					#15
#		c(0,1,2,3,4,5,7,9,11,14,16,19), 			#20
#		c(0,1,2,3,4,5,7,9,11,14,16,19,21,24), 		#25
#		c(0,1,2,3,4,5,7,9,11,14,16,19,21,24,26,29)	#30
#	)
#)
#page.params <- list(
#	window.sizes <- c(1,2,5,10,15,20),
#	overlaps <- list(
#		c(0), 										#1
#		c(0,1), 									#2
#		c(0,1,2,3,4), 								#5
#		c(0,1,2,3,4,5,7,9), 						#10
#		c(0,1,2,3,4,5,7,9,11,14), 					#15
#		c(0,1,2,3,4,5,7,9,11,14,16,19) 				#20
#	)
#)

# more complete parameters
panel.params <- list(
	window.sizes=1:30,
	overlaps=lapply(1:30, function(size) 0:(size-1))	# 465 networks
)
page.params <- list(
	window.sizes=1:20,
	overlaps=lapply(1:20, function(size) 0:(size-1))	# 210 networks
)




###############################################################################
# load scripts
source("src/common/_include.R")

# start logging
start.rec.log(text=SERIES)




###############################################################################
# read raw data
#data <- read.raw.data()
# OR, if already computed, read from file
data <- read.corpus.data()

# compute corpus stats
#data <- compute.corpus.stats(data)

# plot these stats
#plot.corpus.stats(data)




###############################################################################
# extract static scene-based networks
#data <- extract.static.graphs.base(data)

# plot these graphs
plot.static.graphs(data)




###############################################################################
# compute scene-based graph stats
#compute.static.statistics.base(data, char.det="implicit")

# plot these stats
#generate.static.plots.base(data)




###############################################################################
# extract static panel- and page-based networks
#extract.static.graphs.window(data, char.det="implicit", panel.params, page.params)

# compute their stats
#compute.static.statistics.window(char.det="implicit", panel.params, page.params)

# plot these stats
#generate.static.plots.window(panel.params, page.params)




###############################################################################
# compute comparison stats
#compute.static.statistics.comparison(data, char.det="implicit", panel.params, page.params)

# plot these stats
#generate.static.plots.comparison(data, panel.params, page.params)




###############################################################################
# stop logging
end.rec.log()
