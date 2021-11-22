# Main script to extract the networks, compute their topological measures, and 
# generate the corresponding plots.
# 
# Vincent Labatut
# 11/2018
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/main.R")
###############################################################################
source("src/common/include.R")




###############################################################################
# start logging
start.rec.log(text=SERIES)




###############################################################################
## setup parameters
## test
#panel.window.sizes <- c(1, 2, 3)
#panel.overlaps <- list(c(0), c(0,1), c(0,1,2))
#page.window.sizes <- c(1,2,3)
#page.overlaps <- list(c(0), c(0,1), c(0,1,2))

# regular values
#panel.window.sizes <- c(1,2,5,10,15,20,25,30)
#panel.overlaps <- list(
#	c(0), 										#1
#	c(0,1), 									#2
#	c(0,1,2,3,4), 								#5
#	c(0,1,2,3,4,5,7,9), 						#10
#	c(0,1,2,3,4,5,7,9,11,14), 					#15
#	c(0,1,2,3,4,5,7,9,11,14,16,19), 			#20
#	c(0,1,2,3,4,5,7,9,11,14,16,19,21,24), 		#25
#	c(0,1,2,3,4,5,7,9,11,14,16,19,21,24,26,29)	#30
#)
#page.window.sizes <- c(1,2,5,10,15,20)
#page.overlaps <- list(
#	c(0), 										#1
#	c(0,1), 									#2
#	c(0,1,2,3,4), 								#5
#	c(0,1,2,3,4,5,7,9), 						#10
#	c(0,1,2,3,4,5,7,9,11,14), 					#15
#	c(0,1,2,3,4,5,7,9,11,14,16,19) 				#20
#)

## more complete parameters
panel.window.sizes <- c(1:30)
panel.overlaps <- lapply(panel.window.sizes, function(size) 0:(size-1)) # 465 networks
page.window.sizes <- c(1:20)
page.overlaps <- lapply(page.window.sizes, function(size) 0:(size-1))	# 210 networks




###############################################################################
# read raw data
data <- read.raw.data()
# compute and plot corpus stats
data <- compute.stats(data)




###############################################################################
# extract static networks
extract.static.graphs(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
# plot them
plot.static.graphs(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)




###############################################################################
# compute graph stats
compute.static.statistics(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
# plot them
generate.static.plots(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)



# TODO
# export current position to graphml file, then adjust manually with gephi 
# write script to quickly test how manually positioned vertices are rendered by igraph (esp. regarding labels)
# export position to layout file




###############################################################################
# stop logging
end.rec.log()
