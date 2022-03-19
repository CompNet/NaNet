# This script reproduces the computations described in the following paper:
#
#	V. Labatut, 
#	“,” 
#	Submitted, 2022.
# 
# Vincent Labatut
# 03/2022
#
# setwd("~/vlabatut/Eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/main.R")
###############################################################################




###############################################################################
# start logging
start.rec.log(text=SERIES)




###############################################################################
# setup parameters
SERIES <- "Thorgal"

# load scripts
source("src/common/include.R")




###############################################################################
# read raw data
data <- read.raw.data()
# OR, if already computed, read from file
#data <- read.corpus.data()

# compute corpus stats
data <- compute.corpus.stats(data)
# plot corpus stats
plot.corpus.stats(data)




###############################################################################
# extract static networks
data <- extract.static.graphs(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
# plot them
plot.static.graphs(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)




###############################################################################
# compute graph stats
compute.static.statistics(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
# plot them
generate.static.plots(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)




###############################################################################
# stop logging
end.rec.log()




###############################################################################
# additional plots
source("src/post/description/_all_post.R")
