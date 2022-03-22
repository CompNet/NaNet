# This script reproduces the computations described in the following paper:
#
#	V. Labatut, 
#	“Complex Network Analysis of a Graphic Novel: The Case of the Bande Dessinée Thorgal,” 
#	Submitted, 2022.
# 
# Vincent Labatut
# 03/2022
#
# setwd("~/vlabatut/Eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/Labatut2022.R")
###############################################################################




###############################################################################
# setup parameters
SERIES <- "Thorgal"




###############################################################################
# load scripts
source("src/common/include.R")

# start logging
start.rec.log(text=SERIES)




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
data <- extract.static.graphs.base(data)

# plot these graphs
plot.static.graphs(data)




###############################################################################
# compute scene-based graph stats
compute.static.statistics.base(data)

# plot these stats
generate.static.plots.base(data)




###############################################################################
# stop logging
end.rec.log()




###############################################################################
# additional plots
#source("src/post/description/_all_post.R")
