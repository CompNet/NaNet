# Main script to extract the networks, compute their topological measures, and 
# generate the corresponding plots.
# 
# Vincent Labatut
# 02/2023
#
# setwd("~/vlabatut/Eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/dev_ASOIAF.R")
###############################################################################




###############################################################################
# setup parameters
SERIES <- "ASOIAF"




###############################################################################
# load scripts
source("src/common/_include.R")

# start logging
start.rec.log(text=SERIES)




###############################################################################
###############################################################################
# read raw data
data <- read.raw.data(char.det="implicit")
# OR, if already computed, read from file
#data <- read.corpus.data(char.det="implicit")

# compute corpus stats
data <- compute.corpus.stats(data=data, char.det="implicit")

# plot these stats
plot.corpus.stats(data=data, char.det="implicit")




###############################################################################
# extract scene-based networks
data <- extract.static.graphs.base(data=data)

# plot these graphs
plot.static.graphs(data=data)




###############################################################################
# compute scene-based graph stats
compute.static.statistics.base(data=data, char.det="implicit")

# plot these stats
generate.static.plots.base(data=data)




###############################################################################
# stop logging
end.rec.log()

# TODO
# - cum net: test if pub order extr works
# - code novel order dyn net extr, and anal
