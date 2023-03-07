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
source("src/post/asoiaf/_all.R")

# start logging
start.rec.log(text=SERIES)




###############################################################################
# read raw data
data <- read.raw.data.asoiaf()
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
# extract dynamic networks
extract.dyn.nets.asoiaf(data, narr.units=c("scene", "chapter"))

# compute and plot a few measures
plot.dyn.props.asoiaf(data, narr.units=c("scene", "chapter"))




###############################################################################
# stop logging
end.rec.log()

# TODO
# problems:
# - correct Thorgal data by splitting the group scenes (memory/dreams)
#   >> apply the modifications to the explicit annotations too!
