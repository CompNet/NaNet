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
data.impl <- read.raw.data(char.det="implicit")
# OR, if already computed, read from file
#data.impl <- read.corpus.data(char.det="implicit")

# compute corpus stats
data.impl <- compute.corpus.stats(data=data.impl, char.det="implicit")

# plot these stats
plot.corpus.stats(data=data.impl, char.det="implicit")




###############################################################################
# extract scene-based networks
data.impl <- extract.static.graphs.base(data=data.impl)

# plot these graphs
plot.static.graphs(data=data.impl)




###############################################################################
# compute scene-based graph stats
compute.static.statistics.base(data=data.impl, char.det="implicit")

# plot these stats
generate.static.plots.base(data=data.impl)




###############################################################################
# stop logging
end.rec.log()

# TODO
# - fix x labels on histos in "by volume" stats
# - fine tune layout
# - see if cum net src aleady exists, otherwise implement
# - code novel order dyn net extr and anal
