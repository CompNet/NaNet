# This script extracts the comics networks used in the following paper:
#
#	Arthur Amalvy, Madeleine Janickyj, Shane Mannion, Pádraig MacCarron & Vincent Labatut, 
#	“Interconnected Kingdoms: comparing ‘A Song of Ice and Fire’ adaptations across media using complex networks,” 
#	Social Network Analysis and Mining 14:199, 2024.
#   DOI: 10.1007/s13278-024-01365-z
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
# note on scene ordering: publication="as in comic books" vs. story="as in novel chapters"




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

# plot these networks
# TODO

# compute and plot a few measures
plot.dyn.props.asoiaf(data, narr.units=c("scene", "chapter"))




###############################################################################
# stop logging
end.rec.log()
