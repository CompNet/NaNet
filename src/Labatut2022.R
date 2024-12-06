# This script reproduces the computations described in the following paper:
#
#	Vincent Labatut, 
#	“Complex Network Analysis of a Graphic Novel: The Case of the Bande Dessinée Thorgal,” 
#	Advances in Complex Systems 28(2):2240003, 2022.
#   DOI: 10.1142/S0219525922400033
# 
# Vincent Labatut
# 03/2022
#
# setwd("~/vlabatut/Eclipse/workspaces/Networks/NaNet")
# setwd("D:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/Labatut2022.R")
###############################################################################




###############################################################################
# setup parameters
SERIES <- "Thorgal"




###############################################################################
# load scripts
source("src/common/_include.R")

# start logging
start.rec.log(text=SERIES)




###############################################################################
# read raw data
data <- read.raw.data(char.det="implicit")
# OR, if already computed, read from file
#data <- read.corpus.data(char.det="implicit")

# compute corpus stats
data <- compute.corpus.stats(data, char.det="implicit")

# plot corpus stats
plot.corpus.stats(data, char.det="implicit")




###############################################################################
# extract static networks
data <- extract.static.graphs.base(data)

# plot these graphs
plot.static.graphs(data) 




###############################################################################
# compute scene-based graph stats
compute.static.statistics.base(data, char.det="implicit")

# plot these stats
generate.static.plots.base(data)




###############################################################################
# stop logging
end.rec.log()




###############################################################################
# additional plots
source("src/post/description/_all_post.R")
