# Extracts the narrative as two networks, in order to produce two distinct
# graphical reprensetations and highlight changes in the global structure
# of the interpersonal relationships.
#
# Vincent Labatut
# 01/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/partial_extr.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="PartialExtr")




###############################################################################
# represents the narrative as two distinct networks 
tlog(0,"Extract two distinct networks for the narrative")

# plot parameters
pal <- get.palette(2)

# load full graph to get filtered characters
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
filt.names <- V(g)$name[V(g)$Filtered]
if(length(filt.names)==0) error("Empty list of filtered characters")

# load raw data
tlog(0,"Extract the sequence of scene-related cumulative graphs")
data <- read.raw.data()

# compute split scene
split.vol <- "23"	# The Cage
idx <- which(data$stats.scenes[,COL_STATS_VOLUME]==split.vol)[1]
idx <- which(data$inter.df[,"SceneId"]==idx)[1]

# extract first network
idx1 <- 1:(idx-1)
g1 <- extract.static.graph.scenes(volume.info=data$volume.info, char.info=data$char.info, page.info=data$page.info, inter.df=data$inter.df[idx1,], stats.scenes=data$stats.scenes[idx1,])

# extract second network
idx2 <- idx:nrow(data$inter.df)
g2 <- extract.static.graph.scenes(volume.info=data$volume.info, char.info=data$char.info, page.info=data$page.info, inter.df=data$inter.df[idx2,], stats.scenes=data$stats.scenes[idx2,])

# plot the graphs





###############################################################################
# end logging
end.rec.log()
