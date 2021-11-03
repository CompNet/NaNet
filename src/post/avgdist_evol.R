# Studies how the average distance evolves as a function of the number of vertices.
# 
# Vincent Labatut
# 11/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/avgdist_evol.R")
###############################################################################
source("src/common/include.R")




# load raw data
data <- read.raw.data()
# compute the sequence of scene-based graphs (possibly one for each scene)
gs <- extract.static.graph.scenes(volume.info=data$volume.info, char.info=data$char.info, page.info=data$page.info, inter.df=data$inter.df, stats.scenes=data$stats.scenes, ret.seq=TRUE)

# compute average distance for each graph in the sequence
#print(any(sapply(gs, function(g) is_connected(g, mode="weak"))))	# check that each graph is connected
g.orders <- future_sapply(gs, gorder)
dist.vals <- future_sapply(gs, function(g) mean_distance(graph=g, directed=FALSE, unconnected=TRUE))

# plot the distance as the graph order
plot.file <- get.path.topomeas.plot(object="nodepairs", mode="scenes", meas.name="avgdist", plot.type="evolution_publication_lines")
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			# init plot
			plot(x=g.orders, y=dist.vals, 
				xlab="Number of vertices",
				ylab="Average distance",
				col="RED",
				type="l"
			)
		dev.off()
}
