# Extracts the narrative as two networks, in order to produce two distinct
# graphical reprensentations and highlight changes in the global structure
# of the interpersonal relationships. That did not produce anything 
# interesting.
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

# load raw data
data <- read.raw.data()

# load full graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
# get filtered characters
filt.names <- V(g)$name[V(g)$Filtered]
if(length(filt.names)==0) stop("Empty list of filtered characters")

# compute split scene
split.vol <- "23"	# The Cage
idx <- which(data$stats.scenes[,COL_STATS_VOLUME]==split.vol)[1]
idx <- which(data$inter.df[,"SceneId"]==idx)[1]

# init graph list
gs <- list()

# extract first network
tlog(0,"Extract first network")
idx1 <- 1:(idx-1)
gs[[1]] <- extract.static.graph.scenes(volume.info=data$volume.info, char.info=data$char.info, page.info=data$page.info, inter.df=data$inter.df[idx1,], stats.scenes=data$stats.scenes[idx1,])

# extract second network
tlog(0,"Extract second network")
idx2 <- idx:nrow(data$inter.df)
gs[[2]] <- extract.static.graph.scenes(volume.info=data$volume.info, char.info=data$char.info, page.info=data$page.info, inter.df=data$inter.df[idx2,], stats.scenes=data$stats.scenes[idx2,])




###############################################################################
# plot both graphs

# set up vertex colors
vcols <- rep("LIGHTGREY",gorder(g))
col.char.nbr <- 5 
E(g)$weight <- E(g)$Duration
btw <- betweenness(graph=g, directed=FALSE, weights=reverse.weights(E(g)$weight), normalized=FALSE)
col.char.idx <- order(btw,decreasing=TRUE)[1:col.char.nbr]
vcols[col.char.idx] <- get.palette(col.char.nbr)

# create files
for(i in 1:length(gs))
{	# compute graphical parameters
	tmp <- compute.graphical.params(gs[[i]])
	vsizes <- tmp$vsizes
	vlabs <- tmp$vlabs
	vlabsizes <- tmp$vlabsizes
	nww <- tmp$nww
	ewidths <- tmp$ewidths
	
	# set up edge colors
	el <- as_edgelist(graph=gs[[i]], names=FALSE)
	ecols <- sapply(1:nrow(el), function(r) combine.colors(col1=vcols[el[r,1]], col2=vcols[el[r,2]]))
	ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],100-100*lame.normalize(nww[i],exp=3)))
	
	# plot whole unfiltered graph
	graph.file <- get.path.graph.file(mode="scenes", filtered=FALSE, ext=paste0("_part_",i))
	tlog(6,"Plotting the whole unfiltered graph in file ",graph.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
			plot(gs[[i]],
				layout=LAYOUT,
				vertex.size=vsizes, vertex.color=vcols,
				vertex.label=vlabs, vertex.label.cex=vlabsizes,
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				edge.color=ecols, edge.width=ewidths, 
				rescale=FALSE, #axe=TRUE, 
				xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2])
			)
		dev.off()
	}
	
	# get filtered graph
	idx.filtr <- which(!V(g)$Filtered & degree(gs[[i]],mode="all")>0)
	g.filtr <- delete_vertices(graph=gs[[i]], v=which(V(g)$Filtered | degree(gs[[i]],mode="all")==0))
	el <- as_edgelist(graph=gs[[i]], names=FALSE)
	idx.efiltr <- which(el[,1] %in% idx.filtr & el[,2] %in% idx.filtr)
	
	# plot whole filtered graph
	graph.file <- get.path.graph.file(mode="scenes", filtered=TRUE, ext=paste0("_part_",i))
	tlog(6,"Plotting the whole filtered graph in file ",graph.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
			plot(g.filtr,
				layout=LAYOUT[idx.filtr,],	# lay.filtr
				vertex.size=vsizes[idx.filtr], vertex.color=vcols[idx.filtr],
				vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsizes[idx.filtr],
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				edge.color=ecols[idx.efiltr], edge.width=ewidths[idx.efiltr], 
				rescale=FALSE, #axe=TRUE, 
				xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2])
			)
		dev.off()
	}
}




###############################################################################
# end logging
end.rec.log()
