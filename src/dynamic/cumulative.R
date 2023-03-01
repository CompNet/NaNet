# This script extracts a standard cumulative dynamic network.
# 
# Vincent Labatut
# 02/2023
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/dynamic/cumulative.R")
###############################################################################




###############################################################################
# Extracts a cumulative dynamic network based on the specified scenes. 
# 
# inter.df: dataframe containing the pairwise interactions.
# char.stats: list of characters with their attributes.
# scene.chars: which character appears in which scene.
# scene.stats: allows retrieving scene durations.
# volume.stats: allows ordering volumes by publication date or story-wise.
# filtered: whether characters should be filtered or not.
# pub.order: whether to consider volumes in publication vs. story order.
# 
# returns: a sequence of graphs corresponding to a dynamic graph.
###############################################################################
cum.graph.extraction <- function(inter.df, char.stats, scene.chars, scene.stats, volume.stats, filtered=FALSE, pub.order=TRUE)
{	
	# extract the graph
	g <- extract.static.graph.scenes(
			inter.df=inter.df, 
			char.stats=char.stats, 
			volume.stats=volume.stats, 
			scene.stats=scene.stats, scene.chars=scene.chars,
			ret.seq=TRUE, pub.order=pub.order
	)
	
	# possiby compute the filtered version
	if(filtered)
	{	tlog(2,"Filtering the characters")
		filt.names <- char.stats[char.stats[,COL_FILTER]=="Discard",COL_NAME]
		if(length(filt.names)==0) stop("Empty list of filtered characters")
		g <- future_lapply(g, function(g) delete_vertices(g, v=intersect(filt.names,V(g)$name)))
	}
	
	return(res)
}




###############################################################################
# Record a cumulative dynamic graph as a series of graphs.
#
# gs: list of igraph objects representing a dynamic graph.
# filtered: whether the characters have been filtered or not.
# pub.order: whether to consider volumes in publication vs. story order.
# char.det: character detection mode ("implicit" or "explicit").
###############################################################################
cum.write.graph <- function(gs, filtered, pub.order=TRUE, char.det=NA)
{	if(pub.order)	# by publication order
		ord.fold <- "publication"
	else			# by story order
		ord.fold <- "story"
	
	base.file <- get.path.data.graph(mode="scenes", char.det=char.det, net.type="cumulative", order=ord.fold, filtered=filtered, pref="cum")
	write.dynamic.graph(gs=gs, base.path=base.file)
}




###############################################################################
# Read sequence of graphs representing a cumulative dynamic graph, based on a 
# sequence of graphml files, each one representing one step of the dynamic graph.
#
# filtered: whether the characters have been filtered or not.
# remove.isolates: whether to remove isolates in each time slice.
# pub.order: whether to consider volumes in publication vs. story order.
# char.det: character detection mode ("implicit" or "explicit").
#
# returns: list of igraph objects representing a dynamic graph.
###############################################################################
cum.read.graph <- function(filtered, remove.isolates=TRUE, pub.order=TRUE, char.det=NA)
{	if(pub.order)	# by publication order
		ord.fold <- "publication"
	else			# by story order
		ord.fold <- "story"
	
	base.file <- get.path.data.graph(mode="scenes", char.det=char.det, net.type="cumulative", order=ord.fold, filtered=filtered, pref="cum")
	gs <- read.dynamic.graph(base.file=base.file, remove.isolates=remove.isolates)
	
	return(gs)
}




################################################################################
## test
#data <- read.corpus.data(char.det="implicit")
#filtered <- FALSE
#pub.order <- FALSE
#gg <- cum.graph.extraction(inter.df=data$inter.df, char.stats=data$char.stats, scene.chars=data$scene.chars, scene.stats=data$scene.stats, volume.stats=data$volume.stats, filtered=filtered, pub.order=pub.order)
#cum.write.graph(gs=gg, filtered=filtered, pub.order=pub.order, char.det="implicit")
