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
# narr.unit: narrative unit to perform the aggregation (scene, volume, arc).
# 
# returns: a sequence of graphs corresponding to a dynamic graph.
###############################################################################
cum.graph.extraction <- function(inter.df, char.stats, scene.chars, scene.stats, volume.stats, filtered=FALSE, pub.order=TRUE,  narr.unit=NA)
{	
	# extract the graph
	tlog(2,"Extracting the scene sequence graph")
	gg <- extract.static.graph.scenes(
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
		gg <- future_lapply(gg, function(g) delete_vertices(g, v=intersect(filt.names,V(g)$name)))
	}
	
	# aggregate to get a cumulative network
	tlog(2,"Aggregating by ",narr.unit)
	res <- list()
	prev.unit <- NA
	for(s in 1:length(gg))
	{	tlog(4,"Processing scene ",s,"/",length(gg))
		
		# retrieve current scene graph
		sc.g <- gg[[s]]
		
		# retrieve current narrative unit
		sc.id <- gg[[s]]$SceneId
		sc.idx <- which(scene.stats[,COL_SCENE_ID]==sc.id)
		if(narr.unit=="scene")
			cur.unit <- sc.id
		else if(narr.unit=="volume")
			cur.unit <- scene.stats[sc.idx,COL_VOLUME_ID]
		else if(narr.unit=="arc")
			cur.unit <- scene.stats[sc.idx,COL_ARC_ID]
		tlog(4,"Current ",narr.unit,": ",cur.unit," (previous ",narr.unit,": ",prev.unit,")")
		
		# very first graph of the sequence
		if(s==1)
		{	sc.g$NarrUnit <- paste0(narr.unit,"_",cur.unit)
			res[[1]] <- sc.g
			prev.unit <- cur.unit
		}
		# rest of the sequence
		else
		{	cur.g <- prev.g <- res[[length(res)]]
			
			# add current edges to previous graph
			if(gsize(sc.g)>0)
			{	el <- as_edgelist(graph=sc.g, names=TRUE)
				for(e in 1:nrow(el))
				{	#tlog(6,"e=",e," nrow(el)=",nrow(el))
					# edge already exists: increment weights
					if(are_adjacent(graph=cur.g, v1=el[e,1], v2=el[e,2]))
					{	idx <- get.edge.ids(graph=cur.g, vp=el[e,])
						#tlog(6,"idx=",idx)
						E(cur.g)[idx]$Occurrences <- E(cur.g)[idx]$Occurrences + E(sc.g)$Occurrences[e]
						E(cur.g)[idx]$Duration <- E(cur.g)[idx]$Duration + E(sc.g)$Duration[e]
					}
					# otherwise: create new edge
					else
						cur.g <- add_edges(graph=cur.g, edges=el[e,], 
								attr=list(Occurrences=E(sc.g)$Occurrences[e], Duration=E(sc.g)$Duration[e]))
				}
			}
			
			# if same narr unit as previous: store as previous graph
			if(prev.unit==cur.unit)
				res[[length(res)]] <- cur.g
			
			# otherwise: store as new graph
			else
			{	# remove isolates in previous graph
				prev.isolates <- which(degree(prev.g,mode="all")==0)
				prev.g <- delete_vertices(graph=prev.g, v=prev.isolates)
				res[[length(res)]] <- prev.g
				
				# add new graph in the sequence
				cur.g$NarrUnit <- paste0(narr.unit,"_",cur.unit)
				res[[length(res)+1]] <- cur.g
				#res <- c(res, list(prev.g))
				prev.unit <- cur.unit	
			}
		}
	}
	# remove isolates in last graph
	last.g <- res[[length(res)]]
	last.isolates <- which(degree(last.g,mode="all")==0)
	last.g <- delete_vertices(graph=last.g, v=last.isolates)
	res[[length(res)]] <- last.g
	
	# test: plot evolution of nbr of vertices
#	v.nbr <- sapply(res, gorder)
#	units <- 1:length(res)
#	x.labels <- sapply(res, function(g) g$NarrUnit)
#	plot(x=units, y=v.nbr, xaxt="n", xlab=paste0(narr.unit,"s"), ylab="Vertices", col="RED")
#	axis(side=1, at=units, labels=x.labels, las=2)
	
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
