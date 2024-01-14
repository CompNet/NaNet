# This script extracts a standard dynamic network, as a sequence of static
# graphs representing time slices. There is no aggregation here: a different
# function allows extracting cumulative graphs.
# 
# Vincent Labatut
# 03/2023
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/dynamic/instant.R")
###############################################################################




###############################################################################
# Extracts a dynamic network based on the specified scenes. 
# 
# inter.df: dataframe containing the pairwise interactions.
# char.stats: list of characters with their attributes.
# scene.chars: which character appears in which scene.
# scene.stats: allows retrieving scene durations.
# volume.stats: allows ordering volumes by publication date or story-wise.
# filtered: whether characters should be filtered or not.
# pub.order: whether to consider volumes in publication vs. story order.
# narr.unit: narrative unit of the dynamic graph (scene, volume, arc).
# 
# returns: a sequence of graphs corresponding to a dynamic graph.
###############################################################################
inst.graph.extraction <- function(inter.df, char.stats, scene.chars, scene.stats, volume.stats, filtered=FALSE, pub.order=TRUE,  narr.unit=NA)
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
	
	# possibly aggregate to handle the narrative unit
	res <- list()
	if(narr.unit!="scene")
	{	tlog(2,"Aggregating by ",narr.unit)
		prev.unit <- NA
		for(s in 1:length(gg))
		{	#tlog(4,"Processing scene ",s,"/",length(gg))
			
			# retrieve current scene graph
			sc.g <- gg[[s]]
			sc.g <- delete_graph_attr(sc.g, COL_SCENE_ID)	# remove the scene id, irrelevant for larger narrative units
			
			# retrieve current narrative unit
			sc.id <- gg[[s]]$SceneId
			sc.idx <- which(scene.stats[,COL_SCENE_ID]==sc.id)
			if(narr.unit=="chapter")
				cur.unit <- scene.stats[sc.idx,COL_CHAPTER_ID]
			else if(narr.unit=="volume")
				cur.unit <- scene.stats[sc.idx,COL_VOLUME_ID]
			else if(narr.unit=="arc")
				cur.unit <- scene.stats[sc.idx,COL_ARC_ID]
			#tlog(4,"Current ",narr.unit,": ",cur.unit," (previous ",narr.unit,": ",prev.unit,")")
			
			# first graph of a narrative unit
			if(is.na(prev.unit) || prev.unit!=cur.unit)
			{	# possibly remove isolates in previous graph
				# note: not needed anymore, as extract.static.graph.scenes now only returns the required vertices
#				if(!is.na(prev.unit))
#				{	prev.g <- res[[length(res)]]
#					prev.isolates <- which(degree(prev.g,mode="all")==0)
#					prev.g <- delete_vertices(graph=prev.g, v=prev.isolates)
#					res[[length(res)]] <- prev.g
#				}
				
				# add current instant graph in the sequence
				sc.g$NarrUnit <- paste0(narr.unit,"_",cur.unit)
				res[[length(res)+1]] <- sc.g
				prev.unit <- cur.unit
			}
			
			# rest of the narrative unit
			else
			{	prev.g <- res[[length(res)]]
				
				# add new vertices to previous graph
				new.names <- setdiff(V(sc.g)$name, V(prev.g)$name)
				if(length(new.names)>0)
				{	attr <- vertex_attr_names(sc.g)
					for(nm in new.names)
					{	# retrieve vertex attributes
						vals <- lapply(attr, function(a) vertex_attr(graph=sc.g,name=a,index=nm))
						names(vals) <- attr
						# add new vertex to current graph
						prev.g <- add_vertices(graph=prev.g, nv=1, attr=vals)
					}
					prev.g <- permute(graph=prev.g, permutation=rank(V(prev.g)$name))
				}
				
				# add current edges to previous graph
				if(gsize(sc.g)>0)
				{	el <- as_edgelist(graph=sc.g, names=TRUE)
					for(e in 1:nrow(el))
					{	#tlog(6,"e=",e," nrow(el)=",nrow(el))
						# edge already exists: increment weights
						if(are_adjacent(graph=prev.g, v1=el[e,1], v2=el[e,2]))
						{	idx <- get.edge.ids(graph=prev.g, vp=el[e,])
							#tlog(6,"idx=",idx)
							E(prev.g)[idx]$Occurrences <- E(prev.g)[idx]$Occurrences + E(sc.g)$Occurrences[e]
							E(prev.g)[idx]$Duration <- E(prev.g)[idx]$Duration + E(sc.g)$Duration[e]
						}
						# otherwise: create new edge
						else
							prev.g <- add_edges(graph=prev.g, edges=el[e,], 
									attr=list(Occurrences=E(sc.g)$Occurrences[e], Duration=E(sc.g)$Duration[e]))
					}
				}
				
				# update last graph in result sequence
				res[[length(res)]] <- prev.g
			}
		}
		
		# remove isolates in last graph
		# note: not needed anymore, see remark above
#		last.g <- res[[length(res)]]
#		last.isolates <- which(degree(last.g,mode="all")==0)
#		last.g <- delete_vertices(graph=last.g, v=last.isolates)
#		res[[length(res)]] <- last.g
	}
	
	# no aggregation needed for scenes, as it is the smallest narrative unit
	else
	{	for(s in 1:length(gg))
		{	# get current graph
			sc.g <- gg[[s]]
			
			# add narrative unit attribute to graph
			cur.unit <- gg[[s]]$SceneId
			sc.g$NarrUnit <- paste0(narr.unit,"_",cur.unit)
			
			# add graph to list
			res[[length(res)+1]] <- sc.g
		}
	}
	
	# test: plot evolution of nbr of vertices
#	v.nbr <- sapply(res, gorder)
#	units <- 1:length(res)
#	x.labels <- sapply(res, function(g) g$NarrUnit)
#	plot(x=units, y=v.nbr, type="l", xaxt="n", xlab=paste0(narr.unit,"s"), ylab="Vertices", col="RED")
#	axis(side=1, at=units, labels=x.labels, las=2)
	
	return(res)
}




###############################################################################
# Record a dynamic graph as a series of graphs.
#
# gs: list of igraph objects representing a dynamic graph.
# filtered: whether the characters have been filtered or not.
# pub.order: whether to consider volumes in publication vs. story order.
# char.det: character detection mode ("implicit" or "explicit").
###############################################################################
inst.write.graph <- function(gs, filtered, pub.order=TRUE, char.det=NA)
{	if(pub.order)	# by publication order
		ord.fold <- "publication"
	else			# by story order
		ord.fold <- "story"
	
	# retrieve narrative unit
	narr.unit <- strsplit(gs[[1]]$NarrUnit, split="_")[[1]][1]
	
	base.file <- get.path.data.graph(mode="scenes", char.det=char.det, net.type="instant", order=ord.fold, filtered=filtered, subfold=narr.unit, pref="inst")
	write.dynamic.graph(gs=gs, base.path=base.file)
}




###############################################################################
# Read sequence of graphs representing a dynamic graph, based on a sequence of 
# graphml files, each one representing one step of the dynamic graph.
#
# filtered: whether the characters have been filtered or not.
# remove.isolates: whether to remove isolates in each time slice.
# pub.order: whether to consider volumes in publication vs. story order.
# char.det: character detection mode ("implicit" or "explicit").
# narr.unit: narrative unit used to extract the dynamic network (scene, volume, etc.).
#
# returns: list of igraph objects representing a dynamic graph.
###############################################################################
inst.read.graph <- function(filtered, remove.isolates=TRUE, pub.order=TRUE, char.det=NA, narr.unit=NA)
{	if(pub.order)	# by publication order
		ord.fold <- "publication"
	else			# by story order
		ord.fold <- "story"
	
	base.file <- get.path.data.graph(mode="scenes", char.det=char.det, net.type="instant", order=ord.fold, filtered=filtered, subfold=narr.unit, pref="inst")
	gs <- read.dynamic.graph(base.file=base.file, remove.isolates=remove.isolates)
	
	return(gs)
}
