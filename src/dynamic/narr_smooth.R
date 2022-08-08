# This script extracts a dynamic network using the narrative smoothing method
# proposed in
# 		X. Bost, V. Labatut, S. Gueye, and G. Linarès, 
#		“Narrative smoothing: dynamic conversational network for the analysis of TV Series plots,” 
#		2nd International Workshop on Dynamics in Networks (DyNo/ASONAM), 2016, pp. 1111–1118.
#		DOI: 10.1109/ASONAM.2016.7752379
# 
# Vincent Labatut
# 02/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/dynamic/narr_smooth.R")
###############################################################################




###############################################################################
# For two characters i and j and a period of time [t0;t1], this function computes
# a vector of interaction scores. Each score corresponds to a scene in [t0;t1].
# It represents how much either i or j interacts with other characters on said scene.
# The function assumes that i and j do not interact with one another in [t0;t1].
# The score is obtained by multiplying the number of characters in the scene
# (other than i and j) and the duration of the scene (in panels). In other words,
# compared to the original narrative smoothing, we assume that all characters
# interact with one another during a scene (we do not consider speech turns).
# The obtained vector is then summed to get the final result.
#
# i: first character.
# j: second character.
# t0: first scene.
# t1: last scene.
# char.stats: list of characters with their attributes.
# scene.chars: which character appears in which scene.
# scene.stats: allows retrieving scene durations.
# scene.mat: characters x scenes matrix
#
# returns: sum of interaction scores.
###############################################################################
ns.compute.interaction.scores <- function(i, j, t0, t1, char.stats, scene.chars, scene.stats, scene.mat)
{	# targeted period
	scenes <- t0:t1
	# name of the concerned characters
	ij.names <- char.stats[c(i,j),COL_NAME]
	
	# init interaction weights for each considered scenes
	vals <- rep(0, length(scenes))
	
	# identify scenes involving one of our characters (i or j)
	idx <- which(scene.mat[i,scenes] | scene.mat[j,scenes])
	
#	chars.org <- scene.chars[t0:t1]
#	chars.org.lgt <- sapply(chars.org, length)
#	chars.dif <- lapply(chars.org, function(cc) setdiff(cc,ij.names))
#	chars.dif.lgt <- sapply(chars.dif, length)
#	idx <- which(chars.org.lgt > chars.dif.lgt)
	
	# update interaction weights for each scene involving one char (i or j)
	if(length(idx)>0)
	{	vals[idx] <- apply(scene.mat[,scenes[idx],drop=FALSE], 2, function(col) length(which(col)))*scene.stats[scenes[idx], COL_PANELS]
#		vals[idx] <- sapply(idx, function(s) chars.dif.lgt[s]*scene.stats[scenes[s], COL_PANELS])
		#print(cbind(chars.dif.lgt[idx],scene.stats[scenes[idx], COL_PANELS]))
	}
	
	# wrap up
	res <- sum(vals)
	return(res)
}




###############################################################################
# Normalizes the scores produced by the narrative smoothing, using a sigmoid
# function.
#
# x: values to normalize.
# mu: slope of the sigmoid function.
#
# returns: normalized values.
###############################################################################
ns.normalization <- function(x, mu=0.01)
{	res <- 1 / (1 + exp(-mu * x))
	res <- round(res, 4)
	
	return(res)
}




###############################################################################
# Uses narrative smoothing to extract a dynamic network based on the specified
# scenes. 
# 
# An similar Python function was proposed by Xavier Bost:
# https://github.com/bostxavier/Narrative-Smoothing/blob/c306c5bd94240dfbc8f5ea918857c8acf392ed81/network_processing.py#L52
# The difference is that we do not consider speech turns here: all characters
# participating to the same scene are considered to interact with one another,
# with a weight corresponding to the scene duration (expressesd in panels).
# 
# char.stats: list of characters with their attributes.
# scene.chars: which character appears in which scene.
# scene.stats: allows retrieving scene durations.
# volume.stats: allows ordering volumes by publication date or story-wise.
# filtered: whether characters should be filtered or not.
# pub.order: whether to consider volumes in publication vs. story order.
# 
# returns: a sequence of graphs corresponding to a dynamic graph.
###############################################################################
ns.graph.extraction <- function(char.stats, scene.chars, scene.stats, volume.stats, filtered=FALSE, pub.order=TRUE)
{	tlog(2, "Extracting a dynamic network using narrative smoothing")
	
	# NOTE: we could remove scenes with zero or one characters, but that does not change the outcome
	
	# read the whole graph
	graph.file <- get.path.data.graph(mode="scenes", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
	g <- read.graphml.file(file=graph.file)
	atts <- vertex_attr_names(graph=g)
	
	# possibly filter the characters
	if(filtered)
	{	kept <- which(V(g)$Filter=="Keep")
		g <- delete_vertices(graph=g, v=which(V(g)$Filter=="Discard"))
		char.stats <- char.stats[kept,]
		scene.chars <- lapply(scene.chars, function(ll) intersect(ll, char.stats[,COL_NAME]))
	}
	
	# init char x scene matrix
	scene.mat <- matrix(FALSE, 
		nrow=nrow(char.stats), ncol=nrow(scene.stats), 
		dimnames=list(char.stats[,COL_NAME], c()))
	for(s in 1:length(scene.chars))
	{	chars <- scene.chars[[s]]
		if(length(chars)>0)
			scene.mat[chars,s] <- rep(TRUE,length(chars))
	}
	
	# possibly order scenes
	if(!pub.order)
	{	vol.ranks <- volume.stats[scene.stats[,COL_VOLUME_ID],COL_RANK]
		scene.ranks <- scene.stats[,COL_SCENE_ID]
		idx <- order(vol.ranks, scene.ranks)
		scene.stats <- scene.stats[idx,]
		scene.mat <- scene.mat[,idx]
	}
	
	# init the list of graphs
	res <- list()
	for(s in 1:length(scene.chars))
	{	# create empty graph (no edge)
		gt <- make_empty_graph(n=nrow(char.stats), directed=FALSE)
		gt <- set_vertex_attr(graph=gt, name=COL_SCENE_ID, value=scene.stats[s,COL_SCENE_ID])
		# copy the vertex attributes of the static graph
		for(att in atts)
			gt <- set_vertex_attr(graph=gt, name=att, value=vertex_attr(graph=g, name=att))
		# store for later use
		res[[s]] <- gt
	}
	
	# test:
	# i <- 345	# Dracon
	# j <- 1066	# Séréna
	##### individual scenes:
	# 1471 1472      1489      1491 1492      1494                1499 1500 1501 1504 1505      1507      1509
	#           1484      1490 1491 1492 1493 1494 1496 1497 1498                     1505 1506 1507 1508      1516 1517 1518 1519 1655
	##### common scenes:
	# 						   1491 1492      1494                                    1505      1507
	
	# loop over the characters for the first end point
	tlog.start.loop(2,nrow(char.stats)-1,"Looping over characters (first character)")
	for(i in 1:(nrow(char.stats)-1))
	{	i.name <- char.stats[i,COL_NAME]
		tlog.loop(4,i,"Processing character #i=\"",i.name,"\" (",i,"/",(nrow(char.stats)),")")
		
		# scenes where char i appears
		#i.sc.ids <- which(sapply(scene.chars, function(chars) i.name %in% chars))
		i.sc.ids <- which(scene.mat[i.name,])
		
		# loop over the remaining characters for the second end point
		tlog.start.loop(4,nrow(char.stats)-i,"Looping over characters (second character)")
		for(j in (i+1):nrow(char.stats))
		{	j.name <- char.stats[j,COL_NAME]
			tlog.loop(6,j-i,"Processing character pair #i=\"",i.name,"\" (",i,"/",(nrow(char.stats)),") -- #j=",j.name," (",j-i,"/",nrow(char.stats)-i,")")
			
			# scenes where char j appears
			#j.sc.ids <- which(sapply(scene.chars, function(chars) j.name %in% chars))
			j.sc.ids <- which(scene.mat[j.name,])
			
			# scenes where both chars appear
			ij.sc.ids <- intersect(i.sc.ids, j.sc.ids)
			ij.sc.dur <- scene.stats[ij.sc.ids, COL_PANELS]
			tlog(8, "Scenes: i=",length(i.sc.ids)," j=",length(j.sc.ids)," both=",length(ij.sc.ids))
			
			# init weights
			ij.weights <- rep(-Inf, length(scene.chars))
			
			# check whether the characters interact at least once
			if(length(ij.sc.ids)>0)
			{	rem.sc.ids <- 1:length(scene.chars)
				
				######## set the weights for the scenes where the relation is active 
				tlog(8, "Processing active scenes")
				ij.weights[ij.sc.ids] <- ij.sc.dur
				rem.sc.ids <- setdiff(rem.sc.ids, ij.sc.ids)
				
				
				######## set the weights for the scenes before/after the first/last occurrence i or j
				tlog(8, "Processing inactive periods")
				any.ids <- union(i.sc.ids,j.sc.ids)
				start.id <- min(any.ids)
				end.id <- max(any.ids)
				
				# scenes before the very first occurrence of character i or j
				if(start.id>1)
				{	s.ids <- 1:(start.id-1)
					#ij.weights[s.ids] <- -Inf
					rem.sc.ids <- setdiff(rem.sc.ids, s.ids)
				}
				
				# scenes after the very last occurrence of character i or j
				if(end.id<length(scene.chars))
				{	e.ids <- (end.id+1):length(scene.chars)
					#ij.weights[e.ids] <- -Inf
					rem.sc.ids <- setdiff(rem.sc.ids, e.ids)
				}
				
				
				######## compute the weights for the remaining scenes
				tlog(8, "Processing inactive scenes")
				
				# compute the narrative persistence for the other scenes
				tlog(10, "Computing narrative persistence")
				narr.per <- rep(-Inf, length(rem.sc.ids))
				# init with previous scene involving both characters
				prev.sc.ids <- sapply(rem.sc.ids, function(s)
				{	cands <- which(ij.sc.ids<s)
					if(length(cands)>0)
						res <- ij.sc.ids[max(cands)]
					else
						res <- NA
					return(res)
				})
				ids.per <- which(!is.na(prev.sc.ids))
				if(length(ids.per)>0)
				{	narr.per[ids.per] <- ij.weights[prev.sc.ids[ids.per]]
					#print(cbind(rem.sc.ids,prev.sc.ids,narr.per))
					# update with intermediary scenes involving only one of the characters
					updt.per <- sapply(ids.per, function(k)
					{	t <- rem.sc.ids[k]
						theta <- prev.sc.ids[k]
						ns.compute.interaction.scores(i, j, t0=theta+1, t1=t, char.stats, scene.chars, scene.stats, scene.mat)
					})
					#print(cbind(rem.sc.ids[ids.per],prev.sc.ids[ids.per],narr.per[ids.per],updt.per))
					narr.per[ids.per] <- narr.per[ids.per] - updt.per
				}
				
				# compute the narrative anticipation for the other scenes
				tlog(10, "Computing narrative anticipation")
				narr.ant <- rep(-Inf, length(rem.sc.ids))
				# init with next scene involving both characters
				next.sc.ids <- sapply(rem.sc.ids, function(s)
						{	cands <- which(ij.sc.ids>s)
							if(length(cands)>0)
								res <- ij.sc.ids[min(cands)]
							else
								res <- NA
							return(res)
						})
				ids.ant <- which(!is.na(next.sc.ids))
				if(length(ids.ant)>0)
				{	narr.ant[ids.ant] <- ij.weights[next.sc.ids[ids.ant]]
					#print(cbind(rem.sc.ids,next.sc.ids,narr.ant))
					# update with intermediary scenes involving only one of the characters
					updt.ant <- sapply(ids.ant, function(k)
							{	t <- rem.sc.ids[k]
								theta <- next.sc.ids[k]
								ns.compute.interaction.scores(i, j, t0=t, t1=theta-1, char.stats, scene.chars, scene.stats, scene.mat)
							})
					#print(cbind(rem.sc.ids[ids.ant],next.sc.ids[ids.ant],narr.ant[ids.ant],updt.ant))
					narr.ant[ids.ant] <- narr.ant[ids.ant] - updt.ant
				}
				
				# combine narrative persistence and anticipation
				tmp <- apply(cbind(narr.per, narr.ant), 1, max)
				ij.weights[rem.sc.ids] <- tmp
			}
			
			# normalize the weights
			ij.weights <- ns.normalization(ij.weights)
			
			# update the list of graphs
			for(s in 1:length(scene.chars))
			{	if(ij.weights[s]!=0)
				{	g <- res[[s]]
					g <- add_edges(graph=g, edges=c(i,j), attr=list(weight=ij.weights[s]))
					res[[s]] <- g
				}
			}
		}
		tlog.end.loop(4,"Finished the second character loop")
	}
	tlog.end.loop(2,"Finished the first character loop")
	
	return(res)
}




###############################################################################
# Record a dynamic graph as a series of graph.
#
# gs: list of igraph objects representing a dynamic graph.
# filtered: whether the characters have been filtered or not.
# pub.order: whether to consider volumes in publication vs. story order.
###############################################################################
ns.write.graph <- function(gs, filtered, pub.order=TRUE)
{	if(pub.order)	# by publication order
		ord.fold <- "publication"
	else			# by story order
		ord.fold <- "story"
	
	base.file <- get.path.data.graph(mode="scenes", net.type="narr_smooth", order=ord.fold, filtered=filtered, pref="ns")
	write.dynamic.graph(gs=gs, base.path=base.file)
}




###############################################################################
# Read sequence of graphs representing a dynamic graph, based on a sequence of
# graphml files, each one representing one step of the dynamic graph.
#
# filtered: whether the characters have been filtered or not.
# remove.isolates: whether to remove isolates in each time slice.
# pub.order: whether to consider volumes in publication vs. story order.
#
# returns: list of igraph objects representing a dynamic graph.
###############################################################################
ns.read.graph <- function(filtered, remove.isolates=TRUE, pub.order=TRUE)
{	if(pub.order)	# by publication order
		ord.fold <- "publication"
	else			# by story order
		ord.fold <- "story"
	
	base.file <- get.path.data.graph(mode="scenes", net.type="narr_smooth", order=ord.fold, filtered=filtered, pref="ns")
	gs <- read.dynamic.graph(base.file=base.file, remove.isolates=remove.isolates)
	
	return(gs)
}




################################################################################
## test
#data <- read.corpus.data()
#filtered <- FALSE
#pub.order <- FALSE
#gg <- ns.graph.extraction(char.stats=data$char.stats, scene.chars=data$scene.chars, scene.stats=data$scene.stats, volume.stats=data$volume.stats, filtered=filtered, pub.order=pub.order)
#ns.write.graph(gs=gg, filtered=filtered, pub.order=pub.order)
