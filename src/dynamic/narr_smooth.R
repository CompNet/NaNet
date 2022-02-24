# This script extracts a dynamic network using the narrative smoothing method
# proposed in
# 		X. Bost, V. Labatut, S. Gueye, and G. Linarès, 
#		“Narrative smoothing: dynamic conversational network for the analysis of TV Series plots,” 
#		2nd International Workshop on Dynamics in Networks (DyNo/ASONAM), 2016, pp. 1111–1118.
#		DOI: 10.1109/ASONAM.2016.7752379
# 
# Vincent Labatut
# 02/2022
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
# stats.chars: list of characters with their attributes.
# char.scenes: which character appears in which scene.
# stats.scenes: allows retrieving scene durations.
#
# returns: sum of interaction scores.
###############################################################################
ns.compute.interaction.scores <- function(i, j, t0, t1, stats.chars, char.scenes, stats.scenes)
{	# targeted period
	scenes <- t0:t1
	# name of the concerned characters
	ij.names <- stats.chars[c(i,j),COL_STATS_CHAR]
	
	# init interaction weights for each considered scenes
	vals <- rep(0, length(scenes))
	
	# identify scenes involving one of our characters (i or j)
	chars.org <- char.scenes[t0:t1]
	chars.org.lgt <- future_sapply(chars.org, length)
	chars.dif <- future_lapply(chars.org, function(cc) setdiff(cc,ij.names))
	chars.dif.lgt <- future_sapply(chars.dif, length)
	idx <- which(chars.org.lgt > chars.dif.lgt)
	
	# update interaction weights for each scene involving one char (i or j)
	if(length(idx)>0)
		vals[idx] <- future_sapply(idx, function(s) chars.dif.lgt[s]*stats.scenes[scenes[s], COL_STATS_PANELS])
	#print(cbind(chars.dif.lgt[idx],stats.scenes[scenes[idx], COL_STATS_PANELS]))
	
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
# stats.chars: list of characters with their attributes.
# char.scenes: which character appears in which scene.
# stats.scenes: allows retrieving scene durations.
# 
# returns: a sequence of graphs corresponding to a dynamic graph.
###############################################################################
ns.graph.extraction <- function(stats.chars, char.scenes, stats.scenes, filtered=FALSE)
{	tlog(2, "Extracting a dynamic network using narrative smoothing")
	
	# NOTE: we could remove scenes with zero or one characters, but that does not change the outcome
	
	# possible filter the characters
	if(filtered)
	{	# read the graph
		graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
		g <- read_graph(file=graph.file, format="graphml")
		V(g)$name <- fix.encoding(strings=V(g)$name)
		V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
		kept <- which(!V(g)$Filtered)
		
		# remove from tables and list
		stats.chars <- stats.chars[kept,]
		char.scenes <- future_lapply(char.scenes, function(ll) intersect(ll, stats.chars[,COL_STATS_CHAR]))
	}
	
	# init char x scene matrix
	scene.mat <- matrix(FALSE, 
			nrow=nrow(stats.chars), ncol=length(char.scenes), 
			dimnames=list(stats.chars[,COL_STATS_CHAR], c()))
	for(s in 1:length(char.scenes))
	{	chars <- char.scenes[[s]]
		scene.mat[chars,s] <- rep(TRUE,length(chars))
	}
	
	# init the list of graphs
	res <- list()
	for(s in 1:length(char.scenes))
		res[[s]] <- make_empty_graph(n=nrow(stats.chars), directed=FALSE)
	
	# test:
	# i <- 345	# Dracon
	# j <- 1066	# Séréna
	##### individual scenes:
	# 1471 1472      1489      1491 1492      1494                1499 1500 1501 1504 1505      1507      1509
	#           1484      1490 1491 1492 1493 1494 1496 1497 1498                     1505 1506 1507 1508      1516 1517 1518 1519 1655
	##### common scenes:
	# 						   1491 1492      1494                                    1505      1507
	
	# loop over the characters for the first end point
	tlog.start.loop(2,nrow(stats.chars)-1,"Looping over characters (first character)")
	for(i in 1:(nrow(stats.chars)-1))
	{	i.name <- stats.chars[i,COL_STATS_CHAR]
		tlog.loop(4,i,"Processing character #i=\"",i.name,"\" (",i,"/",(nrow(stats.chars)),")")
		
		# scenes where char i appears
		#i.sc.ids <- which(future_sapply(char.scenes, function(chars) i.name %in% chars))
		i.sc.ids <- which(scene.mat[i.name,])
		
		# loop over the remaining characters for the second end point
		tlog.start.loop(4,(i+1):nrow(stats.chars),"Looping over characters (second character)")
		for(j in (i+1):nrow(stats.chars))
		{	j.name <- stats.chars[j,COL_STATS_CHAR]
			tlog.loop(6,j,"Processing character pair #i=\"",i.name,"\" (",i,"/",(nrow(stats.chars)),") -- #j=",j.name," (",j,"/",nrow(stats.chars),")")
			
			# scenes where char j appears
			#j.sc.ids <- which(future_sapply(char.scenes, function(chars) j.name %in% chars))
			j.sc.ids <- which(scene.mat[j.name,])
			
			# scenes where both chars appear
			ij.sc.ids <- intersect(i.sc.ids, j.sc.ids)
			ij.sc.dur <- stats.scenes[ij.sc.ids, COL_STATS_PANELS]
			tlog(8, "Scenes: i=",length(i.sc.ids)," j=",length(j.sc.ids)," both=",length(ij.sc.ids))
			
			# init weights
			ij.weights <- rep(-Inf, length(char.scenes))
			
			# check whether the characters interact at least once
			if(length(ij.sc.ids)>0)
			{	rem.sc.ids <- 1:length(char.scenes)
				
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
				if(end.id<length(char.scenes))
				{	e.ids <- (end.id+1):length(char.scenes)
					#ij.weights[e.ids] <- -Inf
					rem.sc.ids <- setdiff(rem.sc.ids, e.ids)
				}
				
				
				######## compute the weights for the remaining scenes
				tlog(8, "Processing inactive scenes")
				
				# compute the narrative persistence for the other scenes
				tlog(10, "Computing narrative persistence")
				narr.per <- rep(-Inf, length(rem.sc.ids))
				# init with previous scene involving both characters
				prev.sc.ids <- future_sapply(rem.sc.ids, function(s)
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
					updt.per <- future_sapply(ids.per, function(k)
							{	t <- rem.sc.ids[k]
								theta <- prev.sc.ids[k]
								ns.compute.interaction.scores(i, j, t0=theta+1, t1=t, stats.chars, char.scenes, stats.scenes)
							})
					#print(cbind(rem.sc.ids[ids.per],prev.sc.ids[ids.per],narr.per[ids.per],updt.per))
					narr.per[ids.per] <- narr.per[ids.per] - updt.per
				}
				
				# compute the narrative anticipation for the other scenes
				tlog(10, "Computing narrative anticipation")
				narr.ant <- rep(-Inf, length(rem.sc.ids))
				# init with next scene involving both characters
				next.sc.ids <- future_sapply(rem.sc.ids, function(s)
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
					updt.ant <- future_sapply(ids.ant, function(k)
							{	t <- rem.sc.ids[k]
								theta <- next.sc.ids[k]
								ns.compute.interaction.scores(i, j, t0=t, t1=theta-1, stats.chars, char.scenes, stats.scenes)
							})
					#print(cbind(rem.sc.ids[ids.ant],next.sc.ids[ids.ant],narr.ant[ids.ant],updt.ant))
					narr.ant[ids.ant] <- narr.ant[ids.ant] - updt.ant
				}
				
				# combine narrative persistence and anticipation
				tmp <- future_apply(cbind(narr.per, narr.ant), 1, max)
				ij.weights[rem.sc.ids] <- tmp
			}
			
			# normalize the weights
			ij.weights <- ns.normalization(ij.weights)
			
			# update the list of graphs
			for(s in 1:length(char.scenes))
			{	if(ij.weights[s]!=0)
				{	g <- res[[s]]
					g <- add_edges(graph=g, edges=c(i,j), attr=list(weight=ij.weights[s]))
					res[[s]] <- g
				}
			}
		}
		tlog.end.loop(4,"Finished the second character loop")
	}
	tlog.end.loop(6,"Finished the first character loop")
	
	return(res)
}




# gg <- ns.graph.extraction(stats.chars=data$stats.chars, char.scenes=data$char.scenes, stats.scenes=data$stats.scenes)
