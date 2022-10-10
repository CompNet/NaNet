# This script contains functions related to the extraction of character
# networks based on raw interaction tables.
# 
# Vincent Labatut
# 11/2018
###############################################################################




###############################################################################
# Extracts a graph based on a list of pairwise interactions, using the
# scene as the time unit, without overlap. Nodes are characters, and links
# represent them (inter-)acting during the same scene.
#
# Depending on the parameters, the function extracts the network corresponding
# to the whole series, or a specific narrative arc or volume. It can also output
# a sequence of cumulative graphs (each one corresponding to a scene).
#
# inter.df: dataframe containing the pairwise interactions.
# char.stats: table describing all the characters occurring in the BD series.
# scene.stats: table describing all the scenes constituting the BD series.
# scene.chars: list of characters occurring in each scene.
# volume.stats: table describing all the volumes constituting the BD series.
# vol: volume of interest (optional, and ignored if arc is specififed or if ret.seq is TRUE).
# arc: narrative arc of interest (optional, and ignored if ret.seq is TRUE).
# ret.set: whether to return the full sequence of incremental graphs (longer computation).
# pub.order: whether to consider volumes in publication vs. story order.
#
# returns: the corresponding static graph. It contains several edge weights:
#		   - Occurrences: number of interactions between the concerned nodes.
#		   - Duration: total duration (in number of panels).
#		   If ret.set==TRUE, then the function returns a list of graphs.
###############################################################################
extract.static.graph.scenes <- function(inter.df, char.stats, scene.stats, scene.chars, volume.stats, vol=NA, arc=NA, ret.seq=FALSE, pub.order=TRUE)
{	tlog(2,"Extracting the scene-based static graph")
	res <- list()
	vname <- NA
	
	# init the dataframe
	static.df <- data.frame(
			From=character(), To=character(), 
			Occurrences=integer(), Duration=integer(), 
			stringsAsFactors=FALSE)
	Encoding(static.df$From) <- "UTF-8"
	Encoding(static.df$To) <- "UTF-8"
	cn <- c(COL_CHAR_FROM, COL_CHAR_TO, COL_OCCURRENCES, COL_DURATION)
	colnames(static.df) <- cn
	
	# possibly filter interactions
	if(!is.na(arc))
	{	is <- which(inter.df[,COL_ARC_ID]==arc)
		scenes.ord <- which(scene.stats[,COL_ARC_ID]==arc)
	}
	else if(!is.na(vol))
	{	vname <- paste0(vol,"_",volume.stats[vol,COL_VOLUME])
		is <- which(inter.df[,COL_VOLUME_ID]==vol)
		scenes.ord <- which(scene.stats[,COL_VOLUME_ID]==vol)
	}
	else
	{	is <- 1:nrow(inter.df)
		scenes.ord <- 1:nrow(scene.stats)
	}
	
	# possibly sort scenes and interactions by story order
	if(!pub.order)
	{	is <- is[order(inter.df[is,COL_RANK])]
		scenes.ord <- scenes.ord[order(scene.stats[scenes.ord,COL_RANK])]
	}
		
	# possibly init the list with empty graphs or isolates
	if(ret.seq)
	{	#tlog(2,"Initializing the graph list")
		s <- 1
		while(scenes.ord[s]!=inter.df[is[1],COL_SCENE_ID])
		{	#tlog(4,"Processing s=",s," (scenes.ord[s]=",scenes.ord[s]," and inter.df[is[1],COL_SCENE_ID]=",inter.df[is[1],COL_SCENE_ID],") -- (length(scene.chars[[s]]=",length(scene.chars[[s]]),")")
			if(length(scene.chars[[scenes.ord[s]]])==0)
				g <- make_empty_graph(n=0, directed=FALSE)
			else if(length(scene.chars[[scenes.ord[s]]])==1)
			{	idx <- which(char.stats[,COL_NAME]==scene.chars[[scenes.ord[s]]])
				g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.stats[idx,])
			}
			g <- set_edge_attr(g, name=COL_OCCURRENCES, value=NA)
			g <- set_edge_attr(g, name=COL_DURATION, value=NA)
			res[[s]] <- g
			s <- s + 1
		}
	}
	
	# build the edgelist by considering each line (i.e. interaction) in the dataframe
	prev.scene <- NA
	prev.scene.idx <- NA
	tlog(2,"Building the edge list")
	for(i in is)
	{	# get the current scene id
		cur.scene <- inter.df[i,COL_SCENE_ID]
		cur.scene.idx <- which(scenes.ord==cur.scene)
		#tlog(4,"Processing scene ",cur.scene," (",cur.scene.idx,"/",length(scenes.ord),")")
		
		# get the characters
		from.char <- inter.df[i,COL_CHAR_FROM]
		to.char <- inter.df[i,COL_CHAR_TO]
		
		# get the corresponding row in the new (integrated) dataframe
		index <- which(static.df[,COL_CHAR_FROM]==from.char & static.df[,COL_CHAR_TO]==to.char)
		
		# compute the number of panels in the sequence
		length <- inter.df[i,COL_PANEL_END_ID] - inter.df[i,COL_PANEL_START_ID] + 1
		
		# update the integrated dataframe
		if(length(index)==0)
		{	# insert the couple of characters (never met before)
			tmp.df <- data.frame(From=from.char, To=to.char, Occurrences=1, Duration=length, stringsAsFactors=FALSE)
			colnames(tmp.df) <- cn
			static.df <- rbind(static.df, tmp.df)
		}
		else
		{	# update the couple of characters (already inserted vefore)
			static.df[index, COL_OCCURRENCES] <- static.df[index, COL_OCCURRENCES] + 1
			static.df[index, COL_DURATION] <- static.df[index, COL_DURATION] + length
		}
		
		# if graph sequence required
		if(ret.seq)
		{	# possibly copy previous graph
			if(!is.na(prev.scene) && cur.scene!=prev.scene)
			{	# possibly several times, to represent interaction-less scenes
				for(s in (prev.scene.idx+1):(cur.scene.idx-1))
				{	#tlog(6,"s=",s," scenes.ord[s-1]=",scenes.ord[s-1]," length(res)=",length(res))
					g <- res[[s-1]]
					g <- set_graph_attr(graph=g, name="SceneId", value=scene.stats[scenes.ord[s],COL_SCENE_ID])
					res[[s]] <- g
				}
			}
			# build and add current graph
			static.df <- static.df[order(static.df[,COL_CHAR_FROM],static.df[,COL_CHAR_TO]),]
			idx <- which(char.stats[,COL_NAME] %in% c(cbind(static.df[,COL_CHAR_FROM],static.df[,COL_CHAR_TO])))
			g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.stats[idx,])
			g$Scene <- cur.scene
			res[[cur.scene.idx]] <- g
		}
		
		prev.scene <- cur.scene
		prev.scene.idx <- cur.scene.idx
	}
	
	# set up result variable
	if(ret.seq)
	{	msg <- paste0("returning a series of ",length(res)," graphs")
	}
	else
	{	static.df <- static.df[order(static.df[,COL_CHAR_FROM],static.df[,COL_CHAR_TO]),]
		#print(static.df)
		
		# init the graph
		g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.stats)
		#plot(g, layout=layout_with_fr(g))
		res <- g
		msg <- "returning a single graph"
	}
	
	tlog(2,"Extraction of the scene-based static graph completed, ",msg)
	return(res)
}




###############################################################################
# Receives the unfiltered static graph and extract the filtered version. Records
# the list of filtered characters in a separate file.
#
# g: unfiltered static graph.
# char.stats: table containing the character statistics.
# volume.stats: table containing the volume statistics.
#
# returns: list containing the updated graphs and stats.
###############################################################################
extract.static.graph.filtered <- function(g, char.stats, volume.stats)
{	tlog(1,"Filtering extras from the graph")
	
	tlog(2,"Counts for criteria deg>1 vs. freq>3")
	x1 <- sapply(degree(g), function(x) if(x<=1) "Interactions<=1" else "Interactions>1")
	x2 <- sapply(V(g)$Frequency, function(x) if(x<=3) "Occurrences<=3" else "Occurrences>3")
	tt <- table(x1,x2)
	ttt <- rbind(cbind(tt,rowSums(tt)), c(colSums(tt),sum(tt)))
	colnames(ttt)[3] <- "Total"
	rownames(ttt)[3] <- "Total"
	print(ttt)
	tlog(4,"Interactions: number of characters the character of interest interacts with through the whole series (i.e. its degree in the graph)")
	tlog(4,"Occurrences: number of scenes in which the character of interest appears through the whole series (i.e. its frequency)")
	file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=FALSE, pref="filtering_criteria", ext=".csv")
	write.csv(x=ttt, file=file, fileEncoding="UTF-8", row.names=TRUE)
	
	# filtering by freq and occ
	crit <- degree(g)<=1 | V(g)$Frequency<=3
	idx.remove <- which(crit)
	idx.keep <- which(!crit)
	g.filtr <- delete_vertices(graph=g, v=idx.remove)
	tlog(2,"Named characters among those meeting the criteria: ",length(which(V(g.filtr)$Named)))
	
	# keeping the giant comp
	tlog(2,"Component sizes in the network of characters meeting the criteria: ",paste(components(graph=g.filtr, mode="weak")$csize,collapse=", "))
	tmp <- get.largest.component(g.filtr, indices=TRUE)
	idx.cmp <- idx.keep[tmp$indices]
	g.cmp <- tmp$comp
	
	# write graph to file
	graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=TRUE, pref="graph", ext=".graphml")
	tlog(4,"Recording filtered graph in \"",graph.file,"\"")
	write_graph(graph=g.cmp, file=graph.file, format="graphml")
	
	# add new attribute to unfiltered graph
	V(g)$Filter <- rep("Discard",gorder(g))
	V(g)$Filter[idx.cmp] <- "Keep"
	# record graph file
	graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
	tlog(4,"Updating unfiltered graph file with new \"Filtered\" vertex attribute: \"",graph.file,"\"")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	# add col to char stats table and record
	char.stats <- data$char.stats
	if(COL_FILTER %in% colnames(char.stats))
		char.stats[,COL_FILTER] <- V(g)$Filter
	else
	{	char.stats <- cbind(char.stats, V(g)$Filter)
		colnames(char.stats)[ncol(char.stats)] <- COL_FILTER
	}
	data$char.stats <- char.stats
	# update stats file
	file <- get.path.stats.corpus(object="characters",subfold="unfiltered",pref="_char_stats")
	tlog(4,"Writing character stats \"",file,"\"")
	write.csv(x=char.stats, file=paste0(file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
	
	# plot corpus stats for unfiltered chars
	plot.stats.char(
		char.stats=char.stats, 
		volume.stats=volume.stats, 
		cur.vol=NA, cur.arc=NA,
		filtered=FALSE
	)
	# plot corpus stats for filtered chars
	plot.stats.char(
		char.stats=char.stats, 
		volume.stats=volume.stats, 
		cur.vol=NA, cur.arc=NA,
		filtered=TRUE
	)
	
	res <- list(
		g=g, g.filt=g.cmp,
		char.stats=char.stats
	)
	return(res)
}




###############################################################################
# Main function for the extraction of graphs based on interaction tables.
#
# data: preprocessed data.
#
# returns: the updated data.
###############################################################################
extract.static.graphs.base <- function(data)
{	tlog(1,"Extracting static graphs")
	inter.df <- data$inter.df
	page.stats <- data$page.stats
	char.stats <- data$char.stats
	scene.stats <- data$scene.stats
	scene.chars <- data$scene.chars
	volume.stats <- data$volume.stats
	
	# extract the full scene-based static graph
	g <- extract.static.graph.scenes(
		inter.df=inter.df,
		char.stats=char.stats,
		scene.stats=scene.stats, scene.chars=scene.chars,
		volume.stats=volume.stats
	)
	# record to file
	graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
	tlog(2,"Record to file \"",graph.file,"\"")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	# extract the filtered version and record
	tmp <- extract.static.graph.filtered(
		g=g, 
		char.stats=char.stats, 
		volume.stats=volume.stats
	)
	g <- tmp$g
	g.filt <- tmp$g.filt
	char.stats <- tmp$char.stats
	data$char.stats <- char.stats
	
	# extract the graph of each specific narrative arc
	arc.titles <- unique(volume.stats[,COL_ARC])
	arc.nbr <- length(arc.titles)
	for(a in 1:arc.nbr)
	{	tlog(2,"Extracting graph for narrative arc ",a,"/",arc.nbr)
		
		# extract the unfiltered scene-based static graph for the arc
		g <- extract.static.graph.scenes(
			inter.df=inter.df,
			char.stats=char.stats, 
			scene.stats=scene.stats, scene.chars=scene.chars,
			volume.stats=volume.stats, 
			arc=a
		)
		# record to file
		graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", arc=a,  filtered=FALSE, pref="graph", ext=".graphml")
		tlog(2,"Record to file \"",graph.file,"\"")
		write_graph(graph=g, file=graph.file, format="graphml")
		
		# extract the filtered version
		idx.remove <- which(V(g)$Filter=="Discard" | degree(g)==0)
		g.filtr <- delete_vertices(graph=g, v=idx.remove)
		# record to file
		graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", arc=a, filtered=TRUE, pref="graph", ext=".graphml")
		tlog(3,"Record to file \"",graph.file,"\"")
		write_graph(graph=g.filtr, file=graph.file, format="graphml")
	}
	
	# extract the graph of each volume
	volume.nbr <- nrow(volume.stats)
	for(v in 1:volume.nbr)
	{	tlog(2,"Extracting graph for volume id ",v,"/",volume.nbr)
		vname <- paste0(v,"_",volume.stats[v,COL_VOLUME])
		
		# extract the unfiltered scene-based static graph for the volume
		g <- extract.static.graph.scenes(
			inter.df=data$inter.df,
			char.stats=char.stats, 
			scene.stats=scene.stats, scene.chars=scene.chars,
			volume.stats=volume.stats, 
			vol=v
		)
		# record to file
		graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", vol=vname, filtered=FALSE, pref="graph", ext=".graphml")
		tlog(2,"Record to file \"",graph.file,"\"")
		write_graph(graph=g, file=graph.file, format="graphml")
		
		# extract the filtered version
		idx.remove <- which(V(g)$Filter=="Discard" | degree(g)==0)
		g.filtr <- delete_vertices(graph=g, v=idx.remove)
		# record to file
		graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", vol=vname, filtered=TRUE, pref="graph", ext=".graphml")
		tlog(3,"Record to file \"",graph.file,"\"")
		write_graph(graph=g.filtr, file=graph.file, format="graphml")
	}
	
	tlog(1,"Extraction of the static graphs complete")
	return(data)
}
