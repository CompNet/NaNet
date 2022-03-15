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
extract.static.graph.scenes <- function(inter.df, char.stats, volume.stats, vol=NA, arc=NA, ret.seq=FALSE, pub.order=TRUE)
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
	}
	else if(!is.na(vol))
	{	vname <- paste0(vol,"_",volume.stats[vol,COL_VOLUME])
		is <- which(inter.df[,COL_VOLUME_ID]==vol)
	}
	else
	{	# order interactions by publication order
		if(pub.order)
			is <- 1:nrow(inter.df)
		# or by story order
		else
		{	vol.ranks <- volume.stats[inter.df[,COL_VOLUME_ID],COL_RANK]
			scene.ranks <- inter.df[,COL_SCENE_ID]
			is <- order(vol.ranks, scene.ranks)
		}
	}
	
	# build the edgelist by considering each line (i.e. interaction) in the dataframe
	prev.scene <- NA
	for(i in is)
	{	# get the current scene id
		cur.scene <- inter.df[i,COL_SCENE_ID]
		
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
			{	for(s in (prev.scene+1):(cur.scene-1))
				{	g <- res[[s-1]]
					g <- set_graph_attr(graph=g, name="Scene", value=s)
					res[[s]] <- g
				}
			}
			# build and add current graph
			static.df <- static.df[order(static.df[,COL_CHAR_FROM],static.df[,COL_CHAR_TO]),]
			idx <- which(char.stats[,COL_NAME] %in% c(cbind(static.df[,COL_CHAR_FROM],static.df[,COL_CHAR_TO])))
			g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.stats[idx,])
			g$Scene <- cur.scene
			res[[cur.scene]] <- g
		}
		
		prev.scene <- cur.scene
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
	tt <- table(degree(g)>1, V(g)$Frequency>3)
	ttt <- rbind(cbind(tt,rowSums(tt)), c(colSums(tt),sum(tt)))
	colnames(ttt) <- c("Degree<1","Degree>=1","Total")
	rownames(ttt) <- c("Frequency<1","Frequency>=1","Total")
	print(ttt)
	file <- get.path.graph.file(mode="scenes", filtered=FALSE, desc="filtering_criteria", ext=".csv")
	write.csv(x=ttt, file=file, row.names=TRUE)
	
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
	graph.file <- get.path.graph.file(mode="scenes", filtered=TRUE, desc="static", ext=".graphml")
	tlog(4,"Recording filtered graph in \"",graph.file,"\"")
	write_graph(graph=g.cmp, file=graph.file, format="graphml")
	
	# add new attribute to unfiltered graph
	V(g)$Filtered <- rep(TRUE,gorder(g))
	V(g)$Filtered[idx.cmp] <- FALSE
	# record graph file
	graph.file <- get.path.graph.file(mode="scenes", filtered=FALSE, desc="static", ext=".graphml")
	tlog(4,"Updating unfiltered graph file with new \"Filtered\" vertex attribute: \"",graph.file,"\"")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	# add col to char stats table and record
	char.stats <- data$char.stats
	if(COL_FILTERED %in% colnames(char.stats))
		char.stats[,COL_FILTERED] <- V(g)$Filtered
	else
	{	char.stats <- cbind(char.stats, V(g)$Filtered)
		colnames(char.stats)[ncol(char.stats)] <- COL_FILTERED
	}
	data$char.stats <- char.stats
	# update stats file
	file <- get.path.stat.corpus(object="characters",subfold="unfiltered",desc="_char_stats")
	tlog(4,"Writing character stats \"",file,"\"")
	write.csv(x=char.stats, file=paste0(file,".csv"), row.names=FALSE)
	
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
# Extracts a static graph based on a list of pairwise interactions, using the
# specified window size and overlap, both expressed in number of panels.
#
# inter.df: dataframe containing the pairwise interactions.
# char.stats: table describing all the characters occurring in the BD series.
# window.size: size of the time window (expressed in panels).
# overlap: how much consecutive windows overlap (expressed in panels). Must be strictly
#          smaller than window.size.
#
# returns: the corresponding static graph, whose edge weights correspond to the
#		   number of co-occurrences between the concerned nodes.
###############################################################################
extract.static.graph.panel.window <- function(inter.df, char.stats, window.size=10, overlap=2)
{	tlog(2,"Extracting the panel window-based static graph for parameters window.size=",window.size," and overlap=",overlap)
	
	# check the overlap parameter
	if(overlap>=window.size)
	{	msg <- paste0("ERROR: overlap parameter must be smaller than or equal to window.size: window.size=",window.size,", overlap=",overlap)
		tlog(4,msg)
		stop(msg)
	}
	
	# init the dataframe
	static.df <- data.frame(
		From=character(), To=character(), 
		Occurrences=integer(), 
		stringsAsFactors=FALSE)
	Encoding(static.df$From) <- "UTF-8"
	Encoding(static.df$To) <- "UTF-8"
	cn <- c(COL_CHAR_FROM, COL_CHAR_TO, COL_OCCURRENCES)
	colnames(static.df) <- cn
	
	# compute the co-occurrences
	last.panel <- max(inter.df[,COL_PANEL_END_ID])
	window.start <- 1
	window.end <- window.size
	covered <- FALSE
	while(!covered)
	{	window.end <- min(window.end,last.panel)
		covered <- window.end==last.panel
		tlog(3,"Current window: [",window.start,",",window.end,"]")
		# scenes intersecting the window
		idx <- which(!(inter.df[,COL_PANEL_END_ID]<window.start | inter.df[,COL_PANEL_START_ID]>window.end))
		# get all concerned chars
		chars <- sort(unique(c(as.matrix(inter.df[idx,c(COL_CHAR_FROM,COL_CHAR_TO)]))))
		if(length(chars)>1)
		{	pairs <- t(combn(x=chars,m=2))
			# update dataframe
			for(i in 1:nrow(pairs))
			{	from.char <- pairs[i,1]
				to.char <- pairs[i,2]
				index <- which(static.df[,COL_CHAR_FROM]==from.char & static.df[,COL_CHAR_TO]==to.char)
				if(length(index)==0)
				{	tmp.df <- data.frame(From=from.char, To=to.char, Occurrences=1, stringsAsFactors=FALSE)
					colnames(tmp.df) <- cn
					static.df <- rbind(static.df, tmp.df)
				}
				else
					static.df[index, COL_OCCURRENCES] <- static.df[index, COL_OCCURRENCES] + 1
			}
		}
#		print(chars)
		# update window
		window.start <- window.start + window.size - overlap
		window.end <- window.start + window.size - 1
	}
	
	static.df <- static.df[order(static.df[,COL_CHAR_FROM],static.df[,COL_CHAR_TO]),]
#	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.stats)
	# write to file
	graph.file <- get.path.graph.file(mode="panel.window", window.size=window.size, overlap=overlap, filtered=FALSE, desc="static", ext=".graphml")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the panel window-based static graph completed for parameters window.size=",window.size," and overlap=",overlap)
	return(g)
}




###############################################################################
# Extracts a static graph based on a list of pairwise interactions, using the
# specified window size and overlap, both expressed in number of pages.
#
# char.stats: table describing all the characters occurring in the BD series.
# inter.df: dataframe containing the pairwise interactions.
# page.stats: dataframe containing the number of panels in the pages.
# window.size: size of the time window (expressed in pages).
# overlap: how much consecutive windows overlap (expressed in pages). Must be strictly
#          smaller than window.size.
#
# returns: the corresponding static graph, whose edge weights correspond to the
#		   number of co-occurrences between the concerned nodes.
###############################################################################
extract.static.graph.page.window <- function(inter.df, char.stats, page.stats, window.size=2, overlap=1)
{	tlog(2,"Extracting the page window-based static graph for parameters window.size=",window.size," and overlap=",overlap)
	
	# check the overlap parameter
	if(overlap>=window.size)
	{	msg <- paste0("ERROR: overlap must be smaller than window.size: window.size=",window.size,", overlap=",overlap)
		tlog(4,msg)
		stop(msg)
	}
	
	# init the dataframe
	static.df <- data.frame(
			From=character(), To=character(), 
			Occurrences=integer(), 
			stringsAsFactors=FALSE)
	Encoding(static.df$From) <- "UTF-8"
	Encoding(static.df$To) <- "UTF-8"
	cn <- c(COL_CHAR_FROM, COL_CHAR_TO, COL_OCCURRENCES)
	colnames(static.df) <- cn
	
	# compute the co-occurrences
	last.page <- nrow(page.stats)
	window.start <- 1
	window.end <- window.size
	covered <- FALSE
	while(!covered)
	{	window.end <- min(window.end,last.page)
		covered <- window.end==last.page
		msg <- paste0("Current window: [",window.start,",",window.end,"]")
		# compute start/end in terms of panels
		start.panel <- page.stats[window.start,COL_PANEL_START_ID]
		end.panel <- page.stats[window.end,COL_PANEL_START_ID] + page.stats[window.end,COL_PANELS] - 1
		tlog(3,paste0(msg, " ie [",start.panel,",",end.panel,"]"))
		# scenes intersecting the window
		idx <- which(!(inter.df[,COL_PANEL_END_ID]<start.panel | inter.df[,COL_PANEL_START_ID]>end.panel))
		# get all concerned chars
		chars <- sort(unique(c(as.matrix(inter.df[idx,c(COL_CHAR_FROM,COL_CHAR_TO)]))))
#		print(chars)
		if(length(chars)>1)
		{	pairs <- t(combn(x=chars,m=2))
			# update dataframe
			for(i in 1:nrow(pairs))
			{	from.char <- pairs[i,1]
				to.char <- pairs[i,2]
				index <- which(static.df[,COL_CHAR_FROM]==from.char & static.df[,COL_CHAR_TO]==to.char)
				if(length(index)==0)
				{	tmp.df <- data.frame(From=from.char, To=to.char, Occurrences=1, stringsAsFactors=FALSE)
					colnames(tmp.df) <- cn
					static.df <- rbind(static.df, tmp.df)
				}
				else
					static.df[index, COL_OCCURRENCES] <- static.df[index, COL_OCCURRENCES] + 1
			}
		}
		# update window
		window.start <- window.start + window.size - overlap
		window.end <- window.start + window.size - 1
	}
	
	static.df <- static.df[order(static.df[,COL_CHAR_FROM],static.df[,COL_CHAR_TO]),]
#	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.stats)
	# write to file
	graph.file <- get.path.graph.file(mode="page.window", window.size=window.size, overlap=overlap, filtered=FALSE, desc="static", ext=".graphml")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the page window-based static graph completed for parameters window.size=",window.size," and overlap=",overlap)
	return(g)
}




###############################################################################
# Main function for the extraction of graphs based on interaction tables.
#
# data: preprocessed data.
# panel.window.sizes: values for this parameter
# panel.overlaps: values for this parameter, specified for of the above parameter values.
# page.window.sizes: same for page-based windows instead of panel-based.
# page.overlaps: same.
#
# returns: the updated data.
###############################################################################
extract.static.graphs <- function(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Extracting static graphs")
	inter.df <- data$inter.df
	page.stats <- data$page.stats
	char.stats <- data$char.stats
	volume.stats <- data$volume.stats
	
	# extract the full scene-based static graph
	g <- extract.static.graph.scenes(
		inter.df=inter.df,
		char.stats=char.stats,
		volume.stats=volume.stats
	)
	# record to file
	graph.file <- get.path.graph.file(mode="scenes", filtered=FALSE, desc="static", ext=".graphml")
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
			volume.stats=volume.stats, 
			arc=a
		)
		# record to file
		graph.file <- get.path.graph.file(mode="scenes", arc=a,  filtered=FALSE, desc="static", ext=".graphml")
		tlog(2,"Record to file \"",graph.file,"\"")
		write_graph(graph=g, file=graph.file, format="graphml")
		
		# extract the filtered version
		idx.remove <- which(V(g)$Filtered | degree(g)==0)
		g.filtr <- delete_vertices(graph=g, v=idx.remove)
		# record to file
		graph.file <- get.path.graph.file(mode="scenes", arc=a, filtered=TRUE, desc="static", ext=".graphml")
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
			volume.stats=volume.stats, 
			vol=v
		)
		# record to file
		graph.file <- get.path.graph.file(mode="scenes", vol=vname, filtered=FALSE, desc="static", ext=".graphml")
		tlog(2,"Record to file \"",graph.file,"\"")
		write_graph(graph=g, file=graph.file, format="graphml")
		
		# extract the filtered version
		idx.remove <- which(V(g)$Filtered | degree(g)==0)
		g.filtr <- delete_vertices(graph=g, v=idx.remove)
		# record to file
		graph.file <- get.path.graph.file(mode="scenes", vol=vname, filtered=TRUE, desc="static", ext=".graphml")
		tlog(3,"Record to file \"",graph.file,"\"")
		write_graph(graph=g.filtr, file=graph.file, format="graphml")
	}
	
#	# extract the panel window-based static graphs
#	future_sapply(1:length(panel.window.sizes), function(i)
#	#for(i in 1:length(panel.window.sizes))
#	{	window.size <- panel.window.sizes[i]
#		for(overlap in panel.overlaps[[i]])
#			g <- extract.static.graph.panel.window(
#					inter.df=inter.df, 
#					char.stats=char.stats, 
#					window.size=window.size, overlap=overlap)
#	})
#	
#	# extract the page window-based static graphs
#	future_sapply(1:length(page.window.sizes), function(i)
#	#for(i in 1:length(page.window.sizes))
#	{	window.size <- page.window.sizes[i]
#		for(overlap in page.overlaps[[i]])
#			g <- extract.static.graph.page.window(
#					inter.df=data$inter.df, 
#					char.stats=data$char.stats, 
#					page.stats=data$page.stats, 
#					window.size=window.size, overlap=overlap)
#	})
	
	tlog(1,"Extraction of the static graphs complete")
	return(data)
}
