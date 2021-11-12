# This script contains functions related to the extraction of character
# networks based on raw interaction tables.
# 
# Vincent Labatut
# 11/2018
###############################################################################




###############################################################################
# Extracts a static graph based on a list of pairwise interactions, using the
# scene as the time unit, without overlap. Nodes are characters, and links
# represents them (inter-)acting during the same scene.
#
# volume.info: table describing all the volumes constituting the BD series.
# char.info: table describing all the characters occurring in the BD series.
# page.info: table describing all the pages constituting the BD series.
# inter.df: dataframe containing the pairwise interactions (columns 
#			COL_INTER_FROM_CHAR and COL_INTER_TO_CHAR) and their time of 
#           occurrence (columns COL_INTER_START_PANEL_ID and COL_INTER_END_PANEL_ID).
# stats.scenes: scene statistics, only needed if ret.seq is TRUE.
# vol: volume of interest (optional, and ignored if ret.seq is TRUE).
# ret.set: whether to return the full sequence of incremental graphs (longer).
#
# returns: the corresponding static graph. It contains several edge weigths:
#		   - Occurrences: number of interactions between the concerned nodes.
#		   - Duration: total duration (in number of panels).
#		   If ret.set==TRUE, then the function returns a list of graphs.
###############################################################################
extract.static.graph.scenes <- function(volume.info, char.info, page.info, inter.df, stats.scenes, vol=NA, ret.seq=FALSE)
{	tlog(2,"Extracting the scene-based static graph")
	g <- make_empty_graph(0,directed=FALSE)
	res <- list()
	
	# init the dataframe
	static.df <- data.frame(
			From=character(), To=character(), 
			Occurrences=integer(), Duration=integer(), 
			stringsAsFactors=FALSE)
	Encoding(static.df$From) <- "UTF-8"
	Encoding(static.df$To) <- "UTF-8"
	cn <- c(COL_INTER_FROM_CHAR, COL_INTER_TO_CHAR, COL_INTER_OCCURRENCES, COL_INTER_DURATION)
	colnames(static.df) <- cn
	
	# possibly filter interactions
	if(is.na(vol))
	{	# possibly order interactions by story order (not publication order)
#		if(ret.seq)
#		{	pg.starts <- page.info[,COL_PAGES_START_PANEL_ID]
#			pg.ends <- page.info[,COL_PAGES_START_PANEL_ID] + page.info[,COL_PAGES_PANELS] - 1
#			page.ids <- future_sapply(1:nrow(inter.df), function(r) which(pg.starts<=inter.df[r,COL_INTER_START_PANEL_ID] & pg.ends>=inter.df[r,COL_INTER_START_PANEL_ID]))
#			vol.ranks <- volume.info[page.info[page.ids, COL_PAGES_VOLUME_ID], COL_VOLS_RANK]
#			inter.df <- inter.df[order(vol.ranks, inter.df[,COL_INTER_START_PANEL_ID]),]
#		}
		# get interactions numbers
		is <- 1:nrow(inter.df)
	}
	else
	{	idx.pg <- which(page.info[,COL_PAGES_VOLUME]==vol)
		start.pn <- page.info[idx.pg[1],COL_PAGES_START_PANEL_ID]
		#end.pn <- page.info[idx.pg[length(idx.pg)]+1,COL_PAGES_START_PANEL_ID] - 1
		end.pn <- page.info[idx.pg[length(idx.pg)],COL_PAGES_START_PANEL_ID]
		idx.pn <- seq(start.pn, end.pn)
		#is <- which(stats.scenes[,COL_STATS_VOLUME]==vol)
		is <- which(inter.df[,COL_INTER_START_PANEL_ID] %in% idx.pn)
	}
		
	# build the edgelist by considering each line (i.e. interaction) in the dataframe
	for(i in is)
	{	# get the characters
		from.char <- inter.df[i,COL_INTER_FROM_CHAR]
		to.char <- inter.df[i,COL_INTER_TO_CHAR]
		# get the corresponding row in the new (integrated) dataframe
		index <- which(static.df[,COL_INTER_FROM_CHAR]==from.char & static.df[,COL_INTER_TO_CHAR]==to.char)
		# compute the number of panels in the sequence
		length <- inter.df[i,COL_INTER_END_PANEL_ID] - inter.df[i,COL_INTER_START_PANEL_ID] + 1
		# update the integrated dataframe
		if(length(index)==0)
		{	# insert the couple of characters (never met before)
			tmp.df <- data.frame(From=from.char, To=to.char, Occurrences=1, Duration=length, stringsAsFactors=FALSE)
			colnames(tmp.df) <- cn
			static.df <- rbind(static.df, tmp.df)
		}
		else
		{	# update the couple of characters (already inserted vefore)
			static.df[index, COL_INTER_OCCURRENCES] <- static.df[index, COL_INTER_OCCURRENCES] + 1
			static.df[index, COL_INTER_DURATION] <- static.df[index, COL_INTER_DURATION] + length
		}
		
		# if graph sequence required
		if(ret.seq)
		{	tlog(4,"Adding the graph to the sequence (",i,"/",length(is),")")
			static.df <- static.df[order(static.df[,COL_INTER_FROM_CHAR],static.df[,COL_INTER_TO_CHAR]),]
			idx <- which(char.info[,COL_CHAR_NAME] %in% c(cbind(static.df[,COL_INTER_FROM_CHAR],static.df[,COL_INTER_TO_CHAR])))
			g1 <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.info[idx,])
			if(gsize(g1)!=gsize(g) || gorder(g1)!=gorder(g))
			{	g <- g1
				res[[length(res)+1]] <- g
			}
		}
	}
	
	static.df <- static.df[order(static.df[,COL_INTER_FROM_CHAR],static.df[,COL_INTER_TO_CHAR]),]
	#print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.info)
	# write to file
	graph.file <- get.path.graph.file(mode="scenes", vol=vol)
	write_graph(graph=g, file=graph.file, format="graphml")
	#plot(g, layout=layout_with_fr(g))
	
	# set up result variable
	if(ret.seq)
	{	if(gsize(g1)!=gsize(g) || gorder(g1)!=gorder(g))
			res[[length(res)+1]] <- g
		msg <- paste0("returning a series of ",length(res)," graphs")
	}
	else
	{	res <- g
		msg <- "returning a single graph"
	}
	
	tlog(2,"Extraction of the scene-based static graph completed, ",msg)
	return(res)
}


###############################################################################
# Extracts a static graph based on a list of pairwise interactions, using the
# specified window size and overlap, both expressed in number of panels.
#
# char.info: table describing all the characters occurring in the BD series.
# inter.df: dataframe containing the pairwise interactions (columns 
#			COL_INTER_FROM_CHAR and COL_INTER_TO_CHAR) and their time of 
#           occurrence (columns COL_INTER_START_PANEL_ID and COL_INTER_END_PANEL_ID).
# window.size: size of the time window (expressed in panels).
# overlap: how much consecutive windows overlap (expressed in panels). Must be strictly
#          smaller than window.size.
#
# returns: the corresponding static graph, whose edge weights correspond to the
#		   number of co-occurrences between the concerned nodes.
###############################################################################
extract.static.graph.panel.window <- function(char.info, inter.df, window.size=10, overlap=2)
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
	cn <- c(COL_INTER_FROM_CHAR, COL_INTER_TO_CHAR, COL_INTER_OCCURRENCES)
	colnames(static.df) <- cn
	
	# compute the co-occurrences
	last.panel <- max(inter.df[,COL_INTER_END_PANEL_ID])
	window.start <- 1
	window.end <- window.size
	covered <- FALSE
	while(!covered)
	{	window.end <- min(window.end,last.panel)
		covered <- window.end==last.panel
		tlog(3,"Current window: [",window.start,",",window.end,"]")
		# scenes intersecting the window
		idx <- which(!(inter.df[,COL_INTER_END_PANEL_ID]<window.start | inter.df[,COL_INTER_START_PANEL_ID]>window.end))
		# get all concerned chars
		chars <- sort(unique(c(as.matrix(inter.df[idx,c(COL_INTER_FROM_CHAR,COL_INTER_TO_CHAR)]))))
		if(length(chars)>1)
		{	pairs <- t(combn(x=chars,m=2))
			# update dataframe
			for(i in 1:nrow(pairs))
			{	from.char <- pairs[i,1]
				to.char <- pairs[i,2]
				index <- which(static.df[,COL_INTER_FROM_CHAR]==from.char & static.df[,COL_INTER_TO_CHAR]==to.char)
				if(length(index)==0)
				{	tmp.df <- data.frame(From=from.char, To=to.char, Occurrences=1, stringsAsFactors=FALSE)
					colnames(tmp.df) <- cn
					static.df <- rbind(static.df, tmp.df)
				}
				else
					static.df[index, COL_INTER_OCCURRENCES] <- static.df[index, COL_INTER_OCCURRENCES] + 1
			}
		}
#		print(chars)
		# update window
		window.start <- window.start + window.size - overlap
		window.end <- window.start + window.size - 1
	}
	
	static.df <- static.df[order(static.df[,COL_INTER_FROM_CHAR],static.df[,COL_INTER_TO_CHAR]),]
#	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.info)
	# write to file
	graph.file <- get.path.graph.file(mode="panel.window", window.size=window.size, overlap=overlap)
	write_graph(graph=g, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the panel window-based static graph completed for parameters window.size=",window.size," and overlap=",overlap)
	return(g)
}


###############################################################################
# Extracts a static graph based on a list of pairwise interactions, using the
# specified window size and overlap, both expressed in number of pages.
#
# char.info: table describing all the characters occurring in the BD series.
# inter.df: dataframe containing the pairwise interactions (columns 
#			COL_INTER_FROM_CHAR and COL_INTER_TO_CHAR) and their time of 
#           occurrence (columns COL_INTER_START_PANEL_ID and COL_INTER_END_PANEL_ID).
# page.info: dataframe containing the number of panels in the pages.
# window.size: size of the time window (expressed in pages).
# overlap: how much consecutive windows overlap (expressed in pages). Must be strictly
#          smaller than window.size.
#
# returns: the corresponding static graph, whose edge weights correspond to the
#		   number of co-occurrences between the concerned nodes.
###############################################################################
extract.static.graph.page.window <- function(char.info, inter.df, page.info, window.size=2, overlap=1)
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
	cn <- c(COL_INTER_FROM_CHAR, COL_INTER_TO_CHAR, COL_INTER_OCCURRENCES)
	colnames(static.df) <- cn
	
	# compute the co-occurrences
	last.page <- nrow(page.info)
	window.start <- 1
	window.end <- window.size
	covered <- FALSE
	while(!covered)
	{	window.end <- min(window.end,last.page)
		covered <- window.end==last.page
		msg <- paste0("Current window: [",window.start,",",window.end,"]")
		# compute start/end in terms of panels
		start.panel <- page.info[window.start,COL_INTER_START_PANEL_ID]
		end.panel <- page.info[window.end,COL_INTER_START_PANEL_ID] + page.info[window.end,COL_PAGES_PANELS] - 1
		tlog(3,paste0(msg, " ie [",start.panel,",",end.panel,"]"))
		# scenes intersecting the window
		idx <- which(!(inter.df[,COL_INTER_END_PANEL_ID]<start.panel | inter.df[,COL_INTER_START_PANEL_ID]>end.panel))
		# get all concerned chars
		chars <- sort(unique(c(as.matrix(inter.df[idx,c(COL_INTER_FROM_CHAR,COL_INTER_TO_CHAR)]))))
#		print(chars)
		if(length(chars)>1)
		{	pairs <- t(combn(x=chars,m=2))
			# update dataframe
			for(i in 1:nrow(pairs))
			{	from.char <- pairs[i,1]
				to.char <- pairs[i,2]
				index <- which(static.df[,COL_INTER_FROM_CHAR]==from.char & static.df[,COL_INTER_TO_CHAR]==to.char)
				if(length(index)==0)
				{	tmp.df <- data.frame(From=from.char, To=to.char, Occurrences=1, stringsAsFactors=FALSE)
					colnames(tmp.df) <- cn
					static.df <- rbind(static.df, tmp.df)
				}
				else
					static.df[index, COL_INTER_OCCURRENCES] <- static.df[index, COL_INTER_OCCURRENCES] + 1
			}
		}
		# update window
		window.start <- window.start + window.size - overlap
		window.end <- window.start + window.size - 1
	}
	
	static.df <- static.df[order(static.df[,COL_INTER_FROM_CHAR],static.df[,COL_INTER_TO_CHAR]),]
#	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.info)
	# write to file
	graph.file <- get.path.graph.file(mode="page.window", window.size=window.size, overlap=overlap)
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
###############################################################################
extract.static.graphs <- function(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Extracting static graphs")
	# extract the scene-based static graph
	g <- extract.static.graph.scenes(
			volume.info=data$volume.info, char.info=data$char.info, 
			page.info=data$page.info, inter.df=data$inter.df)
	
	# extract the filtered version
	crit <- degree(g)<=1 | V(g)$Frequency<=3
	idx.remove <- which(crit)
	idx.keep <- which(!crit)
	g.filtr <- delete_vertices(graph=g, v=idx.remove)
	tmp <- get.largest.component(g.filtr, indices=TRUE)
	idx.cmp <- idx.keep[tmp$indices]
	g.cmp <- tmp$comp
	# write to file
	graph.file <- get.path.graph.file(mode="scenes", filtered=TRUE)
	write_graph(graph=g.cmp, file=graph.file, format="graphml")
	# update main file
	V(g)$Filtered <- rep(TRUE,gorder(g))
	V(g)$Filtered[idx.cmp] <- FALSE
	graph.file <- get.path.graph.file(mode="scenes")
	write_graph(graph=g, file=graph.file, format="graphml")
	# add col to char info
	char.info <- data$char.info
	char.info <- cbind(char.info, V(g)$Filtered)
	colnames(char.info)[ncol(char.info)] <- COL_CHAR_FILTERED
	data$char.info <- char.info
	
	# extract the panel window-based static graphs
	future_sapply(1:length(panel.window.sizes), function(i)
	#for(i in 1:length(panel.window.sizes))
	{	window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
			g <- extract.static.graph.panel.window(
					char.info=data$char.info, inter.df=data$inter.df, 
					window.size=window.size, overlap=overlap)
	})
	
	# extract the page window-based static graphs
	future_sapply(1:length(page.window.sizes), function(i)
	#for(i in 1:length(page.window.sizes))
	{	window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
			g <- extract.static.graph.page.window(
					char.info=data$char.info, inter.df=data$inter.df, page.info=data$page.info, 
					window.size=window.size, overlap=overlap)
	})
	
	tlog(1,"Extraction of the static graphs complete")
}
