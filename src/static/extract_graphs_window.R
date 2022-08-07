# This script contains functions related to the extraction of character
# networks based on raw interaction tables.
# 
# Vincent Labatut
# 11/2018
#
# source("src/static/extract_graphs_window.R")
###############################################################################




###############################################################################
# Extracts a static graph based on a list of pairwise interactions, using the
# specified window size and overlap, both expressed in number of panels.
#
# inter.df: dataframe containing the pairwise interactions.
# char.stats: table describing all the characters occurring in the BD series.
# window.size: size of the time window (expressed in panels).
# overlap: how much consecutive windows overlap (expressed in panels). Must be strictly
#          smaller than window.size.
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
	cnt <- 0
	while(!covered)
	{	window.end <- min(window.end,last.panel)
		covered <- window.end==last.panel
		cnt <- cnt + 1
		if(cnt %% 1000 == 0)
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
	
	# init the unfiltered graph
	tlog(3,"Creating unfiltered graph")
	g.unf <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.stats)
	# write to file
	graph.file <- get.path.data.graph(mode="panel.window", net.type="static", window.size=window.size, overlap=overlap, filtered=FALSE, pref="graph", ext=".graphml")
	tlog(3,"Recording graph in file '",graph.file,"'")
	write_graph(graph=g.unf, file=graph.file, format="graphml")
	
	# remove chars to get unfiltered graph
	filt.names <- char.stats[char.stats[,COL_FILTER]=="Discard",COL_NAME]
	idx <- match(filt.names, V(g.unf)$name)
	idx <- idx[!is.na(idx)]
	tlog(3,"Creating filtered graph: discarding ",length(idx),"/",length(filt.names)," minor characters")
	g.filt <- delete_vertices(g.unf, idx)
	# write to file
	graph.file <- get.path.data.graph(mode="panel.window", net.type="static", window.size=window.size, overlap=overlap, filtered=TRUE, pref="graph", ext=".graphml")
	tlog(3,"Recording graph in file '",graph.file,"'")
	write_graph(graph=g.filt, file=graph.file, format="graphml")

	tlog(2,"Extraction of the panel window-based static graph completed for parameters window.size=",window.size," and overlap=",overlap)
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
	cnt <- 0
	while(!covered)
	{	window.end <- min(window.end,last.page)
		covered <- window.end==last.page
		msg <- paste0("Current window: [",window.start,",",window.end,"]")
		# compute start/end in terms of panels
		start.panel <- page.stats[window.start,COL_PANEL_START_ID]
		end.panel <- page.stats[window.end,COL_PANEL_START_ID] + page.stats[window.end,COL_PANELS] - 1
		cnt <- cnt + 1
		if(cnt %% 100 == 0)
			tlog(3,"Current window: [",window.start,",",window.end,"]")
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
	
	# init the unfiltered graph
	tlog(3,"Creating unfiltered graph")
	g.unf <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.stats)
	# write to file
	graph.file <- get.path.data.graph(mode="page.window", net.type="static", window.size=window.size, overlap=overlap, filtered=FALSE, pref="graph", ext=".graphml")
	tlog(3,"Recording graph in file '",graph.file,"'")
	write_graph(graph=g.unf, file=graph.file, format="graphml")
	
	# remove chars to get unfiltered graph
	filt.names <- char.stats[char.stats[,COL_FILTER]=="Discard",COL_NAME]
	idx <- match(filt.names, V(g.unf)$name)
	idx <- idx[!is.na(idx)]
	tlog(3,"Creating filtered graph: discarding ",length(idx),"/",length(filt.names)," minor characters")
	g.filt <- delete_vertices(g.unf, idx)
	# write to file
	graph.file <- get.path.data.graph(mode="page.window", net.type="static", window.size=window.size, overlap=overlap, filtered=TRUE, pref="graph", ext=".graphml")
	tlog(3,"Recording graph in file '",graph.file,"'")
	write_graph(graph=g.filt, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the page window-based static graph completed for parameters window.size=",window.size," and overlap=",overlap)
}




###############################################################################
# Main function for the extraction of graphs based on interaction tables.
#
# data: preprocessed data.
# panel.params: panel-related parameters.
# page.params: page-related parameters.
###############################################################################
extract.static.graphs.window <- function(data, panel.params, page.params)
{	tlog(1,"Extracting static graphs")
	
	# retrieve data
	inter.df <- data$inter.df
	page.stats <- data$page.stats
	char.stats <- data$char.stats
	volume.stats <- data$volume.stats
	
	# retrieve parameters
	panel.window.sizes <- panel.params$window.sizes
	panel.overlaps <- panel.params$overlaps
	page.window.sizes <- page.params$window.sizes
	page.overlaps <- page.params$overlaps
	
	# extract the panel window-based static graphs
#	future_sapply(1:length(panel.window.sizes), function(i)
	for(i in 1:length(panel.window.sizes))
	{	panel.window.size <- panel.window.sizes[i]
		for(panel.overlap in panel.overlaps[[i]])
			extract.static.graph.panel.window(
				inter.df=inter.df, 
				char.stats=char.stats, 
				window.size=panel.window.size, overlap=panel.overlap)
	}#)
	
	# extract the page window-based static graphs
#	future_sapply(1:length(page.window.sizes), function(i)
	for(i in 1:length(page.window.sizes))
	{	page.window.size <- page.window.sizes[i]
		for(page.overlap in page.overlaps[[i]])
			extract.static.graph.page.window(
				inter.df=inter.df, 
				char.stats=char.stats, 
				page.stats=page.stats, 
				window.size=page.window.size, overlap=page.overlap)
	}#)
	
	tlog(1,"Extraction of the static graphs complete")
}
