# This script contains functions related to the extraction of character
# networks based on raw interaction tables.
# 
# Vincent Labatut
# 11/2018
###############################################################################




###############################################################################
# Extracts a static graph based on a list of pairwise interactions, using the
# segment as the time unit, without overlap. Nodes are characters, and links
# represents them (inter-)acting during the same segment.
#
# inter.df: dataframe containing the pairwise interactions (columns 
#			COL_INTER_FROM_CHAR and COL_INTER_TO_CHAR) and their time of 
#           occurrence (columns COL_INTER_START_PANEL_ID and COL_INTER_END_PANEL_ID).
#
# returns: the corresponding static graph. It contains several edge weigths:
#		   - Occurrences: number of interactions between the concerned nodes.
#		   - Duration: total duration (in number of panels).
###############################################################################
extract.static.graph.from.segments <- function(inter.df)
{	tlog(2,"Extracting the segment-based static graph")
	
	# init the dataframe
	static.df <- data.frame(
			From=character(), To=character(), 
			Occurrences=integer(), Duration=integer(), 
			stringsAsFactors=FALSE)
	Encoding(static.df$From) <- "UTF-8"
	Encoding(static.df$To) <- "UTF-8"
	cn <- c(COL_INTER_FROM_CHAR, COL_INTER_TO_CHAR, COL_INTER_OCCURRENCES, COL_INTER_DURATION)
	colnames(static.df) <- cn
	
	# build the edgelist by considering each line (ie interaction) in the dataframe
	for(i in 1:nrow(inter.df))
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
	}
	
	static.df <- static.df[order(static.df[,COL_INTER_FROM_CHAR],static.df[,COL_INTER_TO_CHAR]),]
#	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	graph.file <- get.graphname.static(mode="segments")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the segment-based static graph completed")
	return(g)
}


###############################################################################
# Extracts a static graph based on a list of pairwise interactions, using the
# specified window size and overlap, both expressed in number of panels.
#
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
extract.static.graph.from.panel.window <- function(inter.df, window.size=10, overlap=2)
{	tlog(2,"Extracting the panel window-based static graph")
	tlog(3,"For parameters window.size=",window.size," and overlap=",overlap)
	
	# check the overlap parameter
	if(overlap>=window.size)
		stop("ERROR: overlap parameter must be smaller than or equal to window.size")
	
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
		# segments intersecting the window
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
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	graph.file <- get.graphname.static(mode="panel.window", window.size=window.size, overlap=overlap)
	write_graph(graph=g, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the panel window-based static graph completed")
	return(g)
}


###############################################################################
# Extracts a static graph based on a list of pairwise interactions, using the
# specified window size and overlap, both expressed in number of pages.
#
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
extract.static.graph.from.page.window <- function(inter.df, page.info, window.size=2, overlap=1)
{	tlog(2,"Extracting the page window-based static graph")
	tlog(3,"For parameters window.size=",window.size," and overlap=",overlap)
	
	# check the overlap parameter
	if(overlap>=window.size)
		stop("ERROR: overlap must be smaller than window.size")
	
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
		# segments intersecting the window
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
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	graph.file <- get.graphname.static(mode="page.window", window.size=window.size, overlap=overlap)
	write_graph(graph=g, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the page window-based static graph completed")
	return(g)
}




###############################################################################
# Main function for the extraction of graphs based on interaction tables.
#
# data: raw data, read from the original files.
# panel.window.sizes: values for this parameter
# panel.overlaps: values for this parameter, specified for of the above parameter values.
# page.window.sizes: same for page-based windows instead of panel-based.
# page.overlaps: same.
###############################################################################
extract.static.graphs <- function(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Extracting static graphs")
	# extract the segment-based static graph
	g <- extract.static.graph.from.segments(data$inter.df)
	#plot(g, layout=layout_with_fr(g))
	
	# extract the panel window-based static graphs
#	for(i in 1:length(panel.window.sizes))
	foreach(i=1:length(panel.window.sizes)) %dopar% 
	{	source("src/define_imports.R")
		
		window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
			g <- extract.static.graph.from.panel.window(data$inter.df, window.size=window.size, overlap=overlap)
	}
	
	# extract the page window-based static graphs
#	for(i in 1:length(page.window.sizes))
	foreach(i=1:length(page.window.sizes)) %dopar% 
	{	source("src/define_imports.R")
		
		window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
			g <- extract.static.graph.from.page.window(data$inter.df, data$page.info, window.size=window.size, overlap=overlap)
	}

	tlog(1,"Extraction of the static graphs complete")
}
