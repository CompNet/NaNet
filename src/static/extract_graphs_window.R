# This script contains functions related to the extraction of character
# networks based on raw interaction tables.
# 
# Vincent Labatut
# 11/2018
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
	graph.file <- get.path.data.graph(mode="panel.window", net.type="static", window.size=window.size, overlap=overlap, filtered=FALSE, pref="graph", ext=".graphml")
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
	graph.file <- get.path.data.graph(mode="page.window", net.type="static", window.size=window.size, overlap=overlap, filtered=FALSE, pref="graph", ext=".graphml")
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
extract.static.graphs.window <- function(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Extracting static graphs")
	inter.df <- data$inter.df
	page.stats <- data$page.stats
	char.stats <- data$char.stats
	volume.stats <- data$volume.stats
	
	# extract the panel window-based static graphs
	future_sapply(1:length(panel.window.sizes), function(i)
	#for(i in 1:length(panel.window.sizes))
	{	window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
			g <- extract.static.graph.panel.window(
				inter.df=inter.df, 
				char.stats=char.stats, 
				window.size=window.size, overlap=overlap)
	})
	
	# extract the page window-based static graphs
	future_sapply(1:length(page.window.sizes), function(i)
	#for(i in 1:length(page.window.sizes))
	{	window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
			g <- extract.static.graph.page.window(
				inter.df=data$inter.df, 
				char.stats=data$char.stats, 
				page.stats=data$page.stats, 
				window.size=window.size, overlap=overlap)
	})
	
	tlog(1,"Extraction of the static graphs complete")
	return(data)
}
