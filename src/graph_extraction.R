# TODO: Add comment
# 
# Vincent Labatut
# 11/2018
###############################################################################




###############################################################################
# Extract a static graph based on a list of pairwise interactions, using the
# segment as the time unit, without overlap.
#
# inter.df: dataframe containing the pairwise interactions (columns From, To)
#			and their time of occurrence (columns Start, End).
#
# returns: the corresponding static graph. It contains several edge weigths:
#		   - Occurrences: number of interactions between the concerned nodes.
#		   - Duration: total duration (in number of panels).
###############################################################################
extract.static.graph.from.segments <- function(inter.df)
{	# init the dataframe
	static.df <- data.frame(From=character(), To=character(), Occurrences=integer(), Duration=integer(), stringsAsFactors=FALSE)
	Encoding(static.df$From) <- "UTF-8"
	Encoding(static.df$To) <- "UTF-8"
	
	# build the edgelist
	for(i in 1:nrow(inter.df))
	{	from.char <- inter.df[i,"From"]
		to.char <- inter.df[i,"To"]
		index <- which(static.df[,"From"]==from.char & static.df[,"To"]==to.char)
		length <- inter.df[i,"End"] - inter.df[i,"Start"] + 1
		if(length(index)==0)
			static.df <- rbind(static.df, data.frame(From=from.char, To=to.char, Occurrences=1, Duration=length))
		else
		{	static.df[index, "Occurrences"] <- static.df[index, "Occurrences"] + 1
			static.df[index, "Duration"] <- static.df[index, "Duration"] + length
		}
	}
	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	dir.create(OUT_FOLDER, showWarnings=FALSE)
	graph.file <- file.path(OUT_FOLDER,"static_segments.graphml")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	return(g)
}


###############################################################################
# Extract a static graph based on a list of pairwise interactions, using the
# specified window size and overlap, both expressed in number of panels.
#
# inter.df: dataframe containing the pairwise interactions (columns From, To)
#			and their time of occurrence (columns Start, End).
# window.size: size of the time window (expressed in panels).
# overlap: how much consecutive windows overlap (expressed in panels)
#
# returns: the corresponding static graph, whose edge weights correspond to the
#		   number of co-occurrences between the concerned nodes.
###############################################################################
extract.static.graph.from.panel.window <- function(inter.df, window.size=10, overlap=2)
{	# check the overlap parameter
	if(overlap>=window.size)
		stop("ERROR: overlap must be smaller than window.size")
	
	# init the dataframe
	static.df <- data.frame(From=character(), To=character(), Occurrences=integer(), stringsAsFactors=FALSE)
	Encoding(static.df$From) <- "UTF-8"
	Encoding(static.df$To) <- "UTF-8"
	
	# compute the co-occurrences
	last.panel <- max(inter.df[,"End"])
	window.start <- 1
	window.end <- window.size
	covered <- FALSE
	while(!covered)
	{	window.end <- min(window.end,last.panel)
		covered <- window.end==last.panel
		cat("Current window: [",window.start,",",window.end,"]\n")
		# segments intersecting the window
		idx <- which(!(inter.df[,"End"]<window.start | inter.df[,"Start"]>window.end))
		# get all concerned chars
		chars <- sort(unique(c(as.matrix(inter.df[idx,c("From","To")]))))
		if(length(chars)>1)
		{	pairs <- t(combn(x=chars,m=2))
			# update dataframe
			for(i in 1:nrow(pairs))
			{	from.char <- pairs[i,1]
				to.char <- pairs[i,2]
				index <- which(static.df[,"From"]==from.char & static.df[,"To"]==to.char)
				if(length(index)==0)
					static.df <- rbind(static.df, data.frame(From=from.char, To=to.char, Occurrences=1))
				else
					static.df[index, "Occurrences"] <- static.df[index, "Occurrences"] + 1
			}
		}
		print(chars)
		# update window
		window.start <- window.start + window.size - overlap
		window.end <- window.start + window.size - 1
	}
	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	dir.create(OUT_FOLDER, showWarnings=FALSE)
	graph.file <- file.path(OUT_FOLDER,paste0("static_panels_ws=",window.size,"_ol=",overlap,".graphml"))
	write_graph(graph=g, file=graph.file, format="graphml")
	
	return(g)
}


###############################################################################
# Extract a static graph based on a list of pairwise interactions, using the
# specified window size and overlap, both expressed in number of pages.
#
# inter.df: dataframe containing the pairwise interactions (columns From, To)
#			and their time of occurrence (columns Start, End).
# pages.info: dataframe containing the number of panels in the pages.
# window.size: size of the time window (expressed in pages).
# overlap: how much consecutive windows overlap (expressed in pages)
#
# returns: the corresponding static graph, whose edge weights correspond to the
#		   number of co-occurrences between the concerned nodes.
###############################################################################
extract.static.graph.from.page.window <- function(inter.df, pages.info, window.size=2, overlap=1)
{	# check the overlap parameter
	if(overlap>=window.size)
		stop("ERROR: overlap must be smaller than window.size")
	
	# init the dataframe
	static.df <- data.frame(From=character(), To=character(), Occurrences=integer(), stringsAsFactors=FALSE)
	Encoding(static.df$From) <- "UTF-8"
	Encoding(static.df$To) <- "UTF-8"
	
	# compute the co-occurrences
	last.page <- nrow(pages.info)	
	window.start <- 1
	window.end <- window.size
	covered <- FALSE
	while(!covered)
	{	window.end <- min(window.end,last.page)
		covered <- window.end==last.page
		cat("Current window: [",window.start,",",window.end,"]")
		# compute start/end in terms of panels
		start.panel <- pages.info[window.start,"Start"]
		end.panel <- pages.info[window.end,"Start"] + pages.info[window.end,"Panels"] - 1
		cat(" ie [",start.panel,",",end.panel,"]\n")
		# segments intersecting the window
		idx <- which(!(inter.df[,"End"]<start.panel | inter.df[,"Start"]>end.panel))
		# get all concerned chars
		chars <- sort(unique(c(as.matrix(inter.df[idx,c("From","To")]))))
		print(chars)
		if(length(chars)>1)
		{	pairs <- t(combn(x=chars,m=2))
			# update dataframe
			for(i in 1:nrow(pairs))
			{	from.char <- pairs[i,1]
				to.char <- pairs[i,2]
				index <- which(static.df[,"From"]==from.char & static.df[,"To"]==to.char)
				if(length(index)==0)
					static.df <- rbind(static.df, data.frame(From=from.char, To=to.char, Occurrences=1))
				else
					static.df[index, "Occurrences"] <- static.df[index, "Occurrences"] + 1
			}
		}
		# update window
		window.start <- window.start + window.size - overlap
		window.end <- window.start + window.size - 1
	}
	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	dir.create(OUT_FOLDER, showWarnings=FALSE)
	graph.file <- file.path(OUT_FOLDER,paste0("static_pages_ws=",window.size,"_ol=",overlap,".graphml"))
	write_graph(graph=g, file=graph.file, format="graphml")
	
	return(g)
}



