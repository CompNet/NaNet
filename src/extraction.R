# This script contains functions related to the extraction of character
# networks based on raw interaction tables.
# 
# Vincent Labatut
# 11/2018
###############################################################################




###############################################################################
# Reads the raw data contained in several tables, and returns them under the
# form of data frames.
#
# returns: a list of 3 dataframes, volumes.info (information related to the
#          volumes), pages.info (information related to the pages), inter.df
#          (interactions between the characters).
###############################################################################
read.raw.data <- function()
{	# read the file describing the volumes
	tlog(2,"Trying to read the volume file (",INTER_FILE,")")
	volumes.info <- read.csv(VOLUMES_FILE, header=TRUE, check.names=FALSE)
	tlog(2,"Reading of the volume file completed")
	
	# read the file describing the pages
	tlog(2,"Trying to read the page file (",PAGES_FILE,")")
	pages.info <- read.csv(PAGES_FILE, header=TRUE, check.names=FALSE)
	Start <- cumsum(c(1,pages.info[,2]))	# get the number of the panel starting each page since the beginning
	Start <- Start[1:(length(Start)-1)]
	pages.info <- cbind(pages.info,Start)
	tlog(2,"Reading of the page file completed")
	
	# read the file describing the interactions
	tlog(2,"Trying to read the interaction file (",INTER_FILE,")")
	con <- file(INTER_FILE, open="r")
	temp <- readLines(con)
	close(con)
	lines <- strsplit(temp, split='\t', fixed=TRUE)
	tlog(2,"Reading of the interaction file completed")
	
	## get the list of all characters
	#all.chars <- c()
	#for(line in lines)
	#{	chars <- line[3:length(line)]
	#	all.chars <- union(all.chars,chars)
	#}
	#all.chars <- sort(all.chars)
	
	# extract the edge list
	tlog(2,"Converting interactions to dataframe")
	inter.df <- data.frame(
			From=character(), To=character(), 
			Start=integer(), End=integer(), 
			stringsAsFactors=FALSE)
	Encoding(inter.df$From) <- "UTF-8"
	Encoding(inter.df$To) <- "UTF-8"
	for(line in lines)
	{	# get segment bounds
		start <- strsplit(line[1], split='.', fixed=TRUE)[[1]]
		start.page <- as.integer(start[1])
		start.panel <- as.integer(start[2])
		start.abs <- pages.info[start.page,"Start"] + start.panel - 1
		if(is.na(start.abs))
			stop(paste0("Problem with line:\"",paste(line,collapse=","),"\""))
		end <- strsplit(line[2], split='.', fixed=TRUE)[[1]]
		end.page <- as.integer(end[1])
		end.panel <- as.integer(end[2])
		end.abs <- pages.info[end.page,"Start"] + end.panel - 1
		if(is.na(end.abs))
			stop(paste0("Problem with line:\"",paste(line,collapse=","),"\""))
		# compute segment length (in pages)
		page.length <- end.page - start.page + 1
		# get all combinations of characters
		chars <- line[3:length(line)]
		chars <- gsub("[()]", "", chars)	# remove parenthesis (representing ghost characters)
		chars <- sort(chars[which(chars!="" & chars!=" ")])
		chars <- t(combn(x=chars,m=2))
		# add segment to data frame
		df <- data.frame(From=(chars[,1]), To=chars[,2], 
				Start=as.integer(rep(start.abs,nrow(chars))), End=as.integer(rep(end.abs,nrow(chars))),
				stringsAsFactors=FALSE)
		inter.df <- rbind(inter.df, df)
	}
	tlog(2,"Conversion of the interaction raw data completed")
	
	# build result and return
	result <- list (pages.info=pages.info, volumes.info=volumes.info, inter.df=inter.df)
	return(result)
}




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
{	tlog(2,"Extracting the segment-based static graph")
	
	# init the dataframe
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
#	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	graph.file <- file.path(NET_FOLDER,"static_segments.graphml")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the segment-based static graph completed")
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
{	tlog(2,"Extracting the panel window-based static graph")
	
	# check the overlap parameter
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
		tlog(3,"Current window: [",window.start,",",window.end,"]")
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
#		print(chars)
		# update window
		window.start <- window.start + window.size - overlap
		window.end <- window.start + window.size - 1
	}
#	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	graph.file <- file.path(NET_FOLDER,paste0("static_panels_ws=",window.size,"_ol=",overlap,".graphml"))
	write_graph(graph=g, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the panel window-based static graph completed")
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
{	tlog(2,"Extracting the page window-based static graph")
	
	# check the overlap parameter
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
		msg <- paste0("Current window: [",window.start,",",window.end,"]")
		# compute start/end in terms of panels
		start.panel <- pages.info[window.start,"Start"]
		end.panel <- pages.info[window.end,"Start"] + pages.info[window.end,"Panels"] - 1
		tlog(3,paste0(msg, " ie [",start.panel,",",end.panel,"]"))
		# segments intersecting the window
		idx <- which(!(inter.df[,"End"]<start.panel | inter.df[,"Start"]>end.panel))
		# get all concerned chars
		chars <- sort(unique(c(as.matrix(inter.df[idx,c("From","To")]))))
#		print(chars)
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
#	print(static.df)
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	graph.file <- file.path(NET_FOLDER,paste0("static_pages_ws=",window.size,"_ol=",overlap,".graphml"))
	write_graph(graph=g, file=graph.file, format="graphml")
	
	tlog(2,"Extraction of the page window-based static graph completed")
	return(g)
}




###############################################################################
# Main function for the extraction of graphs based on interaction tables.
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
extract.graphs <- function()
{	# read the raw data
	tlog(1,"Reading data files")
	data <- read.raw.data()
	
	tlog(1,"Extracting static graphs")
	# extract the segment-based static graph
	g <- extract.static.graph.from.segments(data$inter.df)
	#plot(g, layout=layout_with_fr(g))
	# extract the window-based static graphs
	g <- extract.static.graph.from.panel.window(data$inter.df, window.size=10, overlap=2)
	g <- extract.static.graph.from.page.window(data$inter.df, data$pages.info, window.size=2, overlap=1)
	
	tlog(1,"Extracting dynamic graphs")
	
	
	tlog(1,"Extraction of the graphs completed")
}


