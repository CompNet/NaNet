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
#			and their time of occurrence (columns StartPanel, EndPanel).
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
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	graph.file <- file.path(DATA_FOLDER,"static_segments.graphml")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	return(g)
}


###############################################################################
# Extract a static graph based on a list of pairwise interactions, using the
# specified time unit and overlap.
#
# inter.df: dataframe containing the pairwise interactions (columns From, To)
#			and their time of occurrence (columns StartPanel, EndPanel, StartPage,
#			and EndPage).
# time.unit: either "page" or "panel".
# window.size: size of the time window (expressed in time units).
# overlap: how much consecutive windows overlap (expressed in time units)
#
# returns: the corresponding static graph, whose edge weights correspond to the
#		   number of interactions between the concerned nodes.
###############################################################################
extract.static.graph.from.segments <- function(inter.df, time.unit="page", window.size=1, overlap=0)
{	# check the overlap parameter
	if(overlap>=window.size)
		stop("ERROR: overlap must be smaller than window.size")
	
	# init the dataframe
	static.df <- data.frame(From=character(), To=character(), Occurrences=integer(), stringsAsFactors=FALSE)
	Encoding(static.df$From) <- "UTF-8"
	Encoding(static.df$To) <- "UTF-8"
	
# TODO	
	# build the edgelist
	for(i in 1:nrow(inter.df))
	{	from.char <- inter.df[i,"From"]
		to.char <- inter.df[i,"To"]
		index <- which(static.df[,"From"]==from.char & static.df[,"To"]==to.char)
		length <- inter.df[i,"End"] - inter.df[i,"Start"] + 1
		if(length(index)==0)
			static.df <- rbind(static.df, data.frame(From=from.char, To=to.char, Occurrences=1, Duration=length))
		else
			static.df[index, "Occurrences"] <- static.df[index, "Occurrences"] + 1
	}
	
	# init the graph
	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=NULL)
	# write to file
	graph.file <- file.path(DATA_FOLDER,paste0("static_",time.unit,"_ws=",window.size,"_ol=",overlap,".graphml"))
	write_graph(graph=g, file=graph.file, format="graphml")
	
	return(g)
}


