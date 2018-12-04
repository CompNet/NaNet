# TODO: Add comment
# 
# Vincent Labatut
# 11/2018
###############################################################################




###############################################################################
# Extract a static graph based on a list of pairwise interactions.
#
# inter.df: dataframe containing the pairwise interactions (columns From, To)
#			and their time of occurrence (columns Start, End).
#
# returns: the corresponding static graph. It contains several edge weigths:
#		   - Occurrences: number of interactions between the concerned nodes
#		   - Duration: total duration (in time units).
###############################################################################
extract.static.graph <- function(inter.df)
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
	graph.file <- file.path(DATA_FOLDER,"static.graphml")
	write_graph(graph=g, file=graph.file, format="graphml")
	
	return(g)
}
