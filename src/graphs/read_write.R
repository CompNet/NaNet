#############################################################################################
# Functions used to read and write graph files.
# 
# 03/2022 Vincent Labatut
#
# source("src/graphs/read_write.R")
#############################################################################################




#############################################################################################
# Reads the graph produced by the script, converting the strings when needed.
#
# file: file path.
#
# returns: igraph object.
#############################################################################################
read.graphml.file <- function(file)
{	# read graph
	g <- read_graph(file=file, format="graphml")
	
	# convert strings
	for(col in c(tolower(COL_NAME), COL_NAME_SHORT))
	{	str <- vertex_attr(graph=g, name=col)
		str <- fix.encoding(strings=str)
		g <- set_vertex_attr(graph=g, name=col, value=str)
	}
	
	return(g)
}




###############################################################################
# Record a dynamic graph as a series of graph.
#
# gs: list of igraph objects representing a dynamic graph.
# base.path: path and beginning of the name of the files to create.
###############################################################################
write.dynamic.graph <- function(gs, base.path)
{	tlog(2,"Writting files of the form \"",base.path,"\"")
	
	# number of digits in the file names
	sc.nbr <- length(gs)
	digits <- floor(log(sc.nbr,10)) + 1
	
	# loop over scenes
	for(s in 1:sc.nbr)
	{	graph.file <- paste0(base.path,"_",sprintf(paste0("%0",digits,"d"),s),".graphml")
		if(s==1 || s %% 500 == 0 || s==sc.nbr)
			tlog(4, "(",s,"/",sc.nbr,") Recording file ",graph.file)
		g <- gs[[s]]
		write.graph(graph=g, file=graph.file, format="graphml")
	}
}




###############################################################################
# Read sequence of graphs representing a dynamic graph, based on a sequence of
# graphml files, each one representing one step of the dynamic graph.
#
# base.path: path and beginning of the name of the files to read.
# remove.isolates: whether to remove isolates in each time slice.
#
# returns: list of igraph objects representing a dynamic graph.
###############################################################################
read.dynamic.graph <- function(base.path, remove.isolates=TRUE)
{	tlog(2,"Reading files of the form \"",base.file,"\"")
	gs <- list()
	
	# get folder path 
	folder <- dirname(base.path)
	file <- basename(base.path)
	ll <- list.files(path=folder, pattern=paste0(file,".+"), full.names=TRUE)
	
	# number of digits in the file names
	sc.nbr <- length(ll)
	digits <- floor(log(sc.nbr,10)) + 1
	
	# loop over scenes
	go.on <- TRUE
	s <- 1
	while(go.on)
	{	graph.file <- paste0(base.file,"_",s,".graphml")
		if(file.exists(graph.file))
		{	if(s==1 || s %% 500 == 0 || s==length(gs))
				tlog(4, "Reading file ",graph.file)
			
			# read graph
			g <- read.graphml.file(file=graph.file)
			
			# possibly remove isolates
			if(remove.isolates)
			{	isolates <- which(degree(g)==0)
				g <- delete_vertices(g, isolates)
			}
			
			gs[[s]] <- g
			s <- s + 1
		}
		else
			go.on <- FALSE
	}
	
	return(gs)
}
