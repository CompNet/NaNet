# This script contains functions to plot previously extracted character
# networks.
# 
# Vincent Labatut
# 05/2021
###############################################################################




###############################################################################
# Plots the static graph extracted from scenes.
#
# data: list of dataframes containing everything computed beforehand.
###############################################################################
plot.static.graph.scenes <- function(data)
{	tlog(2,"Plotting the scene-based static graph")
	
	# read the graph
	graph.file <- get.graphname.static(mode="scenes")
	g <- read_graph(file=graph.file, format="graphml")
	
	# set up layout
	if(any(is.na(LAYOUT)))
		setup.graph.layout(g)
	
	# set up vertex size
	E(g)$weight <- E(g)$Duration
	btw <- betweenness(graph=g, directed=FALSE, weights=reverse.weights(E(g)$weight), normalized=FALSE)
	#vsizes <- btw/max(btw) * 8 + 2
	nbtw <- (btw - min(btw)) / (max(btw) - min(btw))
	vsizes <- sqrt(-nbtw^2 + 2*nbtw) * 8 + 2
	
	# set up vertex labels
	vlabs <- rep(NA, gorder(g))
	idx <- which(V(g)$Frequency>=sort(V(g)$Frequency, decreasing=TRUE)[10])
	vlabs[idx] <- sapply(idx, function(i) if(V(g)$ShortName[i]=="") V(g)$name[i] else V(g)$ShortName[i])
	vlabsize <- vsizes*0.12
	
	# plot the whole graph
	graph.file <- file.path(NET_SCENES_FOLDER, "static_scenes")
	tlog(4,"Plotting the whole graph in file ",graph.file)
	png(filename=paste0(graph.file,".png"), width=800, height=800, units="px", pointsize=20, bg="white")
#	pdf(file=paste0(graph.file,".pdf"),bg="white")
		plot(g, 
			layout=LAYOUT,
			vertex.size=vsizes, vertex.color="LIGHTGREY", 
			vertex.label=vlabs, vertex.label.cex=vlabsize,
			vertex.label.family="sans",
			vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
			vertex.label.color="BLACK"
		)
	dev.off()
	
	# plot each volume separately (full graph but different colors)
	vols.folder <- file.path(NET_SCENES_FOLDER, "volumes")
	dir.create(path=vols.folder, showWarnings=FALSE, recursive=TRUE)
	graph.file <- file.path(vols.folder, "static_scenes")
	tlog(4,"Plotting volume-related graphs using vertex colors, in file ",graph.file)
	for(v in 1:length(data$char.volumes))
	{	tlog(6,"Plotting volume ",v,"/",length(data$char.volumes))
		idx <- match(data$char.volumes[[v]], data$char.info[,COL_CHAR_NAME])
		cols <- rep("LIGHTGRAY", nrow(data$char.info))
		cols[idx] <- "RED"
		png(filename=paste0(graph.file,"_vol",v,".png"), width=800, height=800, units="px", pointsize=20, bg="white")
#		pdf(file=paste0(graph.file,"_vol",v,".pdf"),bg="white")
			plot(g, 
				layout=LAYOUT, 
				vertex.size=vsizes, vertex.color=cols, 
				vertex.label=vlabs, vertex.label.cex=vlabsize,
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				main=paste0(data$volume.info[v,COL_VOLS_VOLUME], " - ", data$volume.info[v,COL_VOLS_TITLE], " (",v,"/",nrow(data$volume.info),")")
			)
		dev.off()
	}
	
	# plot each arc separately (full graph but different colors)
	tlog(4,"Plotting arc-related graphs using vertex colors")
	arcs.folder <- file.path(NET_SCENES_FOLDER, "arcs")
	dir.create(path=arcs.folder, showWarnings=FALSE, recursive=TRUE)
	graph.file <- file.path(arcs.folder, "static_scenes")
	arc.titles <- unique(data$volume.info[,COL_VOLS_ARC])
	for(a in 1:length(data$char.arcs))
	{	tlog(6,"Plotting arc ",a,"/",length(data$char.arcs))
		idx <- match(data$char.arcs[[a]], data$char.info[,COL_CHAR_NAME])
		cols <- rep("LIGHTGRAY", nrow(data$char.info))
		cols[idx] <- "RED"
		png(filename=paste0(graph.file,"_arc",a,".png"), width=800, height=800, units="px", pointsize=20, bg="white")
		#pdf(file=paste0(graph.file,"_arc",a,".pdf"),bg="white")
			plot(g, 
				layout=LAYOUT, 
				vertex.size=vsizes, vertex.color=cols, 
				vertex.label=vlabs, vertex.label.cex=vlabsize,
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				main=paste0(arc.titles[a], " (",a,"/",length(data$char.arcs),")")
			)
		dev.off()
	}
	
	tlog(2,"Extraction of the scene-based static graph completed")
	return(g)
	
	# TODO add the plots without the extras
}


################################################################################
## Extracts a static graph based on a list of pairwise interactions, using the
## specified window size and overlap, both expressed in number of panels.
##
## char.info: table describing all the characters occurring in the BD series.
## inter.df: dataframe containing the pairwise interactions (columns 
##			COL_INTER_FROM_CHAR and COL_INTER_TO_CHAR) and their time of 
##           occurrence (columns COL_INTER_START_PANEL_ID and COL_INTER_END_PANEL_ID).
## window.size: size of the time window (expressed in panels).
## overlap: how much consecutive windows overlap (expressed in panels). Must be strictly
##          smaller than window.size.
##
## returns: the corresponding static graph, whose edge weights correspond to the
##		   number of co-occurrences between the concerned nodes.
################################################################################
#plot.static.graph.panel.window <- function(char.info, inter.df, window.size=10, overlap=2)
#{	tlog(2,"Extracting the panel window-based static graph")
#	tlog(3,"For parameters window.size=",window.size," and overlap=",overlap)
#	
#	# check the overlap parameter
#	if(overlap>=window.size)
#	{	msg <- paste0("ERROR: overlap parameter must be smaller than or equal to window.size: window.size=",window.size,", overlap=",overlap)
#		tlog(4,msg)
#		stop(msg)
#	}
#	
#	# init the dataframe
#	static.df <- data.frame(
#		From=character(), To=character(), 
#		Occurrences=integer(), 
#		stringsAsFactors=FALSE)
#	Encoding(static.df$From) <- "UTF-8"
#	Encoding(static.df$To) <- "UTF-8"
#	cn <- c(COL_INTER_FROM_CHAR, COL_INTER_TO_CHAR, COL_INTER_OCCURRENCES)
#	colnames(static.df) <- cn
#	
#	# compute the co-occurrences
#	last.panel <- max(inter.df[,COL_INTER_END_PANEL_ID])
#	window.start <- 1
#	window.end <- window.size
#	covered <- FALSE
#	while(!covered)
#	{	window.end <- min(window.end,last.panel)
#		covered <- window.end==last.panel
#		tlog(3,"Current window: [",window.start,",",window.end,"]")
#		# scenes intersecting the window
#		idx <- which(!(inter.df[,COL_INTER_END_PANEL_ID]<window.start | inter.df[,COL_INTER_START_PANEL_ID]>window.end))
#		# get all concerned chars
#		chars <- sort(unique(c(as.matrix(inter.df[idx,c(COL_INTER_FROM_CHAR,COL_INTER_TO_CHAR)]))))
#		if(length(chars)>1)
#		{	pairs <- t(combn(x=chars,m=2))
#			# update dataframe
#			for(i in 1:nrow(pairs))
#			{	from.char <- pairs[i,1]
#				to.char <- pairs[i,2]
#				index <- which(static.df[,COL_INTER_FROM_CHAR]==from.char & static.df[,COL_INTER_TO_CHAR]==to.char)
#				if(length(index)==0)
#				{	tmp.df <- data.frame(From=from.char, To=to.char, Occurrences=1, stringsAsFactors=FALSE)
#					colnames(tmp.df) <- cn
#					static.df <- rbind(static.df, tmp.df)
#				}
#				else
#					static.df[index, COL_INTER_OCCURRENCES] <- static.df[index, COL_INTER_OCCURRENCES] + 1
#			}
#		}
##		print(chars)
#		# update window
#		window.start <- window.start + window.size - overlap
#		window.end <- window.start + window.size - 1
#	}
#	
#	static.df <- static.df[order(static.df[,COL_INTER_FROM_CHAR],static.df[,COL_INTER_TO_CHAR]),]
##	print(static.df)
#	
#	# init the graph
#	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.info)
#	# write to file
#	graph.file <- get.graphname.static(mode="panel.window", window.size=window.size, overlap=overlap)
#	write_graph(graph=g, file=graph.file, format="graphml")
#	
#	tlog(2,"Extraction of the panel window-based static graph completed")
#	return(g)
#}
#
#
################################################################################
## Extracts a static graph based on a list of pairwise interactions, using the
## specified window size and overlap, both expressed in number of pages.
##
## char.info: table describing all the characters occurring in the BD series.
## inter.df: dataframe containing the pairwise interactions (columns 
##			COL_INTER_FROM_CHAR and COL_INTER_TO_CHAR) and their time of 
##           occurrence (columns COL_INTER_START_PANEL_ID and COL_INTER_END_PANEL_ID).
## page.info: dataframe containing the number of panels in the pages.
## window.size: size of the time window (expressed in pages).
## overlap: how much consecutive windows overlap (expressed in pages). Must be strictly
##          smaller than window.size.
##
## returns: the corresponding static graph, whose edge weights correspond to the
##		   number of co-occurrences between the concerned nodes.
################################################################################
#plot.static.graph.page.window <- function(char.info, inter.df, page.info, window.size=2, overlap=1)
#{	tlog(2,"Extracting the page window-based static graph")
#	tlog(3,"For parameters window.size=",window.size," and overlap=",overlap)
#	
#	# check the overlap parameter
#	if(overlap>=window.size)
#	{	msg <- paste0("ERROR: overlap must be smaller than window.size: window.size=",window.size,", overlap=",overlap)
#		tlog(4,msg)
#		stop(msg)
#	}
#	
#	# init the dataframe
#	static.df <- data.frame(
#			From=character(), To=character(), 
#			Occurrences=integer(), 
#			stringsAsFactors=FALSE)
#	Encoding(static.df$From) <- "UTF-8"
#	Encoding(static.df$To) <- "UTF-8"
#	cn <- c(COL_INTER_FROM_CHAR, COL_INTER_TO_CHAR, COL_INTER_OCCURRENCES)
#	colnames(static.df) <- cn
#	
#	# compute the co-occurrences
#	last.page <- nrow(page.info)	
#	window.start <- 1
#	window.end <- window.size
#	covered <- FALSE
#	while(!covered)
#	{	window.end <- min(window.end,last.page)
#		covered <- window.end==last.page
#		msg <- paste0("Current window: [",window.start,",",window.end,"]")
#		# compute start/end in terms of panels
#		start.panel <- page.info[window.start,COL_INTER_START_PANEL_ID]
#		end.panel <- page.info[window.end,COL_INTER_START_PANEL_ID] + page.info[window.end,COL_PAGES_PANELS] - 1
#		tlog(3,paste0(msg, " ie [",start.panel,",",end.panel,"]"))
#		# scenes intersecting the window
#		idx <- which(!(inter.df[,COL_INTER_END_PANEL_ID]<start.panel | inter.df[,COL_INTER_START_PANEL_ID]>end.panel))
#		# get all concerned chars
#		chars <- sort(unique(c(as.matrix(inter.df[idx,c(COL_INTER_FROM_CHAR,COL_INTER_TO_CHAR)]))))
##		print(chars)
#		if(length(chars)>1)
#		{	pairs <- t(combn(x=chars,m=2))
#			# update dataframe
#			for(i in 1:nrow(pairs))
#			{	from.char <- pairs[i,1]
#				to.char <- pairs[i,2]
#				index <- which(static.df[,COL_INTER_FROM_CHAR]==from.char & static.df[,COL_INTER_TO_CHAR]==to.char)
#				if(length(index)==0)
#				{	tmp.df <- data.frame(From=from.char, To=to.char, Occurrences=1, stringsAsFactors=FALSE)
#					colnames(tmp.df) <- cn
#					static.df <- rbind(static.df, tmp.df)
#				}
#				else
#					static.df[index, COL_INTER_OCCURRENCES] <- static.df[index, COL_INTER_OCCURRENCES] + 1
#			}
#		}
#		# update window
#		window.start <- window.start + window.size - overlap
#		window.end <- window.start + window.size - 1
#	}
#	
#	static.df <- static.df[order(static.df[,COL_INTER_FROM_CHAR],static.df[,COL_INTER_TO_CHAR]),]
##	print(static.df)
#	
#	# init the graph
#	g <- graph_from_data_frame(d=static.df, directed=FALSE, vertices=char.info)
#	# write to file
#	graph.file <- get.graphname.static(mode="page.window", window.size=window.size, overlap=overlap)
#	write_graph(graph=g, file=graph.file, format="graphml")
#	
#	tlog(2,"Extraction of the page window-based static graph completed")
#	return(g)
#}
#



###############################################################################
# Main function for the extraction of graphs based on interaction tables.
#
# data: preprocessed data.
# panel.window.sizes: values for this parameter
# panel.overlaps: values for this parameter, specified for of the above parameter values.
# page.window.sizes: same for page-based windows instead of panel-based.
# page.overlaps: same.
###############################################################################
plot.static.graphs <- function(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Plotting static graphs")
	# plot the scene-based static graph
	g <- plot.static.graph.scenes(data)
	
#	# plot the panel window-based static graphs
#	future_sapply(1:length(panel.window.sizes), function(i)
#	{	window.size <- panel.window.sizes[i]
#		for(overlap in panel.overlaps[[i]])
#			g <- plot.static.graph.panel.window(data$char.info, data$inter.df, window.size=window.size, overlap=overlap)
#	})
#	
#	# plot the page window-based static graphs
#	future_sapply(1:length(page.window.sizes), function(i)
#	{	window.size <- page.window.sizes[i]
#		for(overlap in page.overlaps[[i]])
#			g <- plot.static.graph.page.window(data$char.info, data$inter.df, data$page.info, window.size=window.size, overlap=overlap)
#	})
	
	tlog(1,"Plotting of the static graphs complete")
}
