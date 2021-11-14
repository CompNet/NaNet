# This script contains functions to plot previously extracted character
# networks.
# 
# Vincent Labatut
# 05/2021
###############################################################################




###############################################################################
# Plots the static graph extracted from all the scenes.
#
# data: list of dataframes containing everything computed beforehand.
###############################################################################
plot.static.graph.scenes.all <- function(data)
{	tlog(2,"Plotting the scene-based static graph")
	
	# read the graph
	graph.file <- get.path.graph.file(mode="scenes")
	g <- read_graph(file=graph.file, format="graphml")
	
	# set up layout
	if(any(is.na(LAYOUT)))
		setup.graph.layout(g, NET_SCENES_FOLDER)
	
	# get filtered graph
	idx.filtr <- which(!V(g)$Filtered)
	g.filtr <- delete_vertices(graph=g, v=which(V(g)$Filtered))
	el <- get.edgelist(g.filtr, names=FALSE)
	ww <- rep(1, gsize(g.filtr))
	#ww <- E(cmp)$weight
	lay.filtr <<- qgraph.layout.fruchtermanreingold(
		edgelist=el, 
		vcount=gorder(g.filtr), 
		weight=ww, 
		area=10*(gorder(g.filtr)^2),repulse.rad=(gorder(g.filtr)^3.0)
	)
	
	# set up vertex size
	E(g)$weight <- E(g)$Duration
	btw <- betweenness(graph=g, directed=FALSE, weights=reverse.weights(E(g)$weight), normalized=FALSE)
	#vsizes <- btw/max(btw) * 8 + 2
	nbtw <- (btw - min(btw)) / (max(btw) - min(btw))
	vsizes <- sqrt(-nbtw^2 + 2*nbtw) * 8 + 2
	
	# set up vertex labels
	vlabs <- rep(NA, gorder(g))
	idx <- which(V(g)$Frequency>=sort(V(g)$Frequency, decreasing=TRUE)[10])
	if(length(idx)==0)
		idx <- which(V(g)$Frequency>=sort(V(g)$Frequency, decreasing=TRUE)[4])
	vlabs[idx] <- sapply(idx, function(i) if(V(g)$ShortName[i]=="") V(g)$name[i] else V(g)$ShortName[i])
	vlabsize <- vsizes*0.12
	
	# plot the whole graph
	graph.file <- file.path(NET_SCENES_FOLDER, "static_scenes")
	tlog(4,"Plotting the whole graph in file ",graph.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			plot(g, 
				layout=LAYOUT,
				vertex.size=vsizes, vertex.color="LIGHTGREY", 
				vertex.label=vlabs, vertex.label.cex=vlabsize,
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK"
			)
		dev.off()
	}
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,"_filtered",PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,"_filtered",PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			plot(g.filtr, 
				layout=LAYOUT[idx.filtr,],	# lay.filtr
				vertex.size=vsizes[idx.filtr], vertex.color="LIGHTGREY"[idx.filtr], 
				vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsize[idx.filtr],
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK"
			)
		dev.off()
	}
	
	# plot each volume separately (full graph but different vertex colors)
	vols.folder <- file.path(NET_SCENES_FOLDER, "volumes")
	dir.create(path=vols.folder, showWarnings=FALSE, recursive=TRUE)
	graph.file <- file.path(vols.folder, "static_scenes_fulledges")
	tlog(4,"Plotting volume-related graphs using vertex colors, in file ",graph.file)
	for(v in 1:length(data$char.volumes))
	{	tlog(6,"Plotting volume ",v,"/",length(data$char.volumes))
		idx <- match(data$char.volumes[[v]], data$char.info[,COL_CHAR_NAME])
		cols <- rep("LIGHTGRAY", nrow(data$char.info))
		cols[idx] <- "RED"
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(graph.file,"_vol",v,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(graph.file,"_vol",v,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
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
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(graph.file,"_vol",v,"_filtered",PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(graph.file,"_vol",v,"_filtered",PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				plot(g.filtr, 
					layout=LAYOUT[idx.filtr,],	# lay.filtr,  
					vertex.size=vsizes[idx.filtr], vertex.color=cols[idx.filtr], 
					vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsize[idx.filtr],
					vertex.label.family="sans",
					vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
					vertex.label.color="BLACK",
					main=paste0(data$volume.info[v,COL_VOLS_VOLUME], " - ", data$volume.info[v,COL_VOLS_TITLE], " (",v,"/",nrow(data$volume.info),")")
				)
			dev.off()
		}
	}
	
	# plot each arc separately (full graph but different vertex colors)
	tlog(4,"Plotting arc-related graphs using vertex colors")
	arcs.folder <- file.path(NET_SCENES_FOLDER, "arcs")
	dir.create(path=arcs.folder, showWarnings=FALSE, recursive=TRUE)
	graph.file <- file.path(arcs.folder, "static_scenes_fulledges")
	arc.titles <- unique(data$volume.info[,COL_VOLS_ARC])
	for(a in 1:length(data$char.arcs))
	{	tlog(6,"Plotting arc ",a,"/",length(data$char.arcs))
		idx <- match(data$char.arcs[[a]], data$char.info[,COL_CHAR_NAME])
		cols <- rep("LIGHTGRAY", nrow(data$char.info))
		cols[idx] <- "RED"
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(graph.file,"_arc",a,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(graph.file,"_arc",a,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
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
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(graph.file,"_arc",a,"_filtered",PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(graph.file,"_arc",a,"_filtered",PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				plot(g.filtr, 
					layout=LAYOUT[idx.filtr,], #lay.filtr, 
					vertex.size=vsizes[idx.filtr], vertex.color=cols[idx.filtr], 
					vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsize[idx.filtr],
					vertex.label.family="sans",
					vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
					vertex.label.color="BLACK",
					main=paste0(arc.titles[a], " (",a,"/",length(data$char.arcs),")")
				)
			dev.off()
		}
	}
	
	tlog(2,"Plotting of the scene-based static graph completed")
	return(g)
}




###############################################################################
# Plots the static graph extracted from only certain scenes (one volume or one
# narrative arc).
#
# data: list of dataframes containing everything computed beforehand.
# arc: the narrative arc to plot.
# vol: the volume to plot (ignored if arc is specified).
###############################################################################
plot.static.graph.scenes.partial <- function(data, arc=NA, vol=NA)
{	tlog(2,"Plotting the scene-based static graph for ",if(is.na(arc)) paste0("vol=",vol) else paste0("arc=",arc))
	
	# read unfiltere graph
	graph.file <- get.path.graph.file(mode="scenes", arc=arc, vol=vol, filtered=FALSE)
	g <- read_graph(file=graph.file, format="graphml")
	
	# set up layout
	if(any(is.na(LAYOUT)))
		setup.graph.layout(g, NET_SCENES_FOLDER)
	
	# read filtered graph
	graph.file <- get.path.graph.file(mode="scenes", arc=arc, vol=vol, filtered=TRUE)
	g.filtr <- read_graph(file=graph.file, format="graphml")
	tmp <- match(V(g.filtr)$name, V(g)$name)
	idx.filtr <- which(!is.na(tmp))
	
	# set up vertex size
	E(g)$weight <- E(g)$Duration
	btw <- betweenness(graph=g, directed=FALSE, weights=reverse.weights(E(g)$weight), normalized=FALSE)
	#vsizes <- btw/max(btw) * 8 + 2
	nbtw <- (btw - min(btw)) / (max(btw) - min(btw))
	vsizes <- sqrt(-nbtw^2 + 2*nbtw) * 8 + 2
	
	# set up vertex labels
	vlabs <- rep(NA, gorder(g))
	idx <- which(V(g)$Frequency>=sort(V(g)$Frequency, decreasing=TRUE)[10])
	if(length(idx)==0)
		idx <- which(V(g)$Frequency>=sort(V(g)$Frequency, decreasing=TRUE)[4])
	vlabs[idx] <- sapply(idx, function(i) if(V(g)$ShortName[i]=="") V(g)$name[i] else V(g)$ShortName[i])
	vlabsize <- vsizes*0.12
	
	# plot the whole graph
	if(is.na(arc))
		graph.file <- file.path(NET_SCENES_FOLDER, "volumes", "static_scenes_vol",vol)
	else
		graph.file <- file.path(NET_SCENES_FOLDER, "arcs", "static_scenes_arc",arc)
	tlog(4,"Plotting the whole graph in file ",graph.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			plot(g, 
				layout=LAYOUT,
				vertex.size=vsizes, vertex.color="LIGHTGREY", 
				vertex.label=vlabs, vertex.label.cex=vlabsize,
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK"
			)
		dev.off()
	}
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,"_filtered",PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,"_filtered",PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			plot(g.filtr, 
				layout=LAYOUT[idx.filtr,],	# lay.filtr
				vertex.size=vsizes[idx.filtr], vertex.color="LIGHTGREY"[idx.filtr], 
				vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsize[idx.filtr],
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK"
			)
		dev.off()
	}
	
	tlog(2,"Plotting of the scene-based static graph completed (",if(is.na(arc)) paste0("vol=",vol) else paste0("arc=",arc),")")
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
plot.static.graphs <- function(data, panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Plotting static graphs")
	
	# plot the scene-based static graph
	g <- plot.static.graph.scenes.all(data)
	
	# same for each arc
	arc.titles <- unique(data$volume.info[,COL_VOLS_ARC])
	for(arc in 1:length(arc.titles))
		plot.static.graph.scenes.partial(data, arc=arc)
	
	# same for each volume
	volume.nbr <- nrow(data$volume.info)
	for(v in 1:volume.nbr)
	{	vol <- data$volume.info[v, COL_VOLS_VOLUME]
		plot.static.graph.scenes.partial(data, vol=vol)
	}
	
	tlog(1,"Plotting of the static graphs complete")
}
