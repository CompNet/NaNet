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
	graph.file <- get.path.graph.file(mode="scenes")
	g <- read_graph(file=graph.file, format="graphml")
	
	# set up layout
	if(any(is.na(LAYOUT)))
		setup.graph.layout(g, NET_SCENES_FOLDER)
	
	# get filtered graph
	idx.filtr <- which(V(g)$Frequency>2)
	g.filtr <- induced_subgraph(g, v=idx.filtr)
	tmp <- get.largest.component(g.filtr, indices=TRUE)
	idx.cmp <- idx.filtr[tmp$indices]
	g.cmp <- tmp$comp
	el <- get.edgelist(g.cmp, names=FALSE)
	ww <- rep(1, gsize(g.cmp))
	#ww <- E(cmp)$weight
	lay.cmp <<- qgraph.layout.fruchtermanreingold(
		edgelist=el, 
		vcount=gorder(g.cmp), 
		weight=ww, 
		area=10*(gorder(g.cmp)^2),repulse.rad=(gorder(g.cmp)^3.0)
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
				layout=lay.cmp, #LAYOUT[idx.cmp,],
				vertex.size=vsizes[idx.cmp], vertex.color="LIGHTGREY"[idx.cmp], 
				vertex.label=vlabs[idx.cmp], vertex.label.cex=vlabsize[idx.cmp],
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK"
			)
		dev.off()
	}
	
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
				plot(g.cmp, 
					layout=lay.cmp, #LAYOUT[idx.cmp,], 
					vertex.size=vsizes[idx.cmp], vertex.color=cols[idx.cmp], 
					vertex.label=vlabs[idx.cmp], vertex.label.cex=vlabsize[idx.cmp],
					vertex.label.family="sans",
					vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
					vertex.label.color="BLACK",
					main=paste0(data$volume.info[v,COL_VOLS_VOLUME], " - ", data$volume.info[v,COL_VOLS_TITLE], " (",v,"/",nrow(data$volume.info),")")
				)
			dev.off()
		}
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
				plot(g.cmp, 
					layout=lay.cmp, #LAYOUT[idx.cmp,], 
					vertex.size=vsizes[idx.cmp], vertex.color=cols[idx.cmp], 
					vertex.label=vlabs[idx.cmp], vertex.label.cex=vlabsize[idx.cmp],
					vertex.label.family="sans",
					vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
					vertex.label.color="BLACK",
					main=paste0(arc.titles[a], " (",a,"/",length(data$char.arcs),")")
				)
			dev.off()
		}
	}
	
	tlog(2,"Extraction of the scene-based static graph completed")
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
	g <- plot.static.graph.scenes(data)
	
	tlog(1,"Plotting of the static graphs complete")
}
