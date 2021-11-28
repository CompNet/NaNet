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
	graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
	g <- read_graph(file=graph.file, format="graphml")
	# clean names
	V(g)$name <- fix.encoding(strings=V(g)$name)
	V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
	
	# set up layout
	if(any(is.na(LAYOUT)))
		setup.graph.layout(g, NET_SCENES_FOLDER)
#V(g)$x <- LAYOUT[,1]
#V(g)$y <- LAYOUT[,2]
	
# this piece of script was once used to manually fine tune vertex positions with gephi
#gg <- read.graph("D:/Users/Vincent/Downloads/Web/Untitled.graphml",format="graphml")
#tab <- cbind(V(gg)$x, V(gg)$y)
#ids <- as.integer(substr(V(gg)$id, 2, nchar(V(gg)$id)))
#idx <- order(ids)
#tab <- tab[idx,]
##tmp <- cbind(tab, LAYOUT)
##apply(tmp, 2, range)
#tab[,1] <- (tab[,1] - min(tab[,1])) / (max(tab[,1]) - min(tab[,1])) * (max(LAYOUT[,1])-min(LAYOUT[,1]))+min(LAYOUT[,1])
#tab[,2] <- (tab[,2] - min(tab[,2])) / (max(tab[,2]) - min(tab[,2])) * (max(LAYOUT[,2])-min(LAYOUT[,2]))+min(LAYOUT[,2])
#LAYOUT <- tab
	
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
	
	# set up vertex sizes
	E(g)$weight <- E(g)$Duration
	btw <- betweenness(graph=g, directed=FALSE, weights=reverse.weights(E(g)$weight), normalized=FALSE)
	#vsizes <- btw/max(btw) * 8 + 2
	nbtw <- (btw - min(btw)) / (max(btw) - min(btw))
	#vsizes <- sqrt(-nbtw^2 + 2*nbtw) * 8 + 2
	vsizes <- lame.normalize(nbtw,exp=2) * 10000 + 750
#V(g)$size <- vsizes
#write_graph(graph=g, file=graph.file, format="graphml")
	
	# set up vertex colors
	vcols <- rep("LIGHTGREY",gorder(g))
	col.char.nbr <- 5 
	col.char.idx <- order(btw,decreasing=TRUE)[1:col.char.nbr]
	vcols[col.char.idx] <- get.palette(col.char.nbr)
	
	# set up vertex labels
	vlabs <- rep(NA, gorder(g))
	if("Named" %in% vertex_attr_names(g))
	{	idx <- which(V(g)$Named)
	}else
	{	idx <- which(V(g)$Frequency>=sort(V(g)$Frequency, decreasing=TRUE)[10])
		if(length(idx)==0)
			idx <- which(V(g)$Frequency>=sort(V(g)$Frequency, decreasing=TRUE)[4])
	}
	vlabs[idx] <- sapply(idx, function(i) if(V(g)$ShortName[i]=="") V(g)$name[i] else V(g)$ShortName[i])
	vlabsizes <- vsizes*0.0004
	
	# set up edge widths
	ww <- E(g)$weight
	nww <- (ww - min(ww)) / (max(ww) - min(ww))
	ewidths <- lame.normalize(nww,exp=1) * 75 + 0.5
	
	# set up edge colors
	el <- as_edgelist(graph=g, names=FALSE)
	ecols <- sapply(1:nrow(el), function(r) combine.colors(col1=vcols[el[r,1]], col2=vcols[el[r,2]]))
	ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],100-100*lame.normalize(nww[i],exp=3)))
	
	# get filtered edges
	idx.efiltr <- which(el[,1] %in% idx.filtr & el[,2] %in% idx.filtr)
	
	# plot whole unfiltered graph
	graph.file <- get.path.graph.file(mode="scenes", filtered=FALSE)
	tlog(4,"Plotting the whole unfiltered graph in file ",graph.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
			plot(g, 
				layout=LAYOUT,
				vertex.size=vsizes, vertex.color=vcols,
				vertex.label=vlabs, vertex.label.cex=vlabsizes,
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				edge.color=ecols, edge.width=ewidths, 
				rescale=FALSE, #axe=TRUE, 
				xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2])
			)
		dev.off()
	}
	# plot whole filtered graph
	graph.file <- get.path.graph.file(mode="scenes", filtered=TRUE)
	tlog(4,"Plotting the whole filtered graph in file ",graph.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
			plot(g.filtr, 
				layout=LAYOUT[idx.filtr,],	# lay.filtr
				vertex.size=vsizes[idx.filtr], vertex.color=vcols[idx.filtr],
				vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsizes[idx.filtr],
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				edge.color=ecols[idx.efiltr], edge.width=ewidths[idx.efiltr], 
				rescale=FALSE, #axe=TRUE, 
				xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2])
			)
		dev.off()
	}
	
	# plot each volume separately (full graph but different vertex colors)
	vols.folder <- file.path(NET_SCENES_FOLDER, "volumes")
	dir.create(path=vols.folder, showWarnings=FALSE, recursive=TRUE)
	tlog(4,"Plotting volume-related graphs using vertex colors")
	for(v in 1:length(data$char.volumes))
	{	vname <- data$volume.info[v,COL_VOLS_VOLUME]
		tlog(6,"Plotting volume ",vname," (",v,"/",length(data$char.volumes),")")
		idx <- match(data$char.volumes[[v]], data$char.info[,COL_CHAR_NAME])
		el <- as_edgelist(graph=g, names=FALSE)
		idx.e <- which(el[,1] %in% idx & el[,2] %in% idx)
		vcols <- rep("LIGHTGRAY", nrow(data$char.info))
		vcols[idx] <- "RED"
		ecols <- rep("LIGHTGREY",gsize(g))
		ecols[idx.e] <- "RED"
		ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],100-80*lame.normalize(nww[i],exp=3)))
		# unfiltered graph
		graph.file <- get.path.graph.file(mode="scenes", vol=vname, filtered=FALSE, subfold="fulledges")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
				plot(g, 
					layout=LAYOUT, 
					vertex.size=vsizes, vertex.color=vcols, 
					vertex.label=vlabs, vertex.label.cex=vlabsizes,
					vertex.label.family="sans",
					vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
					vertex.label.color="BLACK",
					edge.color=ecols, edge.width=ewidths, 
					rescale=FALSE, #axe=TRUE, 
					xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2]),
					main=paste0(vname, " - ", data$volume.info[v,COL_VOLS_TITLE], " (",v,"/",nrow(data$volume.info),")")
			)
			dev.off()
		}
		# filtered graph
		graph.file <- get.path.graph.file(mode="scenes", vol=vname, filtered=TRUE, subfold="fulledges")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(graph.file,"_vol",vname,"_filtered",PLOT_FORMAT_PDF), bg="white", width=40, height=40)
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(graph.file,"_vol",vname,"_filtered",PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
				plot(g.filtr, 
					layout=LAYOUT[idx.filtr,],	# lay.filtr,  
					vertex.size=vsizes[idx.filtr], vertex.color=vcols[idx.filtr], 
					vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsizes[idx.filtr],
					vertex.label.family="sans",
					vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
					vertex.label.color="BLACK",
					edge.color=ecols[idx.efiltr], edge.width=ewidths[idx.efiltr], 
					rescale=FALSE, #axe=TRUE, 
					xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2]),
					main=paste0(vname, " - ", data$volume.info[v,COL_VOLS_TITLE], " (",v,"/",nrow(data$volume.info),")")
				)
			dev.off()
		}
	}
	
	# plot each arc separately (full graph but different vertex colors)
	tlog(4,"Plotting arc-related graphs using vertex colors")
	arcs.folder <- file.path(NET_SCENES_FOLDER, "arcs")
	dir.create(path=arcs.folder, showWarnings=FALSE, recursive=TRUE)
	arc.titles <- unique(data$volume.info[,COL_VOLS_ARC])
	for(a in 1:length(data$char.arcs))
	{	tlog(6,"Plotting arc ",a,"/",length(data$char.arcs))
		idx <- match(data$char.arcs[[a]], data$char.info[,COL_CHAR_NAME])
		el <- as_edgelist(graph=g, names=FALSE)
		idx.e <- which(el[,1] %in% idx & el[,2] %in% idx)
		vcols <- rep("LIGHTGRAY", nrow(data$char.info))
		vcols[idx] <- "RED"
		ecols <- rep("LIGHTGREY",gsize(g))
		ecols[idx.e] <- "RED"
		ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],100-80*lame.normalize(nww[i],exp=3)))
		# unfiltered graph
		graph.file <- get.path.graph.file(mode="scenes", arc=a, filtered=FALSE, subfold="fulledges")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
				plot(g, 
					layout=LAYOUT, 
					vertex.size=vsizes, vertex.color=vcols, 
					vertex.label=vlabs, vertex.label.cex=vlabsizes,
					vertex.label.family="sans",
					vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
					vertex.label.color="BLACK",
					edge.color=ecols, edge.width=ewidths, 
					rescale=FALSE, #axe=TRUE, 
					xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2]),
					main=paste0(arc.titles[a], " (",a,"/",length(data$char.arcs),")")
				)
			dev.off()
		}
		# filtered graph
		graph.file <- get.path.graph.file(mode="scenes", arc=a, filtered=TRUE, subfold="fulledges")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
				plot(g.filtr, 
					layout=LAYOUT[idx.filtr,], #lay.filtr, 
					vertex.size=vsizes[idx.filtr], vertex.color=vcols[idx.filtr], 
					vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsizes[idx.filtr],
					vertex.label.family="sans",
					vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
					vertex.label.color="BLACK",
					edge.color=ecols[idx.efiltr], edge.width=ewidths[idx.efiltr], 
					rescale=FALSE, #axe=TRUE, 
					xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2]),
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
	
	# read unfiltered graph
	graph.file <- get.path.graph.file(mode="scenes", arc=arc, vol=vol, filtered=FALSE, ext=".graphml")
	g <- read_graph(file=graph.file, format="graphml")
	# clean names
	V(g)$name <- fix.encoding(strings=V(g)$name)
	V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
	idx.con <- which(degree(g,mode="all")>0)
			
	# set up layout
	if(any(is.na(LAYOUT)))
		setup.graph.layout(g, NET_SCENES_FOLDER)
	
	# read filtered graph
	graph.file <- get.path.graph.file(mode="scenes", arc=arc, vol=vol, filtered=TRUE, ext=".graphml")
	g.filtr <- read_graph(file=graph.file, format="graphml")
	# clean names
	V(g.filtr)$name <- fix.encoding(strings=V(g.filtr)$name)
	V(g.filtr)$ShortName <- fix.encoding(strings=V(g.filtr)$ShortName)
	idx.filtr <- match(V(g.filtr)$name, V(g)$name)
	
	# set up vertex sizes
	E(g)$weight <- E(g)$Duration
	btw <- betweenness(graph=g, directed=FALSE, weights=reverse.weights(E(g)$weight), normalized=FALSE)
	nbtw <- (btw - min(btw)) / (max(btw) - min(btw))
	vsizes <- lame.normalize(nbtw,exp=3) * 10000 + 750
	vsizes[idx.con] <- vsizes[idx.con] + 1000
	
	# set up vertex colors
	vcols <- rep(make.color.transparent("LIGHTGREY",80),gorder(g))
	vfcols <- rep(make.color.transparent("BLACK",80),gorder(g))
	vcols[idx.con] <- rep("LIGHTGREY",length(idx.con))
	vfcols[idx.con] <- rep("BLACK",length(idx.con))
	
	# set up vertex labels
	vlabs <- rep(NA, gorder(g))
	vlabs[idx.con] <- sapply(idx.con, function(i) if(V(g)$ShortName[i]=="") V(g)$name[i] else V(g)$ShortName[i])
	vlabsizes <- vsizes*0.0004
	
	# set up edge widths
	ww <- E(g)$weight
	nww <- (ww - min(ww)) / (max(ww) - min(ww))
	ewidths <- lame.normalize(nww,exp=1) * 25 + 1
	
	# set up edge colors
	el <- as_edgelist(graph=g, names=FALSE)
	ecols <- rep("GRAY",gsize(g))
	ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],60))
	
	# get filtered edges
	idx.efiltr <- which(el[,1] %in% idx.filtr & el[,2] %in% idx.filtr)
	
	# plot unfiltered graph
	graph.file <- get.path.graph.file(mode="scenes", arc=arc, vol=vol, filtered=FALSE)
	tlog(4,"Plotting the selected unfiltered graph in file ",graph.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
		plot(g, 
				layout=LAYOUT,
				vertex.size=vsizes, vertex.color=vcols,
				vertex.frame.color=vfcols,
				vertex.label=vlabs, vertex.label.cex=vlabsizes,
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				edge.color=ecols, edge.width=ewidths, 
				rescale=FALSE, #axe=TRUE, 
				xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2])
		)
		dev.off()
	}
	
	# plot filtered graph
	graph.file <- get.path.graph.file(mode="scenes", arc=arc, vol=vol, filtered=TRUE)
	tlog(4,"Plotting the selected filtered graph in file ",graph.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(graph.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(graph.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
		plot(g.filtr, 
				layout=LAYOUT[idx.filtr,],	# lay.filtr
				vertex.size=vsizes[idx.filtr], vertex.color=vcols[idx.filtr],
				vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsizes[idx.filtr],
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				edge.color=ecols[idx.efiltr], edge.width=ewidths[idx.efiltr], 
				rescale=FALSE, frame=TRUE, #axe=TRUE, 
				xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2])
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
