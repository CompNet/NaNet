# This script contains functions to plot previously extracted character
# networks.
# 
# Vincent Labatut
# 05/2021
###############################################################################




###############################################################################
# Computes the graphical parameters used when plotting the specified network.
#
# g: graph to plot (by default, the full graph).
#
# returns: list of graphical parameters used later to plot the graph.
###############################################################################
compute.graphical.params <- function(g=NA)
{	# read the graph
	graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
	g0 <- read_graph(file=graph.file, format="graphml")
	# clean names
	V(g0)$name <- fix.encoding(strings=V(g0)$name)
	V(g0)$ShortName <- fix.encoding(strings=V(g0)$ShortName)
	
	# set up layout
	if(any(is.na(LAYOUT)))
		setup.graph.layout(g0, NET_SCENES_FOLDER)
#V(g0)$x <- LAYOUT[,1]
#V(g0)$y <- LAYOUT[,2]
	
# this piece of script was once used to manually fine tune vertex positions with gephi
#gg <- read.graph("D:/Users/Vincent/Downloads/Web/Untitled.graphml",format="graphml")
#tab <- cbind(V(gg)$x, V(gg)$y)
#ids <- as.integer(substr(V(gg)$id, 2, nchar(V(gg)$id)))
#idx <- order(ids)
#tab <- tab[idx,]
##tmp <- cbind(tab, LAYOUT)
##apply(tmp, 2, range)
#tab[,2] <- (tab[,2] - min(tab[,1])) / (max(tab[,1]) - min(tab[,1])) * (max(LAYOUT[,1])-min(LAYOUT[,1]))+min(LAYOUT[,1])
#tab[,1] <- (tab[,1] - min(tab[,1])) / (max(tab[,1]) - min(tab[,1])) * (max(LAYOUT[,1])-min(LAYOUT[,1]))+min(LAYOUT[,1])
#LAYOUT <- tab
#lay.file <- file.path(NET_SCENES_FOLDER, "all_layout.txt")
#write.table(x=LAYOUT,file=lay.file)
	
	# possibly use the default graph
	if(all(is.na(g)))
		g <- g0
	
	# set up vertex sizes
	E(g)$weight <- E(g)$Duration
	btw <- betweenness(graph=g, directed=FALSE, weights=reverse.weights(E(g)$weight), normalized=FALSE)
	#vsizes <- btw/max(btw) * 8 + 2
	nbtw <- (btw - min(btw)) / (max(btw) - min(btw))
	#vsizes <- sqrt(-nbtw^2 + 2*nbtw) * 8 + 2
	vsizes <- lame.normalize(nbtw,exp=2) * 10000 + 750
#V(g)$size <- vsizes
#write_graph(graph=g, file=graph.file, format="graphml")
		
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
	
	# set up vertex colors
	vcols <- rep("LIGHTGREY",gorder(g))
	col.char.nbr <- 5 
	col.char.idx <- order(vsizes,decreasing=TRUE)[1:col.char.nbr]
	vcols[col.char.idx] <- get.palette(col.char.nbr)
	
	# set up edge widths
	ww <- E(g)$weight
	nww <- (ww - min(ww)) / (max(ww) - min(ww))
	ewidths <- lame.normalize(nww,exp=1) * 75 + 0.5
	
	# return result
	res <- list(
		vsizes=vsizes, vcols=vcols,
		vlabs=vlabs, vlabsizes=vlabsizes,
		nww=nww, ewidths=ewidths
	)
	return(res)
}	
	

	
	
###############################################################################
# Plots the static graph extracted from all the scenes. Both the unfiltered and
# filtered versions.
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
	
	# compute graphical parameters
	tmp <- compute.graphical.params()
	vsizes <- tmp$vsizes
	vlabs <- tmp$vlabs
	vlabsizes <- tmp$vlabsizes
	nww <- tmp$nww
	ewidths <- tmp$ewidths
	
	# get filtered graph
	idx.filtr <- which(!V(g)$Filtered)
	g.filtr <- delete_vertices(graph=g, v=which(V(g)$Filtered))
	el <- get.edgelist(g.filtr, names=FALSE)
	ww <- rep(1, gsize(g.filtr))
	#ww <- E(cmp)$weight
	lay.filtr <<- qgraph.layout.fruchtermanreingold(	# actually not used anymore
		edgelist=el, 
		vcount=gorder(g.filtr), 
		weight=ww, 
		area=10*(gorder(g.filtr)^2),repulse.rad=(gorder(g.filtr)^3.0)
	)
	
	# get filtered edges
	el <- as_edgelist(graph=g.filtr, names=TRUE)
	idx.efiltr <- get.edge.ids(g, c(t(el)))
	#does not work
	#el <- as_edgelist(graph=g, names=FALSE)
	#idx.efiltr <- which(el[,1] %in% idx.filtr & el[,2] %in% idx.filtr)
	
	# get vertex attributes
	attrs <- vertex_attr_names(graph=g)
	attrs <- setdiff(attrs, c(COL_NAME, COL_NAME_SHORT, "id", "name")) # COL_FREQ, COL_NAMED, COL_FILTERED
	attrs <- c(attrs, NA)
	
	# plot the graph alone, and also depending on each vertex attribute
	for(a in 1:length(attrs))
	{	# get attribute name
		attr <- attrs[a]
		if(is.na(attr))
			tlog(4,"Plotting without attribute (",a,"/",length(attrs),")")
		else
			tlog(4,"Plotting attribute ",attr," (",a,"/",length(attrs),")")
		
		# set up vertex colors
		if(is.na(attr))
		{	vcols <- rep("LIGHTGREY",gorder(g))
			col.char.nbr <- 5 
			col.char.idx <- order(vsizes,decreasing=TRUE)[1:col.char.nbr]
			vcols[col.char.idx] <- get.palette(col.char.nbr)
		}
		# numeric attribute
		else if(attr==COL_FREQ)
		{	rvals <- vertex_attr(graph=g, name=attr)
			#vals <- rvals									# linear scale
			vals <- log(rvals+1)
			fine <- 500 									# granularity of the color gradient
			pal <- viridis									# extreme colors of the gradient
			#pal <- colorRampPalette(c("YELLOW","RED"))		# extreme colors of the gradient
			finite <- !is.infinite(vals)
			vcols[finite] <- pal(fine)[as.numeric(cut(vals[finite],breaks=fine))]
			vcols[!finite] <- "#575757"						# infinite values are grey
		}
		# categorical attribute
		else
		{	vals <- vertex_attr(graph=g, name=attr)
			uvals <- sort(unique(vals))
			col.nbr <- length(uvals)
			if(attr=="Sex")
				pal <- SEX_COLORS_4[uvals]
			else
				pal <- get.palette(col.nbr)
			names(pal) <- uvals
			vcols <- pal[as.character(vals)]
			names(vcols) <- V(g)$name
		}
		
		# set up edge colors
		el <- as_edgelist(graph=g, names=FALSE)
		ecols <- sapply(1:nrow(el), function(r) combine.colors(col1=vcols[el[r,1]], col2=vcols[el[r,2]]))
		ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],85*(1-lame.normalize(nww[i],exp=3))))
		
		# plot whole unfiltered graph
		graph.file <- get.path.graph.file(mode="scenes", filtered=FALSE)
		if(!is.na(attr))
			graph.file <- paste0(graph.file, "_attr=", attr)
		tlog(6,"Plotting the whole unfiltered graph in file ",graph.file)
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
				if(!is.na(attr))
				{	# numeric attribute
					if(attr==COL_FREQ)
					{	# size
						width <- 100
						height <- 600
						# colors
						lvals <- log(1:fine+1)
						lvals <- (lvals-min(lvals))/(max(lvals-min(lvals)))*fine
						lcols <- pal(fine)[lvals]
						# position
						x1 <- min(LAYOUT[,1])
						x2 <- x1 + width
						y2 <- min(LAYOUT[,2])
						y1 <- y2 + height
						leg.loc <- cbind(x=c(x1, x2, x2, x1), y=c(y1, y1, y2, y2))
						# draw
						legend.gradient(
							pnts=leg.loc,			# position
							cols=lcols,				# color gradient
							limits=sprintf("%.2f", range(rvals[finite],na.rm=TRUE)),
							title=attr,				# title of the legend box
							cex=2.5					# size of the text in the legend
						)
					}
					# categorical attribute
					else
					{	legend(
							title=attr,				# title of the legend box
							x="bottomleft",			# position
							legend=uvals,			# text of the legend
							fill=pal,				# color of the nodes
							bty="n",				# no box around the legend
							cex=2.5					# size of the text in the legend
						)
					}
				}
			dev.off()
		}
		
		# plot whole filtered graph
		if(is.na(attr) || attr!=COL_FILTERED)
		{	graph.file <- get.path.graph.file(mode="scenes", filtered=TRUE)
			if(!is.na(attr))
				graph.file <- paste0(graph.file, "_attr=", attr)
			tlog(6,"Plotting the whole filtered graph in file ",graph.file)
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
					if(!is.na(attr))
					{	# numeric attribute
						if(attr==COL_FREQ)
						{	# size
							width <- 100
							height <- 400
							# colors
							lvals <- log(1:fine+1)
							lvals <- (lvals-min(lvals))/(max(lvals-min(lvals)))*fine
							lcols <- pal(fine)[lvals]
							# position
							#x1 <- min(LAYOUT[,1])
							#y2 <- min(LAYOUT[,2])
							x1 <- 600					# specific position for filtered graph 
							y2 <- -1350
							x2 <- x1 + width
							y1 <- y2 + height
							leg.loc <- cbind(x=c(x1, x2, x2, x1), y=c(y1, y1, y2, y2))
							# draw
							legend.gradient(
								pnts=leg.loc,			# position
								cols=lcols,				# color gradient
								limits=sprintf("%.2f", range(rvals[intersect(idx.filtr,finite)],na.rm=TRUE)),
								title=attr,				# title of the legend box
								cex=2.5					# size of the text in the legend
							)
						}
						# categorical attribute
						else
						{	legend(
								title=attr,				# title of the legend box
								#x="bottomleft",			# position
								x=600, y=-900,		# adjusted for the filtered version of the graph 
								legend=uvals,			# text of the legend
								fill=pal,				# color of the nodes
								bty="n",				# no box around the legend
								cex=2.5					# size of the text in the legend
							)
						}
					}
				dev.off()
			}
		}
	}
	
	# plot each volume separately (full graph but different vertex colors)
	vols.folder <- file.path(NET_SCENES_FOLDER, "volumes")
	dir.create(path=vols.folder, showWarnings=FALSE, recursive=TRUE)
	tlog(4,"Plotting volume-related graphs using vertex colors")
	for(v in 1:length(data$char.volumes))
	{	vname <- data$volume.stats[v,COL_VOLUME]
		tlog(6,"Plotting volume ",vname," (",v,"/",length(data$char.volumes),")")
		idx <- match(data$char.volumes[[v]], data$char.stats[,COL_NAME])
		el <- as_edgelist(graph=g, names=FALSE)
		idx.e <- which(el[,1] %in% idx & el[,2] %in% idx)
		vcols <- rep("LIGHTGRAY", nrow(data$char.stats))
		vcols[idx] <- "RED"
		ecols <- rep("LIGHTGREY",gsize(g))
		ecols[idx.e] <- "RED"
		ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],85*(1-lame.normalize(nww[i],exp=3))))
		
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
					main=paste0(vname, " - ", data$volume.stats[v,COL_TITLE], " (",v,"/",nrow(data$volume.stats),")")
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
					main=paste0(vname, " - ", data$volume.stats[v,COL_TITLE], " (",v,"/",nrow(data$volume.stats),")")
				)
			dev.off()
		}
	}
	
	# plot each arc separately (full graph but different vertex colors)
	tlog(4,"Plotting arc-related graphs using vertex colors")
	arcs.folder <- file.path(NET_SCENES_FOLDER, "arcs")
	dir.create(path=arcs.folder, showWarnings=FALSE, recursive=TRUE)
	arc.titles <- unique(data$volume.stats[,COL_ARC])
	for(a in 1:length(data$char.arcs))
	{	tlog(6,"Plotting arc ",a,"/",length(data$char.arcs))
		idx <- match(data$char.arcs[[a]], data$char.stats[,COL_NAME])
		el <- as_edgelist(graph=g, names=FALSE)
		idx.e <- which(el[,1] %in% idx & el[,2] %in% idx)
		vcols <- rep("LIGHTGRAY", nrow(data$char.stats))
		vcols[idx] <- "RED"
		ecols <- rep("LIGHTGREY",gsize(g))
		ecols[idx.e] <- "RED"
		ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],85*(1-lame.normalize(nww[i],exp=3))))
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
	
	# get default colors
	tmp <- compute.graphical.params()
	vs0 <- tmp$vsizes
	
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
	vsizes <- lame.normalize(nbtw,exp=3) * 6000 + 750
	vsizes[idx.con] <- vsizes[idx.con] + 2000
	
	# set up vertex colors
	vcols <- rep(make.color.transparent("LIGHTGREY",80),gorder(g))
	vfcols <- rep(make.color.transparent("BLACK",80),gorder(g))
	vcols[idx.con] <- rep("LIGHTGREY",length(idx.con))
	vfcols[idx.con] <- rep("BLACK",length(idx.con))
	# main characters
	col.char.nbr <- 5 
	col.char.idx <- order(vs0,decreasing=TRUE)[1:col.char.nbr]
	vcols[col.char.idx] <- get.palette(col.char.nbr)
	
	# set up vertex labels
	vlabs <- rep(NA, gorder(g))
	vlabs[idx.con] <- sapply(idx.con, function(i) if(V(g)$ShortName[i]=="") V(g)$name[i] else V(g)$ShortName[i])
	vlabsizes <- vsizes*0.0003
	
	# set up edge widths
	ww <- E(g)$weight
	nww <- (ww - min(ww)) / (max(ww) - min(ww))
	ewidths <- lame.normalize(nww,exp=1) * 25 + 1
	
	# set up edge colors
	el <- as_edgelist(graph=g, names=FALSE)
	# all grey
	#ecols <- rep("GRAY",gsize(g))
	#ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],60))
	# using vertex color
	ecols <- sapply(1:nrow(el), function(r) combine.colors(col1=vcols[el[r,1]], col2=vcols[el[r,2]]))
	ecols <- sapply(1:length(ecols), function(i) make.color.transparent(ecols[i],80*(1-lame.normalize(nww[i],exp=3))))
	
	# get filtered edges
	el <- as_edgelist(graph=g.filtr, names=TRUE)
	idx.efiltr <- get.edge.ids(g, c(t(el)))
	#does not work
	#el <- as_edgelist(graph=g, names=FALSE)
	#idx.efiltr <- which(el[,1] %in% idx.filtr & el[,2] %in% idx.filtr)
	
	# plot unfiltered graph
	plot.file <- get.path.graph.file(mode="scenes", arc=arc, vol=vol, filtered=FALSE)
	tlog(4,"Plotting the selected unfiltered graph in file ",plot.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
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
	plot.file <- get.path.graph.file(mode="scenes", arc=arc, vol=vol, filtered=TRUE)
	tlog(4,"Plotting the selected filtered graph in file ",plot.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
			plot(g.filtr, 
				layout=LAYOUT[idx.filtr,],	# lay.filtr
				vertex.size=vsizes[idx.filtr], vertex.color=vcols[idx.filtr],
				vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsizes[idx.filtr],
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				edge.color=ecols[idx.efiltr], edge.width=ewidths[idx.efiltr], 
				rescale=FALSE, #frame=TRUE, #axe=TRUE, 
				xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2])
			)
		dev.off()
	}
	
	# plot filtered graph with custom layout
	#ww <- E(g.filtr)$Duration
	ww <- rep(1, gsize(g.filtr))
	el <- get.edgelist(g.filtr, names=FALSE)
	lay.filtr <<- qgraph.layout.fruchtermanreingold(	# actually not used anymore
		edgelist=el, 
		vcount=gorder(g.filtr), 
		weight=ww, 
		area=10*(gorder(g.filtr)^2), repulse.rad=(gorder(g.filtr)^3.1)
	)
	lay.filtr <- lay.filtr*100
	
	plot.file <- get.path.graph.file(mode="scenes", arc=arc, vol=vol, filtered=TRUE, subfold="layout")
	tlog(4,"Plotting the selected filtered graph in file ",plot.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
			plot(g.filtr, 
				layout=lay.filtr,
				vertex.size=vsizes[idx.filtr], vertex.color=vcols[idx.filtr],
				vertex.label=vlabs[idx.filtr], vertex.label.cex=vlabsizes[idx.filtr],
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color="BLACK",
				edge.color=ecols[idx.efiltr], edge.width=ewidths[idx.efiltr], 
				rescale=FALSE, #frame=TRUE, axe=TRUE, 
				xlim=range(lay.filtr[,1]), ylim=range(lay.filtr[,2])
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
	arc.titles <- unique(data$volume.stats[,COL_ARC])
	for(arc in 1:length(arc.titles))
		plot.static.graph.scenes.partial(data, arc=arc)
	
	# same for each volume
	volume.nbr <- nrow(data$volume.stats)
	for(v in 1:volume.nbr)
	{	vol <- data$volume.stats[v, COL_VOLUME]
		plot.static.graph.scenes.partial(data, vol=vol)
	}
	
	tlog(1,"Plotting of the static graphs complete")
}
