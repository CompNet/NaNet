# Functions that compute and plots the evolution of various topological measures over time,
# for a pre-computed sequence of igraph objects representing a dynamic graph.
# 
# Vincent Labatut
# 02/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/dynamic/evol_prop.R")
###############################################################################




###############################################################################
# Compute and plots the evolution of graph topological measures.
#
# gg: list of static graphs representing a dynamic graph.
# volume.stats: table containing the volume statistics.
# net.type: type of dynamic network ("instant", "cumulative", "narr_smooth").
# filtered: whether characters should be filtered or not.
# pub.order: whether to consider volumes in publication vs. story order.
# narr.unit: narrative unit used to extract the dynamic networks.
# plot.vols: whether to plot the volumes as rectangles.
###############################################################################
# compute the graph measures
evol.prop.graph <- function(gg, volume.stats, net.type, filtered, pub.order, narr.unit, plot.vols=TRUE)
{	tlog(2, "Computing the graph measures")
	filt.txt <- if(filtered) "filtered" else "unfiltered"
	sc.nbr <- length(gg)
	
	# order volumes
	if(pub.order)	# by publication order
	{	ord.vols <- 1:nrow(volume.stats)
		ord.fold <- "publication"
	}
	else			# by story order
	{	ord.vols <- (1:nrow(volume.stats))[order(volume.stats[,COL_RANK])]
		ord.fold <- "story"
	}
	
	# selected measures
	gr.meas <- c(
		paste0(MEAS_DEGREE,SFX_AVG),
		paste0(MEAS_STRENGTH,SFX_AVG),
		paste0(MEAS_BETWEENNESS,SFX_AVG),
		paste0(MEAS_BETWEENNESS,SFX_WEIGHT,SFX_AVG),
		paste0(MEAS_CLOSENESS,SFX_AVG),
		paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_AVG),
		paste0(MEAS_HARMO_CLOSENESS,SFX_AVG),
		paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_AVG),
		paste0(MEAS_EIGENCNTR,SFX_AVG),
		paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_AVG),
		paste0(MEAS_TRANSITIVITY,SFX_LOCAL,SFX_AVG),
		paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL,SFX_AVG),
		#
		MEAS_MODULARITY,
		paste0(MEAS_MODULARITY,SFX_WEIGHT),
		paste0(MEAS_COMMUNITY,SFX_NBR),
		paste0(MEAS_COMMUNITY,SFX_WEIGHT,SFX_NBR),
		#
		paste0(MEAS_DISTANCE,SFX_AVG),
		paste0(MEAS_DISTANCE,SFX_WEIGHT,SFX_AVG),
		paste0(MEAS_COMPONENT,SFX_NBR),
		paste0(MEAS_NODE,SFX_NBR),
		paste0(MEAS_LINK,SFX_NBR),
		paste0(MEAS_LINKWEIGHT,SFX_AVG),
		MEAS_DENSITY,
		paste0(MEAS_DENSITY,SFX_WEIGHT)
	)
	log.y <- c(
		paste0(MEAS_CLOSENESS,SFX_AVG), paste0(MEAS_CLOSENESS,SFX_WEIGHT,SFX_AVG),
		MEAS_DENSITY, paste0(MEAS_DENSITY,SFX_WEIGHT),
		paste0(MEAS_EIGENCNTR,SFX_AVG), paste0(MEAS_EIGENCNTR,SFX_WEIGHT,SFX_AVG),
		paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT,SFX_AVG)
	)
	gr.stats <- matrix(NA, nrow=sc.nbr, ncol=length(gr.meas), dimnames=list(c(),gr.meas))
	
	# color palette
	pal <- ATT_COLORS_FILT
	col <- if(filtered) pal["Keep"] else pal["Discard"]
	
	# compute each measure
	tlog.start.loop(4, length(gr.meas), "Looping over graph measures")
	for(m in 1:length(gr.meas))
	{	meas <- gr.meas[m]
		tlog.loop(6, m, "Computing measure ",meas," (",m,"/",length(gr.meas),")")
		
		# compute the measure for each time slice
		for(i in 1:length(gg))
		{	cache <<- list()
			gr.stats[i,m] <- GRAPH_MEASURES[[meas]]$foo(gg[[i]])
		}
		
		# compute y range
		tmp <- gr.stats[,m]
		tmp <- tmp[!is.na(tmp) & !is.nan(tmp) & !is.infinite(tmp)]
		ylim <- range(tmp)
		ylim[2] <- ylim[2]*1.1	# add some space for volume names
		
		# plot the measure
		plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type=net.type, order=ord.fold, meas.name=meas, filtered=filt.txt, subfold=narr.unit)
		tlog(8, "Plotting in file \"",plot.file,"\"")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white", width=15, height=5)
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=2400, height=800, units="px", pointsize=20, bg="white")
			par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
			# init empty plot
			plot(
				NULL, 
#				ylim=GRAPH_MEASURES[[meas]]$bounds, 
				xlim=c(1,sc.nbr), ylim=ylim,
				log=if(meas %in% log.y) "y" else "",
				xlab="Scenes", ylab=GRAPH_MEASURES[[meas]]$cname
			)
			# possibly add volume representations
			if(plot.vols)
				draw.volume.rects(ylim, volume.stats[ord.vols,])
			# add line
			lines(
				x=1:sc.nbr, y=gr.stats[,m], 
				#xlab="Scenes", ylab=GRAPH_MEASURES[[meas]]$cname,
				col=col
			)
			# close file
			dev.off()
		}
	}
	
	# record the stats as a CSV file
	tab.file <- paste0(get.path.stats.topo(mode="scenes", char.det="implicit", net.type=net.type, order=ord.fold, filtered=filt.txt, subfold=narr.unit),".csv")
	write.csv(x=gr.stats, file=tab.file, fileEncoding="UTF-8", row.names=TRUE)
	
	tlog.end.loop(4, "Computation of graph measures over")
}




###############################################################################
# Compute and plots the evolution of vertex topological measures.
#
# gg: list of static graphs representing a dynamic graph.
# vtx.plot: characters of interest whose edges must be plot.
# char.stats: list of characters with their attributes.
# volume.stats: table containing the volume statistics.
# net.type: type of dynamic network ("instant", "cumulative", "narr_smooth").
# filtered: whether characters should be filtered or not.
# pub.order: whether to consider volumes in publication vs. story order.
# narr.unit: narrative unit used to extract the dynamic networks.
# plot.vols: whether to plot the volumes as rectangles.
###############################################################################
evol.prop.vertices <- function(gg, vtx.plot, char.stats, volume.stats, net.type, filtered, pub.order, narr.unit, plot.vols=TRUE)
{	# compute the vertex measures
	tlog(2, "Computing the vertex measures")
	filt.txt <- if(filtered) "filtered" else "unfiltered"
	sc.nbr <- length(gg)
	
	# character names
	char.names <- data$char.stats[,COL_NAME]
	char.shortnames <- data$char.stats[,COL_NAME_SHORT]
	
	# order volumes
	if(pub.order)	# by publication order
	{	ord.vols <- 1:nrow(volume.stats)
		ord.fold <- "publication"
	}
	else			# by story order
	{	ord.vols <- (1:nrow(volume.stats))[order(volume.stats[,COL_RANK])]
		ord.fold <- "story"
	}
	
	# selected measures
	vx.meas <- c(
		MEAS_DEGREE,
		MEAS_STRENGTH,
		paste0(MEAS_BETWEENNESS,SFX_WEIGHT),
		MEAS_CLOSENESS,
		paste0(MEAS_CLOSENESS,SFX_WEIGHT),
		MEAS_HARMO_CLOSENESS,
		paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT),
		MEAS_EIGENCNTR,
		paste0(MEAS_EIGENCNTR,SFX_WEIGHT),
		paste0(MEAS_TRANSITIVITY,SFX_LOCAL),
		paste0(MEAS_TRANSITIVITY,SFX_WEIGHT,SFX_LOCAL)
	)
	log.y <- c(
		MEAS_CLOSENESS, paste0(MEAS_CLOSENESS,SFX_WEIGHT),
		paste0(MEAS_HARMO_CLOSENESS,SFX_WEIGHT)
	)
	vs.stats <- list()
	
	# build character list
	tmp <- t(combn(vtx.plot,2))									# all pairs of chars
	vss <- lapply(seq_len(nrow(tmp)), function(i) tmp[i,])		# convert to a list
	vss[[length(vss)+1]] <- vtx.plot							# keep the full selection
	# color palette
	pal <- get.palette(length(vtx.plot))
	names(pal) <- vtx.plot
	
	# compute each measure
	tlog.start.loop(4, length(vx.meas), "Looping over vertex measures")
	for(m in 1:length(vx.meas))
	{	meas <- vx.meas[m]
		tlog.loop(6, m, "Computing measure ",meas," (",m,"/",length(vx.meas),")")
		
		# init stat matrix
		mat <- matrix(NA, nrow=sc.nbr, ncol=length(char.names), dimnames=list(c(),char.names))
		
		# compute the measure for each time slice
		for(i in 1:length(gg))
		{	cache <<- list()
			if(gorder(gg[[i]])>0)
			{	vals <- NODE_MEASURES[[meas]]$foo(gg[[i]])
				mat[i,V(gg[[i]])$name] <- vals
			}
		}
		# record the stats as a CSV file
		tab.file <- paste0(get.path.stats.topo(mode="scenes", char.det="implicit", net.type=net.type, order=ord.fold, meas.name=meas, filtered=filt.txt, subfold=narr.unit),".csv")
		write.csv(x=mat, file=tab.file, fileEncoding="UTF-8", row.names=TRUE)
		# add to list of matrices
		vs.stats[[meas]] <- mat
		
		for(vs in vss)
		{	# compute y range
			tmp <- c(mat[,vs])
			tmp <- tmp[!is.na(tmp) & !is.nan(tmp) & !is.infinite(tmp)]
			ylim <- range(tmp)
			
			# get short names
			sn <- char.shortnames[match(vs,char.names)]
			if(length(sn)==2)
				pt <- paste(sn,collapse="_")
			else
				pt <- "_main_chars"
			
			# plot the measure
			plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type=net.type, order=ord.fold, meas.name=meas, filtered=filt.txt, subfold=narr.unit, suf=pt)
			tlog(8, "Plotting in file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white", width=15, height=5)
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=2400, height=800, units="px", pointsize=20, bg="white")
				par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
				# init empty plot
				plot(
					NULL, 
					ylim=ylim, xlim=c(1,sc.nbr),
					log=if(meas %in% log.y) "y" else "",
					xlab="Scenes", ylab=NODE_MEASURES[[meas]]$cname
				)
				# possibly add volume representations
				if(plot.vols)
					draw.volume.rects(ylim, volume.stats[ord.vols,])
				# add a serie for each character
				for(v in 1:length(vs))
				{	lines(
						x=1:sc.nbr, y=mat[,vs[v]], 
						col=pal[vs][v],
						type="l"
					)
				}
				# add legend
				legend(
					x="topright",
					fill=pal[vs],
					legend=sn
				)
				dev.off()
			}
		}
	}
	tlog.end.loop(4, "Computation of vertex measures over")
}




###############################################################################
# Compute and plots the evolution of edge tpological measures.
#
# gg: list of static graphs representing a dynamic graph.
# vtx.plot: characters of interest whose edges must be plot.
# char.stats: list of characters with their attributes.
# volume.stats: table containing the volume statistics.
# net.type: type of dynamic network ("instant", "cumulative", "narr_smooth").
# filtered: whether characters should be filtered or not.
# pub.order: whether to consider volumes in publication vs. story order.
# narr.unit: narrative unit used to extract the dynamic networks.
# plot.vols: whether to plot the volumes as rectangles.
###############################################################################
evol.prop.edges <- function(gg, vtx.plot, char.stats, volume.stats, net.type, filtered, pub.order, narr.unit, plot.vols=TRUE)
{	# compute the edge measures
	tlog(2, "Computing the edge measures")
	filt.txt <- if(filtered) "filtered" else "unfiltered"
	sc.nbr <- length(gg)
	
	# character names
	char.names <- data$char.stats[,COL_NAME]
	char.shortnames <- data$char.stats[,COL_NAME_SHORT]
	
	# order volumes
	if(pub.order)	# by publication order
	{	ord.vols <- 1:nrow(volume.stats)
		ord.fold <- "publication"
	}
	else			# by story order
	{	ord.vols <- (1:nrow(volume.stats))[order(volume.stats[,COL_RANK])]
		ord.fold <- "story"
	}
	
	# selected measures
	ed.meas <- c(
		MEAS_LINKWEIGHT
	)
	log.y <- c()
	es.stats <- list()
	
	# edges of interest
	edg.plot <- t(combn(vtx.plot,2))
	e.names <- apply(edg.plot, 1, function(row) paste(row,collapse="--"))
	e.snames <- apply(edg.plot, 1, function(row) paste(char.shortnames[match(row,char.names)],collapse="--"))
	# build edge list
	ess <- list()
	ess <- c(ess, 1:nrow(edg.plot))
	for(v in vtx.plot)
	{	idx <- which(apply(edg.plot, 1, function(row) any(row==v)))
		ess[[length(ess)+1]] <- idx
	}
	# color palette
	pal <- get.palette(length(vtx.plot))
	names(pal) <- vtx.plot
	e.pal <- sapply(1:nrow(edg.plot), function(r) combine.colors(col1=pal[edg.plot[r,1]],col2=pal[edg.plot[r,2]]))
	names(e.pal) <- e.names
	
	# compute each measure (only for the selected edges, otherwise too many values)
	tlog.start.loop(4, length(ed.meas), "Looping over edge measures")
	for(m in 1:length(ed.meas))
	{	meas <- ed.meas[m]
		tlog.loop(6, m, "Computing measure ",meas," (",m,"/",length(ed.meas),")")
		
		# init stat matrix
		mat <- matrix(NA, nrow=sc.nbr, ncol=length(e.names), dimnames=list(c(),e.names))
		
		# compute the measure for each time slice
		for(i in 1:length(gg))
		{	cache <<- list()
			vals <- LINK_MEASURES[[meas]]$foo(gg[[i]])
			enm <- as_edgelist(graph=gg[[i]], names=TRUE)
			idx <- sapply(1:nrow(edg.plot), function(r) 
			{	idx <- which(enm[,1]==edg.plot[r,1] & enm[,2]==edg.plot[r,2] 
								| enm[,1]==edg.plot[r,2] & enm[,2]==edg.plot[r,1])
				if(length(idx)==0)
					res <- NA
				else
					res <- idx[1]
				return(res)
			})
			mat[i,!is.na(idx)] <- vals[idx[!is.na(idx)]]
		}
		# record the stats as a CSV file
		tab.file <- paste0(get.path.stats.topo(mode="scenes", char.det="implicit", net.type=net.type, order=ord.fold, meas.name=meas, filtered=filt.txt, subfold=narr.unit),".csv")
		write.csv(x=mat, file=tab.file, fileEncoding="UTF-8", row.names=TRUE)
		# add to list of matrices
		es.stats[[meas]] <- mat
		
		for(es in ess)
		{	# compute y range
			tmp <- c(mat[,e.names[es]])
			tmp <- tmp[!is.na(tmp) & !is.nan(tmp) & !is.infinite(tmp)]
			ylim <- range(tmp)
			
			# get short names
			if(length(es)==1)
				pt <- e.snames[es]
			else
			{	common <- which(sapply(vtx.plot, function(vname) all(grepl(vname, e.names[es], fixed=TRUE))))
				pt <- char.shortnames[match(vtx.plot[common],char.names)]
			}
			
			# plot the measure
			plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type=net.type, order=ord.fold, meas.name=meas, filtered=filt.txt, subfold=narr.unit, suf=pt)
			tlog(8, "Plotting in file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white", width=15, height=5)
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=2400, height=800, units="px", pointsize=20, bg="white")
				par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
				# init empty plot
				plot(
					NULL, 
					ylim=ylim, xlim=c(1,sc.nbr),
					log=if(meas %in% log.y) "y" else "",
					xlab="Scenes", ylab=LINK_MEASURES[[meas]]$cname
				)
				# possibly add volume representations
				if(plot.vols)
					draw.volume.rects(ylim, volume.stats[ord.vols,])
				# add a serie for each edge
				for(e in 1:length(es))
				{	lines(
						x=1:sc.nbr, y=mat[,e.names[es[e]]], 
						col=e.pal[es[e]],
						type="l"
					)
				}
				# add legend
				if(length(es)>1)
				{	legend(
						x="topright",
						fill=e.pal[es],
						legend=e.snames[es]
					)
				}
				dev.off()
			}
		}
	}
	tlog.end.loop(4, "Computation of edge measures over")
}
