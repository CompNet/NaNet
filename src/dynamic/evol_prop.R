# Computes and plots the evolution of various topolofical measures over time,
# for a pre-computed sequence of igraphs representing a dynamic graph.
# 
# Vincent Labatut
# 02/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/dynamic/evol_prop.R")
###############################################################################
source("src/common/include.R")




###############################################################################
# load the graphs
tlog(0, "Loading the dynamic graph as a sequence of static graphs")

filtred <- TRUE

base.file <- get.path.graph.file(mode="scenes", filtered=filtered, subfold="narr_smooth")
gg <- list()
go.on <- TRUE
s <- 1
while(go.on)
{	graph.file <- paste0(base.file,"_s",s,".graphml")
	if(file.exists(graph.file))
	{	tlog(2, "Reading file ",graph.file)
		
		# read graph
		g <- read.graph(file=graph.file, format="graphml")
		V(g)$name <- fix.encoding(strings=V(g)$name)
		V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
		char.names <- V(g)$name
		char.shortnames <- V(g)$ShortName
		
		# remove isolates and store
		isolates <- which(degree(g)==0)
		gg[[s]] <- delete_vertices(g, isolates)
		
		s <- s + 1
	}
	else
		go.on <- FALSE
}
sc.nbr <- length(gg)




###############################################################################
# tests
mean(strength(graph=gg[[400]]))



###############################################################################
# compute the graph measures
tlog(0, "Computing the graph measures")
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
	paste0(MEAS_DISTANCE,SFX_AVG),
	paste0(MEAS_COMPONENT,SFX_NBR),
	paste0(MEAS_NODE,SFX_NBR),
	paste0(MEAS_LINK,SFX_NBR),
	paste0(MEAS_LINKWEIGHT,SFX_AVG),
	MEAS_DENSITY,
	paste0(MEAS_DENSITY,SFX_WEIGHT)
)
gr.stats <- matrix(NA, nrow=sc.nbr, ncol=length(gr.meas), dimnames=list(c(),gr.meas))

# compute each measure
tlog.start.loop(2, length(gr.meas), "Looping over graph measures")
for(m in 1:length(gr.meas))
{	meas <- gr.meas[m]
	tlog.loop(4, m, "Computing measure ",meas," (",m,"/",length(gr.meas),")")
	
	# compute the measure for each time slice
	for(i in 1:length(gg))
	{	cache <<- list()
		gr.stats[i,m] <- GRAPH_MEASURES[[meas]]$foo(gg[[i]])
	}
	
	# plot the measure
	plot.file <- get.path.topomeas.plot(object="graph", mode="scenes", meas.name=meas, filtered=filtered, subfold="narr_smooth/graph")
	tlog(6, "Plotting in file \"",plot.file,"\"")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white", width=15, height=5)
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=2400, height=800, units="px", pointsize=20, bg="white")
		par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		plot(
			x=1:sc.nbr, y=gr.stats[,m], 
#			ylim=GRAPH_MEASURES[[meas]]$bounds, 
			xlab="Scenes", ylab=GRAPH_MEASURES[[meas]]$cname,
			col=MAIN_COLOR,
			type="l"
		)
		dev.off()
	}
}
tlog.end.loop(2, "Computation of graph measures over")




###############################################################################
# compute the vertex measures
tlog(0, "Computing the vertex measures")
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
vs.stats <- list()

# vertices of interest
vtx.plot <- c("Thorgal Aegirsson","Aaricia Ganfalfsdottir","Kriss de Valnor","Jolan Thorgalsson","Louve")
# build character list
tmp <- t(combn(vtx.plot,2))								# all pairs of chars
vss <- lapply(seq_len(nrow(tmp)), function(i) tmp[i,])	# convert to a list
vss[[length(vss)+1]] <- vtx.plot							# keep the full selection
# color palette
pal <- get.palette(length(vtx.plot))
names(pal) <- vtx.plot

# compute each measure
tlog.start.loop(2, length(gr.meas), "Looping over vertex measures")
for(m in 1:length(vx.meas))
{	meas <- vx.meas[m]
	tlog.loop(4, m, "Computing measure ",meas," (",m,"/",length(vx.meas),")")
	
	# init stat matrix
	mat <- matrix(NA, nrow=sc.nbr, ncol=length(char.names), dimnames=list(c(),char.names))
	
	# compute the measure for each time slice
	for(i in 1:length(gg))
	{	cache <<- list()
		vals <- NODE_MEASURES[[meas]]$foo(gg[[i]])
		mat[i,V(gg[[i]])$name] <- vals
	}
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
		plot.file <- get.path.topomeas.plot(object="nodes", mode="scenes", meas.name=meas, filtered=filtered, subfold=paste0("narr_smooth/nodes/",pt))
		tlog(6, "Plotting in file \"",plot.file,"\"")
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
				xlab="Scenes", ylab=NODE_MEASURES[[meas]]$cname
			)
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
tlog.end.loop(2, "Computation of vertex measures over")




###############################################################################
# compute the edge measures
tlog(0, "Computing the edge measures")
ed.meas <- c(
	MEAS_LINKWEIGHT
)
es.stats <- list()

# edges of interest
vtx.plot <- c("Thorgal Aegirsson","Aaricia Ganfalfsdottir","Kriss de Valnor","Jolan Thorgalsson","Louve")
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
tlog.start.loop(2, length(gr.meas), "Looping over edge measures")
for(m in 1:length(ed.meas))
{	meas <- ed.meas[m]
	tlog.loop(4, m, "Computing measure ",meas," (",m,"/",length(ed.meas),")")
	
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
		plot.file <- get.path.topomeas.plot(object="links", mode="scenes", meas.name=meas, filtered=filtered, subfold="narr_smooth/links/", plot.type=pt)
		tlog(6, "Plotting in file \"",plot.file,"\"")
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
				xlab="Scenes", ylab=LINK_MEASURES[[meas]]$cname
			)
			# add a serie for each edge
			for(e in 1:length(es))
			{	lines(
					x=1:sc.nbr, y=mat[,e.names[e]], 
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
tlog.end.loop(2, "Computation of vertex measures over")

# TODO add bands to represent volumes or arcs?
