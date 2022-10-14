# Additional plots regarding the evolution of the giant component size
# as a function of the number of vertices. We remove each vertex
# one by one, by order of decreasing degree (in the remaining graph).
# 
# Vincent Labatut
# 01/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/comp_vs_vertices.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="CompVsVertices")




###############################################################################
# compute results and plot separate figures

# read the graph
graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
tlog(0, "Reading graph ",graph.file)
g <- read.graphml.file(file=graph.file)

# filter the characters
tlog(0, "Filtering characters")
filt.names <- V(g)$name[V(g)$Filter=="Discard"]
if(length(filt.names)==0) stop("Empty list of filtered characters")
g.filt <- delete_vertices(g, V(g)$Filter=="Discard")

# plot settings
xlab <- "Number of vertices removed"

# compute and plot
tlog(0, "Compute values and plot basic figures")
pal <- ATT_COLORS_FILT[c("Discard","Keep")]
res <- list()
# remove each vertex iteratively
tlog(4, "Removing vertices iteratively for the unfiltered graph")
g0 <- g
props <- c()
nbrs <- c()
tlog(6, "it 0. \tSize of the largest component: ",max(components(g0)$csize),"\tNumber of components: ",components(g0)$no)
for(e in 1:(gorder(g0)-1))
{	# remove the vertex of largest degree
	degs <- igraph::degree(g0, mode="all")
	idx <- which.max(degs)
	nm <- V(g0)$name[idx]
	g0 <- delete_vertices(graph=g0, v=idx)
	
	tmp <- components(g0)
	# compute the size of the giant component
	size <- max(tmp$csize)
	props <- c(props, size)
	# and number of components
	nbr <- tmp$no
	nbrs <- c(nbrs, nbr)
	
	tlog(6, "it ",e,". Removing vertex #",idx," (",nm,") with degree ",degs[idx],"\tSize of the largest component: ",size,"\tNumber of components: ",nbr)
}
res[["Unfiltered-Size"]] <- props
res[["Unfiltered-Nbr"]] <- nbrs
# plot size results
plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", meas.name=MEAS_MULTI_GRAPH, weights="none", filtered="unfiltered", suf="giant-comp-size_vs_nodes")
tlog(4, "Plotting largest component size in file ",plot.file)
ylab <- "Largest component size (proportion of vertices)"
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
		par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		plot(
			x=1:(gorder(g)-1), y=props/gorder(g),
			xlab=TeX(xlab), ylab=TeX(ylab),
			col=pal["Discard"],
			ylim=0:1, log="x",
			type="l"
		)
	dev.off()
}
# plot count results
plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", meas.name=MEAS_MULTI_GRAPH, weights="none", filtered="unfiltered", suf="comp-nbr_vs_nodes")
tlog(4, "Plotting component number in file ",plot.file)
ylab <- "Number of components"
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
	par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		plot(
			x=1:(gorder(g)-1), y=nbrs,
			xlab=TeX(xlab), ylab=TeX(ylab),
			col=pal["Discard"],
			#log="x",
			type="l"
		)
	dev.off()
}

# same for filtered graph 
tlog(4, "Removing vertices iteratively for the filtered graph")
g0 <- g.filt
props <- c()
nbrs <- c()
prev.comp <- rep(1, gorder(g0))
tlog(6, "it 0. \tSize of the largest component: ",max(components(g0)$csize),"\tNumber of components: ",components(g0)$no)
for(e in 1:(gorder(g0)-1))
{	# remove the vertex of largest degree
	degs <- igraph::degree(g0, mode="all")
	idx <- which.max(degs)
	nm <- V(g0)$name[idx]
	g0 <- delete_vertices(graph=g0, v=idx)
	prev.comp <- prev.comp[-idx]
	
	tmp <- components(g0)
	# compute the size of the giant component
	size <- max(tmp$csize)
	props <- c(props, size)
	# and number of components
	nbr <- tmp$no
	nbrs <- c(nbrs, nbr)
	
	tlog(6, "it ",e,". Removing vertex #",idx," (",nm,") with degree ",degs[idx],"\tSize of the largest component: ",size,"\tNumber of components: ",nbr)
	comp <- tmp$membership
	gc <- which.max(tmp$csize)
	tlog(8,"Changes compared to previous iteration:")
	tlog(10,"Cluster ",gc,": largest component (not shown), ",tmp$csize[gc]," vertices")
	for(c in 1:max(comp))
	{	if(c!=gc)
		{	memb.c <- sort(which(comp==c))
			equiv <- sapply(1:max(prev.comp), function(pc) 
					{	memb.pc <- sort(which(prev.comp==pc))
						return(length(memb.c)==length(memb.pc) && all(memb.c==memb.pc))
					})
			if(!any(equiv))
			{	tlog(10,"Cluster ",c,": ",tmp$csize[c]," vertices")
				idx <- which(comp==c)
				sapply(idx, function(i) tlog(12, V(g0)$name[i]))
			}
		}
	}
	prev.comp <- comp
	
#	for(c in 1:max(comp))
#	{	if(c==which.max(tmp$csize))
#			tlog(8,"Cluster ",c,": largest component (not shown), ",tmp$csize[c]," vertices")
#		else
#		{	tlog(8,"Cluster ",c,": ",tmp$csize[c]," vertices")
#			idx <- which(comp==c)
#			sapply(idx, function(i) tlog(10, V(g0)$name[i]))
#		}
#	}
}
res[["Filtered-Size"]] <- props
res[["Filtered-Nbr"]] <- nbrs
# plot size results
plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", meas.name=MEAS_MULTI_GRAPH, weights="none", filtered="filtered", suf="giant-comp-size_vs_nodes")
tlog(4, "Plotting largest component size in file ",plot.file)
ylab <- "Largest component size (proportion of vertices)"
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
		par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		plot(
			x=1:(gorder(g.filt)-1), y=props/gorder(g.filt),
			xlab=TeX(xlab), ylab=TeX(ylab),
			col=pal["Keep"],
			ylim=0:1, log="x",
			type="l"
		)
	dev.off()
}
# plot count results
plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", meas.name=MEAS_MULTI_GRAPH, weights="none", filtered="filtered", suf="comp-nbr_vs_nodes")
tlog(4, "Plotting component number in file ",plot.file)
ylab <- "Number of components"
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
	par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		plot(
			x=1:(gorder(g.filt)-1), y=nbrs,
			xlab=TeX(xlab), ylab=TeX(ylab),
			col=pal["Keep"],
			#log="x",
			type="l"
		)
	dev.off()
}




###############################################################################
# plot comparative figures

# both unfiltered and filtered in the same figure
tlog(0, "Plot figures combining unfiltered and filtered nets results")
plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", meas.name=MEAS_MULTI_GRAPH, weights="none", filtered="both", suf="giant-comp-size_vs_nodes")
tlog(4, "Plotting component size in file ",plot.file)
ylab <- "Largest component size (proportion of vertices)"
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
		par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		# unfiltered net results
		plot(
			x=1:(gorder(g)-1), y=res[["Unfiltered-Size"]]/gorder(g),
			xlab=TeX(xlab), ylab=TeX(ylab),
			col=pal["Discard"],
			ylim=0:1, log="x",
			type="l"
		)
		# filtered net results
		lines(
			x=1:(gorder(g.filt)-1), y=res[["Filtered-Size"]]/gorder(g.filt), 
			col=pal["Keep"] 
		)
		# legend
		legend(
			title="Characters",
			x="topright",
			fill=pal[c("Discard","Keep")],
			legend=c("Unfiltered","Filtered")
		)
	dev.off()
}

tlog(4, "Plotting component number in file ",plot.file)
plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", meas.name=MEAS_MULTI_GRAPH, weights="none", filtered="both", suf="comp-nbr_vs_nodes")
ylab <- "Number of components"
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
		par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		# unfiltered net results
		plot(
			x=1:(gorder(g)-1), y=res[["Unfiltered-Nbr"]],
			xlab=TeX(xlab), ylab=TeX(ylab),
			col=pal["Discard"],
			#log="x",
			type="l"
		)
		# filtered net results
		lines(
			x=1:(gorder(g.filt)-1), y=res[["Filtered-Nbr"]], 
			col=pal["Keep"] 
		)
		# legend
		legend(
			title="Characters",
			x="topleft",
			fill=pal[c("Discard","Keep")],
			legend=c("Unfiltered","Filtered")
		)
	dev.off()
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
