# Identifies and visualize the vertices and paths corresponding to the network
# diameter(s).
#
# Vincent Labatut
# 02/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/diameters.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="Diameters")




###############################################################################
# Highlights the specified paths in the specified graph.
#
# g: graph to process.
# paths: list of paths to highlight.
#
# returns: list of graphical parameters to use when plotting.
###############################################################################
highlight.paths <- function(g, paths)
{	# init graphical params
	vshapes <- rep("circle", gorder(g))
	outline.cols <- rep("BLACK", gorder(g))
	ecols <- rep("GRAY", gsize(g))
	if(is.null(E(g)$weight)) ewidths <- rep(1,gsize(g)) else ewidths <- E(g)$weight
	
	# possibly turn single path into list
	if(!is.list(paths))
		paths <- list(paths)
	
	# process each path
	for(path in paths)
	{	v <- NA
		for(n in path)
		{	if(is.na(v))
			{	v <- n
				outline.cols[v] <- "RED"
				vshapes[v] <- "csquare"
			}
			else
			{	u <- v
				v <- n
				outline.cols[v] <- "RED"
				idx <- as.integer(E(g)[u %--% v])
				g <- delete_edges(g,idx)
				g <- add_edges(g, c(u,v))
				ecols <- ecols[-idx]
				ecols <- c(ecols, "RED")
				width <- ewidths[idx]
				ewidths <- ewidths[-idx]
				ewidths <- c(ewidths, max(3, width+1))
			}
		}
		outline.cols[v] <- "RED"
		vshapes[v] <- "csquare"
	}
	
	res <- list(g=g, vshapes=vshapes, outline.cols=outline.cols, ecols=ecols, ewidths=ewidths)
	return(res)
}




###############################################################################
# read the graph
graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
g <- read.graphml.file(file=graph.file)

# compute graphical parameters
tmp <- compute.graphical.params()
vsizes <- tmp$vsizes
vlabs <- tmp$vlabs
vlabsizes <- tmp$vlabsizes
nww <- tmp$nww
ewidths0 <- tmp$ewidths

# get filtered graph
idx.keep <- which(V(g)$Filter=="Keep")
g.filtr <- delete_vertices(graph=g, v=which(V(g)$Filter=="Discard"))
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

# compute diameter
diam <- diameter(g.filtr)
tlog(0,"Computing diameter: ",diam)
dd <- distances(graph=g.filtr)			# compute all inter-node distances
idx <- which(dd==diam, arr.ind=TRUE)	# retrieve pairs of nodes matching the diameter
idx <- idx[idx[,1]<idx[,2],,drop=FALSE]	# filter (each pair appears twice due to symmetric matrix)
tlog(0,"Number of vertex pairs: ",nrow(idx))
print(cbind(V(g.filtr)$name[idx[,1]],V(g.filtr)$name[idx[,2]]))

# compute diameter paths
tlog(0,"Computing diameter paths")
diam.paths <- lapply(1:nrow(idx), function(r) all_shortest_paths(graph=g.filtr, from=idx[r,1], to=idx[r,2])$res)

# plot diameters
tlog(0,"Plotting diameter paths")
for(pp in 1:length(diam.paths))
{	tlog(2,"Processing vertex pair ",pp,"/",length(diam.paths))
	if(pp==1)
		s <- -1
	else
		s <- 0
	
	q <- 1
	for(p in s:length(diam.paths[[pp]]))
	{	# draw absolutely all diameters on the same plot
		if(p==-1)
		{	tlog(4,"Plotting all diameters on the same graph")
			paths <- unlist(diam.paths, recursive=FALSE)
			plot.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=TRUE, subfold="diameters", pref="graph", suf="all_diameters")
			tlog(6,"Producing file ",plot.file)
			
		}
		# draw all diameter variants on the same plot
		else if(p==0)
		{	tlog(4,"Plotting all diameter variants at once")
			paths <- diam.paths[[pp]]
			plot.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=TRUE, subfold=file.path("diameters",paste0("nodepair_",pp)), pref="graph", suf=paste0("diameter_",pp))
			tlog(6,"Producing file ",plot.file)
		}
		# draw each diameter variant separately
		else
		{	tlog(4,"Plotting diameter variant ",p,"/",length(diam.paths[[pp]]))
			if(p==1 || !all(diam.paths[[pp]][[p]]==diam.paths[[pp]][[p-1]]))
			{	paths <- diam.paths[[pp]][[p]]
				plot.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=TRUE, subfold=file.path("diameters",paste0("nodepair_",pp)), pref="graph", suf=paste0("diameter_",pp,"_",q))
				tlog(6,"Producing file ",plot.file)
				q <- q + 1
			}
			else
				tlog(6,"Same path as before, nothing to plot (multiple edges?)")
		}
		
		# set graphical parameters
		tmp <- highlight.paths(g.filtr, paths=paths)
		g.filtr2 <- tmp$g
		vshapes <- tmp$vshapes
		outline.cols <- tmp$outline.cols
		ecols <- tmp$ecols
		ewidths <- tmp$ewidths
		vsizes <- rep(2500, gorder(g.filtr))
		vsizes[outline.cols=="RED"] <- rep(5000, length(which(outline.cols=="RED")))
		vcolors <- rep("GREY", gorder(g.filtr))
		vcolors[outline.cols=="RED"] <- rep("PINK", length(which(outline.cols=="RED")))
		vs <- match(unique(names(unlist(paths))),V(g)$name)
		vlabs2 <- vlabs
		vlabs2[vs] <- V(g)$ShortName[vs]
		vlabsizes2 <- vlabsizes
		vlabsizes2[vs] <- sapply(vlabsizes[vs], function(val) max(val, 1))
		
		# plot graph
		tlog(6,"Plotting all paths at once for vertex pair #",pp)
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white", width=40, height=40)
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=2000, height=2000, units="px", pointsize=20, bg="white")
			plot(g.filtr2,
				layout=LAYOUT[idx.keep,],	# lay.filtr
#				vertex.size=vsizes[idx.keep], vertex.color=vcols[idx.keep],
				vertex.size=vsizes, vertex.color=vcolors,
				vertex.shape=vshapes, vertex.frame.color=outline.cols,
				vertex.label=vlabs2[idx.keep], vertex.label.cex=vlabsizes2[idx.keep],
				vertex.label.family="sans",
				vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.color=outline.cols,
#				edge.color=ecols[idx.efiltr], edge.width=ewidths[idx.efiltr], 
				edge.color=ecols, edge.width=ewidths, 
				rescale=FALSE, #axe=TRUE, 
				xlim=range(LAYOUT[,1]), ylim=range(LAYOUT[,2])
			)
			dev.off()
		}
	}
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
