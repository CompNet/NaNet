# Identifies and visualize the vertices and paths corresponding to the network
# diameter(s).
#
# Vincent Labatut
# 02/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/diameters.R")
###############################################################################
source("src/common/include.R")
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
	ecols <- rep("BLACK", gsize(g))
	if(is.null(E(g)$weight)) ewidth <- rep(1,gsize(g)) else ewidth <- E(g)$weight
	
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
				ecols[idx] <- "RED"
				ewidth[idx] <- 2*E(g)$weight[idx]
			}
		}
		outline.cols[v] <- "RED"
		vshapes[v] <- "csquare"
	}
	
	res <- list(vshapes=vshapes, outline.cols=outline.cols, ecols=ecols, ewidth=ewidth)
	return(res)
}




###############################################################################
# read the graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
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
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml", filtered=TRUE, ext="_diameter_")
for(pp in 1:length(diam.paths))
{	tlog(2,"Plotting diameter ",pp,"/",length(diam.paths))
	
	tmp <- highlight.paths(g.filtr, paths=diam.paths[[pp]])
	# TODO modif pour utiliser des noms plutot que des indices
	
	
	
	q <- 1
	for(p in 1:length(diam.paths[[pp]]))
	{	tlog(4,"Plotting diameter ",p,"/",length(diam.paths[[pp]]),"\n",sep="")
		if(p==1 || !all(diam.paths[[pp]][[p]]==diam.paths[[pp]][[p-1]]))
		{	custom.gplot(g, paths=diam.paths[[pp]][[p]], file=paste0(graph.file,pp,"_",q))
			q <- q + 1
		}
	}
}








###############################################################################
# end logging
end.rec.log()
