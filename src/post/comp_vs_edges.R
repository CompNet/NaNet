# Ad hoc plots regarding the evolution of the giant component size
# as a function of the number of edges.
# 
# Vincent Labatut
# 01/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/comp_vs_edges.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="CompVsEdges")




###############################################################################

# read the graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
tlog(0, "Reading graph ",graph.file)
g <- read_graph(file=graph.file, format="graphml")
# clean names
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)

# filter the characters
tlog(0, "Filtering characters")
g.filt <- delete_vertices(g, V(g)$Filtered)

# plot settings
xlab <- "Number of edges"
ylab <- "Proportion of vertices"

# compute both types of weights
tlog(0, "Loop over weight types")
pal <- get.palette(2)
wts <- c("Duration","Occurrences")
for(wt in wts)
{	tlog(2, "Dealing with ",wt," weights")
	
	# remove each edge iteratively
	tlog(4, "Removing edges iteratively for unfiltered graph")
	g0 <- g
	vals <- c()
	for(e in 1:gsize(g0))
	{	# remove the edge of smallest weight
		weights <- get.edge.attribute(g0,wt)
		idx <- which.min(weights)
		cat()
		g0 <- delete_edges(graph=g0, edges=idx)
		
		# compute the size of the giant component
		size <- max(components(g0)$csize)
		vals <- c(size, vals)

		tlog(6, "Removing edge #",idx," with weight ",weights[idx],"\tSize of the largest component: ",size)
	}
	# plot result
	plot(
		x=1:gsize(g), y=vals/gorder(g),
		xlab=TeX(xlab), ylab=TeX(ylab),
		col=pal[1],
		ylim=0:1
	)
	
	# same for filtered graph 
	tlog(4, "Removing edges iteratively for filtered graph")
	g0 <- g.filt
	vals <- c()
	for(e in 1:gsize(g0))
	{	# remove the edge of smallest weight
		weights <- get.edge.attribute(g0,wt)
		idx <- which.min(weights)
		g0 <- delete_edges(graph=g0, edges=idx)
		
		# compute the size of the giant component
		size <- max(components(g0)$csize)
		vals <- c(size, vals)
		
		tlog(6, "Removing edge #",idx," with weight ",weights[idx],"\tSize of the largest component: ",size)
	}
	# plot result
	plot(
		x=1:gsize(g.filt), y=vals/gorder(g.filt),
		xlab=TeX(xlab), ylab=TeX(ylab),
		col=pal[2],
		ylim=0:1
	)
}
