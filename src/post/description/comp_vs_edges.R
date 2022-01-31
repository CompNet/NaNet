# Additional plots regarding the evolution of the giant component size
# as a function of the number of edges. We remove edges by order
# of increasing weight (using both types of weights: durations vs.
# occurrences). The process is the same as in the following paper:
#	P. M. Gleiser, 
#	“How to become a superhero,” 
#	Journal of Statistical Mechanics, vol. 2007, no. 9, p. P09020, 2007.
#	DOI: 10.1088/1742-5468/2007/09/P09020
# 
# Vincent Labatut
# 01/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/comp_vs_edges.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="CompVsEdges")




###############################################################################
# compute results and plot separate figures

# read the graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
tlog(0, "Reading graph ",graph.file)
g <- read_graph(file=graph.file, format="graphml")
# clean names
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)

# filter the characters
tlog(0, "Filtering characters")
filt.names <- V(g)$name[V(g)$Filtered]
if(length(filt.names)==0) error("Empty list of filtered characters")
g.filt <- delete_vertices(g, V(g)$Filtered)

# plot settings
xlab <- "Number of edges"
ylab <- "Largest component size (proportion of vertices)"

# compute both types of weights
tlog(0, "Compute values and plot basic figures")
pal <- get.palette(2)
wts <- c("Duration","Occurrences")
res <- list()
for(wt in wts)
{	tlog(2, "Dealing with ",wt," weights")
	
	# remove each edge iteratively
	tlog(4, "Removing edges iteratively for the unfiltered graph")
	g0 <- g
	vals <- c()
	for(e in 1:gsize(g0))
	{	# remove the edge of smallest weight
		weights <- get.edge.attribute(g0,wt)
		idx <- which.min(weights)
		g0 <- delete_edges(graph=g0, edges=idx)
		
		# compute the size of the giant component
		size <- max(components(g0)$csize)
		vals <- c(size, vals)

#		tlog(6, "Removing edge #",idx," with weight ",weights[idx],"\tSize of the largest component: ",size)
	}
	res[[paste0("Unfiltered-",wt)]] <- vals
	# plot results
	plot.file <- get.path.comparison.plot(object="links", mode="scenes", meas.name="linkweight", weights=tolower(wt), filtered=FALSE, plot.type="comp_vs_edges")
	tlog(4, "Plotting in file ",plot.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
			plot(
				x=1:gsize(g), y=vals/gorder(g),
				xlab=TeX(xlab), ylab=TeX(ylab),
				col=pal[1],
				ylim=0:1,
				type="l"
			)
		dev.off()
	}
	
	# same for filtered graph 
	tlog(4, "Removing edges iteratively for the filtered graph")
	g0 <- g.filt
	vals.filt <- c()
	for(e in 1:gsize(g0))
	{	# remove the edge of smallest weight
		weights <- get.edge.attribute(g0,wt)
		idx <- which.min(weights)
		g0 <- delete_edges(graph=g0, edges=idx)
		
		# compute the size of the giant component
		size <- max(components(g0)$csize)
		vals.filt <- c(size, vals.filt)
		
#		tlog(6, "Removing edge #",idx," with weight ",weights[idx],"\tSize of the largest component: ",size)
	}
	res[[paste0("Filtered-",wt)]] <- vals.filt
	# plot result
	plot.file <- get.path.comparison.plot(object="links", mode="scenes", meas.name="linkweight", weights=tolower(wt), filtered=TRUE, plot.type="comp_vs_edges")
	tlog(4, "Plotting in file ",plot.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
			plot(
				x=1:gsize(g.filt), y=vals.filt/gorder(g.filt),
				xlab=TeX(xlab), ylab=TeX(ylab),
				col=pal[2],
				ylim=0:1,
				type="l"
			)
		dev.off()
	}
}




###############################################################################
# plot comparative figures

# both unfiltered and filtered in the same figure
tlog(0, "Plot figures combining unfiltered and filtered nets results")
for(wt in wts)
{	tlog(2, "Dealing with ",wt," weights")
	
	plot.file <- get.path.comparison.plot(object="links", mode="scenes", meas.name="linkweight", weights=tolower(wt), filtered=FALSE, plot.type="comp_vs_edges_both")
	tlog(4, "Plotting in file ",plot.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
			# unfiltered net results
			plot(
				x=1:gsize(g), y=res[[paste0("Unfiltered-",wt)]]/gorder(g),
				xlab=TeX(xlab), ylab=TeX(ylab),
				col=pal[1],
				ylim=0:1,
				type="l"
			)
			# filtered net results
			lines(
				x=1:gsize(g.filt), y=res[[paste0("Filtered-",wt)]]/gorder(g.filt), 
#				lty=2,
				col=pal[2] 
			)
			# legend
			legend(
				title="Characters",
				x="bottomright",
				fill=pal,
				legend=c("Unfiltered","Filtered")
			)
		dev.off()
	}
}

# both types of weights in the same figure
tlog(0, "Plot figures combining both types of weights")
for(filtered in c(FALSE,TRUE))
{	if(filtered)
	{	norm <- gorder(g.filt)
		fn <- "Filtered"
		x <- 1:gsize(g.filt)
		col <- pal[2]
	}
	else
	{	norm <- gorder(g)
		fn <- "Unfiltered"
		x <- 1:gsize(g)
		col <- pal[1]
	}
	tlog(2, "Dealing with the ",tolower(fn)," network")
	
	plot.file <- get.path.comparison.plot(object="links", mode="scenes", meas.name="linkweight", weights="both", filtered=filtered, plot.type="comp_vs_edges")
	tlog(4, "Plotting in file ",plot.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
		par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
			# unfiltered net results
			plot(
				x=x, y=res[[paste0(fn,"-",wts[1])]]/norm,
				xlab=TeX(xlab), ylab=TeX(ylab),
				col=col,
				ylim=0:1,
				type="l"
			)
			# filtered net results
			lines(
				x=x, y=res[[paste0(fn,"-",wts[2])]]/norm,
				lty=2,
				col=col,
			)
			# legend
			legend(
				title="Weights",
				x="bottomright",
				col=col,
				lty=1:2,
				legend=wts
			)
		dev.off()
	}
}

# all in the same figure
plot.file <- get.path.comparison.plot(object="links", mode="scenes", meas.name="linkweight", weights="both", filtered=FALSE, plot.type="comp_vs_edges_both")
tlog(0, "Plot everything in the same file ",plot.file)
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
	par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		# create empty plot
		plot(
			NULL,
			xlab=TeX(xlab), ylab=TeX(ylab),
			col=col,
			ylim=0:1, xlim=c(1,gsize(g))
		)
		# add series
		for(filtered in c("Unfiltered","Filtered"))
		{	if(filtered=="Filtered")
			{	norm <- gorder(g.filt)
				x <- 1:gsize(g.filt)
				col <- pal[2]
			}
			else
			{	norm <- gorder(g)
				x <- 1:gsize(g)
				col <- pal[1]
			}
			for(w in 1:length(wts))
			{	lines(
					x=x, y=res[[paste0(filtered,"-",wts[w])]]/norm,
					lty=w,
					col=col,
				)
			}
		}
		# legends
		legend(
			title="Characters",
			x="topleft",
			fill=pal,
			legend=c("Unfiltered","Filtered")
		)
		legend(
			title="Weights",
			x="bottomright",
			lty=1:2,
			legend=wts
		)
	dev.off()
}




###############################################################################
# end logging
end.rec.log()
