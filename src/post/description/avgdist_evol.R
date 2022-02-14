# Studies how the average distance evolves as a function of the number of vertices.
# The script considers each scene separately, by order of publication or history.
# 
# Vincent Labatut
# 11/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/avgdist_evol.R")
###############################################################################
source("src/common/include.R")




###############################################################################
# plots unfiltered and filtered figures as separate files

# load full graph to get filtered characters
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
filt.names <- V(g)$name[V(g)$Filtered]
if(length(filt.names)==0) stop("Empty list of filtered characters")

# load raw data
data <- read.raw.data()
# compute the sequence of scene-based graphs (possibly one for each scene)
gs <- extract.static.graph.scenes(volume.info=data$volume.info, char.info=data$char.info, page.info=data$page.info, inter.df=data$inter.df, stats.scenes=data$stats.scenes, ret.seq=TRUE)

# init lists
g.orders <- list()
dist.vals <- list()

# compute average distance for each graph in the sequence
#print(any(sapply(gs, function(g) is_connected(g, mode="weak"))))	# check that each graph is connected
g.orders[[1]] <- future_sapply(gs, gorder)
dist.vals[[1]] <- future_sapply(gs, function(g) mean_distance(graph=g, directed=FALSE, unconnected=TRUE))

# same for filtered graphs
gs.filt <- future_lapply(gs, function(g) delete_vertices(g, v=intersect(filt.names,V(g)$name)))
g.orders[[2]] <- future_sapply(gs.filt, gorder)
dist.vals[[2]] <- future_sapply(gs.filt, function(g) mean_distance(graph=g, directed=FALSE, unconnected=TRUE))

# loop over unfiltered/filtered
natures <- c("unfiltered", "filtered")
pal <- get.palette(2)
for(i in 1:2)
{	# setup series
	x <- g.orders[[i]]
	y <- dist.vals[[i]]
	
	# fit a logarithmic relation
	fit <- lm(y ~ log(x))
	print(summary(fit))
	params <- fit$coefficients
	val1 <- params[1]; names(val1) <- NULL
	val2 <- params[2]; names(val2) <- NULL
	
	# perform NL regression
	df <- data.frame(x, y)
	fit <- nlsLM(y ~ c1*log(x) + c2, 
			start=list(c1=val1, c2=val2),
			data = df,
			control=list(maxiter=200))
	print(summary(fit))
	
	# plot the distance as the graph order
	plot.file <- get.path.topomeas.plot(object="nodepairs", mode="scenes", meas.name="avgdist", filtered=i==2, plot.type="evolution_publication_lines")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				# init plot with unfiltered results
				par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
				plot(
					x=x, y=y, 
					xlab=TeX(paste0("Number of ",natures[i]," vertices $n$")),
					ylab=TeX("Average distance $<d>$"),
					las=1, col=pal[i],
					type="l"
				)
				# plot fitted line
				threshold <- min(x)
				x0 <- seq(from=threshold, to=max(x), by=(max(x)-threshold)/100)
				lines(x0, predict(fit, list(x=x0)), col="BLACK", lty=2)
			dev.off()
	}
}




###############################################################################
# same thing, but plots both unfiltered and filtered figures in the same file
plot.file <- get.path.topomeas.plot(object="nodepairs", mode="scenes", meas.name="avgdist", filtered=FALSE, plot.type="evolution_publication_lines_both")

# process all formats
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
	
	# setup unfiltered series
	x <- g.orders[[1]]
	y <- dist.vals[[1]]
		
	# fit a logarithmic relation
	fit <- lm(y ~ log(x))
	params <- fit$coefficients
	val1 <- params[1]; names(val1) <- NULL
	val2 <- params[2]; names(val2) <- NULL
		
	# perform NL regression
	df <- data.frame(x, y)
	fit <- nlsLM(y ~ c1*log(x) + c2, 
		start=list(c1=val1, c2=val2),
		data = df,
		control=list(maxiter=200))
		
	# plot the distance as the graph order
	par(
		mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
		fig=c(0,1,0,1),		# set coordinate space of the original plot
		mgp=c(3,1,0)		# distance between axis ticks and values
	)
	# points
	plot(
		x=x, y=y, 
		xlab=TeX(paste0("Number of vertices $n$")),
		ylab=TeX("Average distance $<d>$"),
		las=1, col=pal[1],
		type="l"
	)
	# plot fitted line
	threshold <- min(x)
	x0 <- seq(from=threshold, to=max(x), by=(max(x)-threshold)/100)
	lines(x0, predict(fit, list(x=x0)), col="BLACK", lty=2)
	
	#####
	# setup filtered series
	x <- g.orders[[2]]
	y <- dist.vals[[2]]
	
	# fit a logarithmic relation
	fit <- lm(y ~ log(x))
	params <- fit$coefficients
	val1 <- params[1]; names(val1) <- NULL
	val2 <- params[2]; names(val2) <- NULL
	
	# perform NL regression
	df <- data.frame(x, y)
	fit <- nlsLM(y ~ c1*log(x) + c2, 
		start=list(c1=val1, c2=val2),
		data = df,
		control=list(maxiter=200))
	
	# plot the distance as the graph order
	par(
		fig=c(0.33,0.98, 0.05, 0.70), 
		new=TRUE,
		mgp=c(3,0.5,0)
	)
	# points
	plot(
		x=x, y=y, 
		xlab=NA, ylab=NA,
		las=1, col=pal[2],
		type="l",
		cex.lab=0.75, cex.axis=0.75, cex=0.75
	)
	# plot fitted line
	threshold <- min(x)
	x0 <- seq(from=threshold, to=max(x), by=(max(x)-threshold)/100)
	lines(x0, predict(fit, list(x=x0)), col="BLACK", lty=2)
	
	# close file
	dev.off()
}
