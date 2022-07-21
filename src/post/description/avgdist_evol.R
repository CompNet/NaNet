# Studies how the average distance evolves as a function of the number of vertices.
# The script considers each scene separately, by order of publication or history.
# 
# Vincent Labatut
# 11/2021
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/avgdist_evol.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="AvgDistEvol")




###############################################################################
tlog(0, "Evolution of the average distance over scenes")

# loop over TRUE=publication order, FALSE=story order 
for(pub.order in c(FALSE, TRUE))
{	###############################################################################
	# publication order vs. story order
	tlog(0, "Processing pub.order=",pub.order)
	
	
	
	
	###############################################################################
	# plots unfiltered and filtered figures as separate files
	
	# load corpus stats
	tlog(0, "Read corpus stats")
	data <- read.corpus.data()
	
	# get filtered characters
	filt.names <- data$char.stats[data$char.stats[,COL_FILTER]=="Discard",COL_NAME]
	if(length(filt.names)==0) stop("Empty list of filtered characters")
	
	# compute the sequence of scene-based graphs (possibly one for each scene)
	tlog(0, "Extract graph sequence")
	gs <- extract.static.graph.scenes(
		inter.df=data$inter.df, 
		char.stats=data$char.stats, 
		scene.stats=data$scene.stats, scene.chars=data$scene.chars,
		volume.stats=data$volume.stats, 
		ret.seq=TRUE, pub.order=pub.order
	)
	
	# init lists
	g.orders <- list()
	dist.vals <- list()
	order.txt <- if(pub.order) "publication" else "story"
	
	# compute average distance for each graph in the sequence
	tlog(0, "Compute average distances for unfiltered graphs")
	#print(any(sapply(gs, function(g) is_connected(g, mode="weak"))))	# check that each graph is connected
	g.orders[[1]] <- future_sapply(gs, gorder)
	dist.vals[[1]] <- future_sapply(gs, function(g) mean_distance(graph=g, directed=FALSE, unconnected=TRUE))
	
	# same for filtered graphs
	tlog(0, "Compute average distances for filtered graphs")
	gs.filt <- future_lapply(gs, function(g) delete_vertices(g, v=intersect(filt.names,V(g)$name)))
	g.orders[[2]] <- future_sapply(gs.filt, gorder)
	dist.vals[[2]] <- future_sapply(gs.filt, function(g) mean_distance(graph=g, directed=FALSE, unconnected=TRUE))
	
	# loop over unfiltered/filtered
	tlog(0, "Loop over unfiltered/filtered graphs")
	natures <- c("unfiltered", "filtered")
	pal <- ATT_COLORS_FILT[c("Discard","Keep")]
	for(i in 1:2)
	{	if(i==1)
		{	filt.txt <- "unfiltered"
			col <- pal["Discard"]
		}
		else 
		{	filt.txt <- "filtered"
			col <- pal["Keep"]
		}
		tlog(2, "Processing ",filt.txt," net (",i,"/",2,")")
		
		# setup series
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
		
		# plot distance as a function of graph order
		plot.file <- get.path.stats.topo(net.type="static", mode="scenes", meas.name=MEAS_MULTI_GRAPH, weights="none", filtered=filt.txt, suf=paste0("distance-average_vs_nodes_",order.txt))
		tlog(2, "Plotting in file ",plot.file)
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
						las=1, col=col,
						type="l"
					)
					# plot fitted line
					threshold <- min(x)
					x0 <- seq(from=threshold, to=max(x), by=(max(x)-threshold)/100)
					lines(x0, predict(fit, list(x=x0)), col="BLACK", lty=2)
				dev.off()
		}
	}
	tlog(0, "Unfiltered/filtered loop complete")
	
	
	
	
	###############################################################################
	# same thing, but plots both unfiltered and filtered figures in the same file
	plot.file <- get.path.stats.topo(net.type="static", mode="scenes", meas.name=MEAS_MULTI_GRAPH, weights="none", filtered="both", suf=paste0("distance-average_vs_nodes_",order.txt))
	tlog(0, "Plotting both unfiltered and filtered results in file ",plot.file)
	
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
			las=1, col=pal["Discard"],
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
			las=1, col=pal["Keep"],
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
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
