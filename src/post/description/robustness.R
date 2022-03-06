# Studies the robustness of the network by removing its most central vertices
# and plotting the size of the resulting largest component.
# 
# Vincent Labatut
# 03/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/robustness.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="RobustnessPlot")




###############################################################################
# get the graphs

# read the unfiltered graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
tlog(0,"Reading graph file \"",graph.file,"\"")
g.unf <- read_graph(file=graph.file, format="graphml")
# clean names
V(g.unf)$name <- fix.encoding(strings=V(g.unf)$name)
V(g.unf)$ShortName <- fix.encoding(strings=V(g.unf)$ShortName)

# filter the characters
tlog(0, "Filtering characters")
g.flt <- delete_vertices(graph=g.unf, v=V(g.unf)$Filtered)

# measures of interest (and random removal)
meass <- c(MEAS_DEGREE, MEAS_BETWEENNESS, "random")




###############################################################################
# compute the measures

# list to store results
ys <- list()

# process each measure separately
tlog.start.loop(0,length(meass),"Looping over measures")
for(m in 1:length(meass))
{	meas <- meass[m]
	tlog.loop(2,m,"Processing measure ",meas," (",m,"/",length(meass),")")
	ll <- list()
	
	# process the unfiltered and filtered networks
	tlog.start.loop(2,2,"Looping over unfiltered/filtered nets")
	for(filtered in c(FALSE, TRUE))
	{	tlog.loop(4,m,"Processing the ",if(!filtered) "un" else "","filtered network")
		
		# init graph
		if(filtered)
			gt <- g.flt
		else
			gt <- g.unf
		vs <- 1:gorder(gt)
		cs <- c()
		
		thre <- round(0.05*gorder(gt))
		
		# remove each vertex one after the other
		tlog.start.loop(4,length(vs),"Looping over vertices")
		for(v in vs)
		{	#tlog.loop(6,v,"Processing vertex ",v,"/",length(xs))
			
			# size of the largest component
			mxcps <- max(components(gt)$csize)
			cs <- c(cs, mxcps)
			if(v==6)
				tlog(8,"Component size when removing the top 5 vertices: ",mxcps," (",sprintf("%.2f",mxcps/gorder(gt)*100),"%)")
			if(v==thre)
				tlog(8,"Component size when removing 5% of the top vertices: ",mxcps," (",sprintf("%.2f",mxcps/gorder(gt)*100),"%)")
			
			# remove the next vertex
			if(meas=="random")
				idx <- sample(gorder(gt), 1)
			else
			{	cache <<- list()
				vals <- NODE_MEASURES[[meas]]$foo(gt)
				idx <- as.integer(which.max(vals)[1])
			}
			gt <- delete_vertices(graph=gt, v=idx)
		}
		tlog.end.loop(4,"Loop over vertices is over")
		ll[[as.character(filtered)]] <- cs
	}
	tlog.end.loop(2,"Loop over unfiltered/filtered graph is over")
	ys[[meas]] <- ll
}
tlog.end.loop(0,"Loop over measures is complete")




###############################################################################
# plotting the results

# file and common parameters
plot.file <- get.path.topomeas.plot(object="nodes", mode="scenes", meas.name="gcompsize", filtered=FALSE, plot.type=paste0("vs_",meas))
tlog(4, "Plotting in file ",plot.file)
xlab <- "Proportion of vertices removed"
pal <- get.palette(2)

# create plot files
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
	
	# add unfiltered results
	par(
		mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
		fig=c(0,1,0,1),		# set coordinate space of the original plot
		mgp=c(3,1,0)		# distance between axis ticks and values
	)
	# init plot
	plot(
		NULL,
		xlab=TeX(xlab), ylab=TeX("Proportion of vertices in the largest component"),
		#log="xy", 
		las=1,
		xlim=0:1, ylim=0:1
	)
	# series
	cols <- c(
		combine.colors(col1=pal[1], col2="WHITE", transparency=65),
		pal[1],
		combine.colors(col1=pal[1], col2="BLACK", transparency=65)
	)
	for(s in 1:length(ys))
	{	points(
			x=1:gorder(g.unf)/gorder(g.unf), y=ys[[s]][["FALSE"]]/gorder(g.unf), 
			col=cols[s], 
			pch=s-1
			# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
		)
	}
	
	# add filtered results
	par(
		fig=c(0.42, 0.97, 0.42, 0.97), 
		new=TRUE,
		mgp=c(1.5,0.5,0)
	)
	# init plot
	plot(
		NULL,
		xlab=NA, ylab=NA,
		#log="xy", 
		las=1,
		xlim=0:1, ylim=0:1,
		cex.lab=0.75, cex.axis=0.75, cex=0.75
	)
	# series
	cols <- c(
		combine.colors(col1=pal[2], col2="WHITE", transparency=65),
		pal[2],
		combine.colors(col1=pal[2], col2="BLACK", transparency=65)
	)
	for(s in 1:length(ys))
	{	points(
			x=1:gorder(g.flt)/gorder(g.flt), y=ys[[s]][["TRUE"]]/gorder(g.flt), 
			col=cols[s], 
			pch=s-1,
			cex.lab=0.75, cex.axis=0.75, cex=0.75
			# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
		)
	}
	# legend
	cols <- c("gray65","gray40","gray25")
	legend(
		x="topright",
		col=cols,
		pch=15:17, 
		legend=sapply(meass, function(meas) if(meas=="random") "Random" else NODE_MEASURES[[meas]]$cname),
		cex=0.75
	)
	
	dev.off()
}











###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()