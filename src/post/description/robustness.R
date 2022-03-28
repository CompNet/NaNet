# Studies the robustness of the network by removing its most central vertices
# and plotting the size of the resulting largest component.
# 
# Vincent Labatut
# 03/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/robustness.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/include.R")
start.rec.log(text="RobustnessPlot")




###############################################################################
# get the graphs

# read the unfiltered graph
graph.file <- get.path.data.graph(mode="scenes", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
tlog(0,"Reading graph file \"",graph.file,"\"")
g.unf <- read.graphml.file(file=graph.file)

# filter the characters
tlog(0, "Filtering characters")
g.flt <- delete_vertices(graph=g.unf, v=V(g.unf)$Filter=="Discard")

# measures of interest (and random removal)
meass <- c(MEAS_DEGREE, MEAS_BETWEENNESS, MEAS_CLOSENESS, MEAS_EIGENCNTR, "random")




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
	tlog.start.loop(4,2,"Looping over unfiltered/filtered nets")
	for(filtered in c(FALSE, TRUE))
	{	tlog.loop(6,m,"Processing the ",if(!filtered) "un" else "","filtered network")
		
		# init graph
		if(filtered)
			gt <- g.flt
		else
			gt <- g.unf
		vs <- 1:gorder(gt)
		cs <- c()
		
		thre <- round(0.05*gorder(gt))
		thre2 <- round(0.10*gorder(gt))
		
		# remove each vertex one after the other
		tlog.start.loop(6,length(vs),"Looping over vertices")
		for(v in vs)
		{	#tlog.loop(8,v,"Processing vertex ",v,"/",length(xs))
			
			# size of the largest component
			mxcps <- max(components(gt)$csize)
			cs <- c(cs, mxcps)
			if(v==6)
				tlog(10,"Component size when removing the top 5 vertices: ",mxcps," (",sprintf("%.2f",mxcps/gorder(gt)*100),"%)")
			if(v==thre)
				tlog(10,"Component size when removing 5% of the top vertices: ",mxcps," (",sprintf("%.2f",mxcps/gorder(gt)*100),"%)")
			if(v==thre2)
				tlog(10,"Component size when removing 10% of the top vertices: ",mxcps," (",sprintf("%.2f",mxcps/gorder(gt)*100),"%)")
			
			# remove the next vertex
			if(meas=="random")
				idx <- sample(gorder(gt), 1)
			else
			{	cache <<- list()
				vals <- NODE_MEASURES[[meas]]$foo(gt)
				vals[is.na(vals)] <- 0
				idx <- which(vals==max(vals,na.rm=T))[1]
			}
			gt <- delete_vertices(graph=gt, v=idx)
		}
		tlog.end.loop(6,"Loop over vertices is over")
		ll[[as.character(filtered)]] <- cs
	}
	tlog.end.loop(4,"Loop over unfiltered/filtered graph is over")
	ys[[meas]] <- ll
}
tlog.end.loop(0,"Loop over measures is complete")




###############################################################################
# plotting the results
meass <- c(MEAS_DEGREE, MEAS_BETWEENNESS, "random")

# file and common parameters
plot.file <- get.path.stats.topo(net.type="static", mode="scenes", weights="none", meas.name=MEAS_MULTI_GRAPH, filtered="both", suf=paste0("giant-comp-size_vs_centrality"))
tlog(4, "Plotting in file ",plot.file)
xlab <- "Proportion of vertices removed"
pal <- ATT_COLORS_FILT[c("Discard","Keep")]

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
		combine.colors(col1=pal["Discard"], col2="WHITE", transparency=65),
		pal["Discard"],
		combine.colors(col1=pal["Discard"], col2="BLACK", transparency=65)
	)
	for(s in 1:length(meass))
	{	points(
			x=1:gorder(g.unf)/gorder(g.unf), y=ys[[meass[s]]][["FALSE"]]/gorder(g.unf), 
			col=cols[s], 
			pch=s-1
			# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
		)
	}
	
	# add filtered results
	par(
		fig=c(0.43, 0.97, 0.43, 0.97), 
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
		combine.colors(col1=pal["Keep"], col2="WHITE", transparency=65),
		pal["Keep"],
		combine.colors(col1=pal["Keep"], col2="BLACK", transparency=65)
	)
	for(s in 1:length(meass))
	{	points(
			x=1:gorder(g.flt)/gorder(g.flt), y=ys[[meass[s]]][["TRUE"]]/gorder(g.flt), 
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
