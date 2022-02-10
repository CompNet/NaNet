# Additional plots regarding the relation between various nodal centrality
# measures and the number of occurrences of the characters.
# 
# Vincent Labatut
# 02/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/centr_vs_occ.R.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="CentrVsOcc")




###############################################################################
tlog(0, "Plotting centrality vs. occurrences")

# plot parameters
pal <- get.palette(2)

# read the graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
# clean names
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)

# measure names
centr.names <- c("degree", "betweenness", "closeness", "eigenvector")
occ.proper.names <- c("panel"="Panel", "scene"="Scene", "page"="Page", "volume"="Volume")
occ.names <- names(occ.proper.names)

# inlay position
inlay.coords <- matrix(nrow=length(centr.names)*length(occ.names), ncol=4)
rownames(inlay.coords) <- apply(expand.grid(centr.names,occ.names), 1, function(row) paste(row, collapse="_"))
inlay.coords["betweenness_page",] <- c(0.53, 0.97, 0.05, 0.49)
inlay.coords["closeness_page",] <- c(0.51, 0.97, 0.05, 0.51)
inlay.coords["degree_page",] <- c(0.52, 0.97, 0.05, 0.52)
inlay.coords["eigenvector_page",] <- c(0.32, 0.97, 0.05, 0.70)
inlay.coords["betweenness_panel",] <- c(0.57, 0.97, 0.05, 0.45)
inlay.coords["closeness_panel",] <- c(0.55, 0.97, 0.05, 0.47)
inlay.coords["degree_panel",] <- c(0.06, 0.49, 0.54, 0.97)
inlay.coords["eigenvector_panel",] <- c(0.39, 0.97, 0.05, 0.63)
inlay.coords["betweenness_scene",] <- c(0.49, 0.97, 0.05, 0.53)
inlay.coords["closeness_scene",] <- c(0.48, 0.97, 0.05, 0.52)
inlay.coords["degree_scene",] <- c(0.49, 0.97, 0.05, 0.53)
inlay.coords["eigenvector_scene",] <- c(0.34, 0.97, 0.05, 0.68)
inlay.coords["betweenness_volume",] <- c(0.45, 0.97, 0.05, 0.57)
inlay.coords["closeness_volume",] <- c(0.47, 0.97, 0.05, 0.55)
inlay.coords["degree_volume",] <- c(0.47, 0.97, 0.05, 0.55)
inlay.coords["eigenvector_volume",] <- c(0.27, 0.97, 0.05, 0.75)

# loop over measures
tlog(0, "Loop over measures")
for(centr.name in centr.names)
{	tlog(2, "Processing centrality ",centr.name)
	
	# loop over modes
	tlog(2, "Loop over occurrence modes")
	for(occ.name in occ.names)
	{	tlog(4, "Processing mode ",occ.name)
		
		# get centrality values
		centr.vals.unf <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=centr.name, filtered=FALSE)
		centr.vals.flt <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=centr.name, filtered=TRUE)
		
		# get occurrence values
		file <- get.path.stat.corpus(object="characters", desc=paste0("chars_distrib_",occ.name,"_nbr_rawvals.csv"))
		occ.vals.unf <- as.matrix(read.csv(file=file))
		file <- get.path.stat.corpus(object="characters", desc=paste0("chars_filtered_distrib_",occ.name,"_nbr_rawvals.csv"))
		occ.vals.flt <- as.matrix(read.csv(file=file))
		
		#### handle unfiltered data
		# filter out zero values and NaN
		idx <- which(!is.nan(occ.vals.unf) & occ.vals.unf>0 & !is.nan(centr.vals.unf) & centr.vals.unf>0)
		centr.vals.unf <- centr.vals.unf[idx]
		occ.vals.unf <- occ.vals.unf[idx]
		avg.occ.vals.unf <- sapply(1:max(occ.vals.unf), function(d) mean(centr.vals.unf[occ.vals.unf==d]))

#		# keep tail
#		thresholds <- quantile(occ.vals.unf, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
#		threshold <- thresholds[7]	# exp=0.42
#		cut.centr.vals.unf <- centr.vals.unf[occ.vals.unf>=threshold]
#		cut.occ.vals.unf <- occ.vals.unf[occ.vals.unf>=threshold]
#		
#		# init parameters using a linear regression
#		fit <- lm(log(cut.centr.vals.unf) ~ log(cut.occ.vals.unf))
#		summary(fit)
#		params <- fit$coefficients
#		val1 <- exp(params[1]); names(val1) <- NULL
#		val2 <- params[2]; names(val2) <- NULL
#		val3 <- 0
#		
#		# perform NL regression
#		df <- data.frame(cut.occ.vals.unf, cut.centr.vals.unf)
#		fit <- nlsLM(cut.centr.vals.unf ~ c1*cut.occ.vals.unf^c2, 
#				start=list(c1=val1, c2=val2),
#				data = df,
#				control=list(maxiter=200))
#		summary(fit)

		# plot unfiltered data
		col <- pal[1]
		col.sec <- combine.colors(col, "WHITE", transparency=20)
		xlab <- paste0("Number of ",occ.proper.names[occ.name],"s")
		ylab <- NODE_MEASURES[[centr.name]]$cname
		plot.file <- get.path.topomeas.plot(object="nodes", mode="scenes", meas.name=paste0("occ_",occ.name,"_vs_",centr.name), filtered=FALSE)
		tlog(6, "Plotting in file ",plot.file)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
		par(
			mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
			fig=c(0,1,0,1),		# set coordinate space of the original plot
			mgp=c(3,1,0)		# distance between axis ticks and values
		)
		# points
		plot(
			x=occ.vals.unf, y=centr.vals.unf, 
			xlab=TeX(xlab), ylab=TeX(ylab),
			log="xy", 
			las=1, col=col,
			xlim=c(1,max(occ.vals.unf)*1.1)
		)
		# mean
#		idx <- which(!is.nan(avg.occ.vals.unf) & avg.occ.vals.unf>0)
#		lines(	
#			x=idx, avg.occ.vals.unf[idx],
#			col=col
#		)
#		# fitted line
#		threshold <- min(cut.occ.vals.unf)
#		x <- seq(from=threshold, to=max(occ.vals.unf), by=(max(occ.vals.unf)-threshold)/100)
#		lines(x, predict(fit, list(cut.occ.vals.unf=x)), col="BLACK", lty=2)
#		# legend
#		legend(
#			x="topright",
#			lty=1:2, col=c(col,"BLACK"),
#			legend=c("Mean","Fit")
#		)
		
		#### handle filtered data
		# filter out zero values and NaN
		idx <- which(!is.nan(occ.vals.flt) & occ.vals.flt>0 & !is.nan(centr.vals.flt) & centr.vals.flt>0)
		centr.vals.flt <- centr.vals.flt[idx]
		occ.vals.flt <- occ.vals.flt[idx]
		avg.occ.vals.flt <- sapply(1:max(occ.vals.flt), function(d) mean(centr.vals.flt[occ.vals.flt==d]))
		
#		# keep tail
#		thresholds <- quantile(occ.vals.flt, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
#		threshold <- thresholds[7]	# exp=1.53
#		cut.centr.vals.flt <- centr.vals.flt[occ.vals.flt>=threshold]
#		cut.occ.vals.flt <- occ.vals.flt[occ.vals.flt>=threshold]
#		
#		# init parameters using a linear regression
#		fit <- lm(log(cut.centr.vals.flt) ~ log(cut.occ.vals.flt))
#		summary(fit)
#		params <- fit$coefficients
#		val1 <- exp(params[1]); names(val1) <- NULL
#		val2 <- params[2]; names(val2) <- NULL
#		val3 <- 0
#		
#		# perform NL regression
#		df <- data.frame(cut.occ.vals.flt, cut.centr.vals.flt)
#		fit <- nlsLM(cut.centr.vals.flt ~ c1*cut.occ.vals.flt^c2, 
#				start=list(c1=val1, c2=val2),
#				data = df,
#				control=list(maxiter=200))
#		summary(fit)
		
		# plot as an inset
		col <- pal[2]
		col.sec <- combine.colors(col, "WHITE", transparency=20)
		par(
			fig=inlay.coords[paste0(centr.name,"_",occ.name),], 
			new=T,
			mgp=c(3,0.5,0)
		)
		# points
		#rect(0.06,0.56, 0.47, 0.97, border=NA, col="WHITE")
		plot(
			x=occ.vals.flt, y=centr.vals.flt, 
			xlab=NA, ylab=NA,
			log="xy", 
			las=1, col=col,
			xlim=c(1,max(occ.vals.flt)*1.1),
			cex.lab=0.75, cex.axis=0.75, cex=0.75
		)
#		# mean
#		idx <- which(!is.nan(avg.occ.vals.flt) & avg.occ.vals.flt>0)
#		lines(	
#			x=idx, avg.occ.vals.flt[idx],
#			col=col
#		)
#		# fitted line
#		threshold <- min(cut.occ.vals.flt)
#		x <- seq(from=threshold, to=max(occ.vals.flt), by=(max(occ.vals.flt)-threshold)/100)
#		lines(x, predict(fit, list(cut.occ.vals.flt=x)), col="BLACK", lty=2)
#		# legend
#		legend(
#			x="bottomleft",
#			lty=1:2, col=c(col,"BLACK"),
#			legend=c("Mean","Fit"),
#			cex=0.75
#		)
		
		# close file
		dev.off()
	}
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
