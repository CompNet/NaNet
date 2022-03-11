# Additional plots regarding the relation between various nodal centrality
# measures.
# 
# Vincent Labatut
# 02/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/centr_vs_centr.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="CentrVsCentr")




###############################################################################
tlog(0, "Plotting centrality vs. centrality")

# plot parameters
pal <- get.palette(2)

# read the graph
#graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
#g <- read_graph(file=graph.file, format="graphml")
## clean names
#V(g)$name <- fix.encoding(strings=V(g)$name)
#V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
# TODO I think I don't need this

# measure names
centr.names <- c(MEAS_DEGREE, MEAS_BETWEENNESS, MEAS_CLOSENESS, MEAS_EIGENCNTR)

# correlation matrices
corr.mat.unf.raw <- matrix(1, nrow=length(centr.names), ncol=length(centr.names), dimnames=list(centr.names,centr.names))
corr.mat.unf.clean <- matrix(1, nrow=length(centr.names), ncol=length(centr.names), dimnames=list(centr.names,centr.names))
corr.mat.flt.raw <- matrix(1, nrow=length(centr.names), ncol=length(centr.names), dimnames=list(centr.names,centr.names))
corr.mat.flt.clean <- matrix(1, nrow=length(centr.names), ncol=length(centr.names), dimnames=list(centr.names,centr.names))

# inlay position
inlay.coords <- matrix(nrow=length(centr.names)*length(centr.names), ncol=4)
rownames(inlay.coords) <- apply(expand.grid(centr.names,centr.names), 1, function(row) paste(row, collapse="_"))
inlay.coords[paste0(MEAS_CLOSENESS,"_",MEAS_BETWEENNESS),] <- c(0.06, 0.46, 0.57, 0.97)
inlay.coords[paste0(MEAS_DEGREE,"_",MEAS_BETWEENNESS),] <- c(0.06, 0.54, 0.49, 0.97)
inlay.coords[paste0(MEAS_EIGENCNTR,"_",MEAS_BETWEENNESS),] <- c(0.27, 0.97, 0.05, 0.75)
inlay.coords[paste0(MEAS_BETWEENNESS,"_",MEAS_CLOSENESS),] <- c(0.59, 0.97, 0.05, 0.43)
inlay.coords[paste0(MEAS_DEGREE,"_",MEAS_CLOSENESS),] <- c(0.06, 0.54, 0.49, 0.97)
inlay.coords[paste0(MEAS_EIGENCNTR,"_",MEAS_CLOSENESS),] <- c(0.41, 0.97, 0.05, 0.61)
inlay.coords[paste0(MEAS_BETWEENNESS,"_",MEAS_DEGREE),] <- c(0.49, 0.97, 0.05, 0.53)
inlay.coords[paste0(MEAS_CLOSENESS,"_",MEAS_DEGREE),] <- c(0.49, 0.97, 0.05, 0.51)
inlay.coords[paste0(MEAS_EIGENCNTR,"_",MEAS_DEGREE),] <- c(0.27, 0.97, 0.05, 0.75)
inlay.coords[paste0(MEAS_BETWEENNESS,"_",MEAS_EIGENCNTR),] <- c(0.06, 0.73, 0.30, 0.97)
inlay.coords[paste0(MEAS_CLOSENESS,"_",MEAS_EIGENCNTR),] <- c(0.06, 0.62, 0.41, 0.97)
inlay.coords[paste0(MEAS_DEGREE,"_",MEAS_EIGENCNTR),] <- c(0.06, 0.73, 0.30, 0.97)

# loop over first measures
tlog(0, "Loop over measures")
for(centr1.name in centr.names)
{	tlog(2, "Processing first centrality measure: ",centr1.name)
	
	# loop over second measures
	for(centr2.name in setdiff(centr.names, centr1.name))
	{	tlog(4, "Processing second centrality measure ",centr2.name)
		
		# get centrality 1 values
		centr1.vals.unf <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=centr1.name, filtered=FALSE)
		centr1.vals.flt <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=centr1.name, filtered=TRUE)
		
		# get centrality 2 values
		centr2.vals.unf <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=centr2.name, filtered=FALSE)
		centr2.vals.flt <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=centr2.name, filtered=TRUE)
		
		#### handle unfiltered data
		tlog(6,"Dealing with the unfiltered data")
		corr.mat.unf.raw[centr1.name,centr2.name] <- cor(centr1.vals.unf, centr2.vals.unf, method="spearman")
		tlog(8,"Spearman correlation before cleaning: ", corr.mat.unf.raw[centr1.name,centr2.name])
		# filter out zero values and NaN
		idx <- which(!is.nan(centr2.vals.unf) & centr2.vals.unf>0 & !is.nan(centr1.vals.unf) & centr1.vals.unf>0)
		centr1.vals.unf <- centr1.vals.unf[idx]
		centr2.vals.unf <- centr2.vals.unf[idx]
		corr.mat.unf.clean[centr1.name,centr2.name] <- cor(centr1.vals.unf, centr2.vals.unf, method="spearman")
		tlog(8,"Spearman correlation before cleaning: ", corr.mat.unf.clean[centr1.name,centr2.name])
		avg.centr2.vals.unf <- sapply(1:max(centr2.vals.unf), function(d) mean(centr1.vals.unf[centr2.vals.unf==d]))

#		# keep tail
#		thresholds <- quantile(centr2.vals.unf, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
#		threshold <- thresholds[7]	# exp=0.42
#		cut.centr1.vals.unf <- centr1.vals.unf[centr2.vals.unf>=threshold]
#		cut.centr2.vals.unf <- centr2.vals.unf[centr2.vals.unf>=threshold]
#		
#		# init parameters using a linear regression
#		fit <- lm(log(cut.centr1.vals.unf) ~ log(cut.centr2.vals.unf))
#		summary(fit)
#		params <- fit$coefficients
#		val1 <- exp(params[1]); names(val1) <- NULL
#		val2 <- params[2]; names(val2) <- NULL
#		val3 <- 0
#		
#		# perform NL regression
#		df <- data.frame(cut.centr2.vals.unf, cut.centr1.vals.unf)
#		fit <- nlsLM(cut.centr1.vals.unf ~ c1*cut.centr2.vals.unf^c2, 
#				start=list(c1=val1, c2=val2),
#				data = df,
#				control=list(maxiter=200))
#		summary(fit)

		# plot unfiltered data
		col <- pal[1]
		col.sec <- combine.colors(col, "WHITE", transparency=20)
		xlab <- NODE_MEASURES[[centr2.name]]$cname
		ylab <- NODE_MEASURES[[centr1.name]]$cname
		plot.file <- get.path.topomeas.plot(object="nodes", mode="scenes", meas.name=paste0("centr_",centr2.name,"_vs_",centr1.name), filtered=FALSE)
		tlog(8, "Plotting in file ",plot.file)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
		par(
			mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
			fig=c(0,1,0,1),		# set coordinate space of the original plot
			mgp=c(3,1,0)		# distance between axis ticks and values
		)
		# points
		plot(
			x=centr2.vals.unf, y=centr1.vals.unf, 
			xlab=TeX(xlab), ylab=TeX(ylab),
			log="xy", yaxt="n", xaxt="n",
			las=1, col=col
		)
		if(max(centr2.vals.flt)/min(centr2.vals.flt)<100) axis(1) else eaxis(1, n.axp=1)
		if(max(centr1.vals.flt)/min(centr1.vals.flt)<100) axis(2) else eaxis(2, n.axp=1)
		# mean
#		idx <- which(!is.nan(avg.centr2.vals.unf) & avg.centr2.vals.unf>0)
#		lines(	
#			x=idx, avg.centr2.vals.unf[idx],
#			col=col
#		)
#		# fitted line
#		threshold <- min(cut.centr2.vals.unf)
#		x <- seq(from=threshold, to=max(centr2.vals.unf), by=(max(centr2.vals.unf)-threshold)/100)
#		lines(x, predict(fit, list(cut.centr2.vals.unf=x)), col="BLACK", lty=2)
#		# legend
#		legend(
#			x="topright",
#			lty=1:2, col=c(col,"BLACK"),
#			legend=c("Mean","Fit")
#		)
		
		#### handle filtered data
		tlog(6,"Dealing with the filtered data")
		corr.mat.flt.raw[centr1.name,centr2.name] <- cor(centr1.vals.flt, centr2.vals.flt, method="spearman")
		tlog(8,"Spearman correlation before cleaning: ", corr.mat.flt.raw[centr1.name,centr2.name])
		# filter out zero values and NaN
		idx <- which(!is.nan(centr2.vals.flt) & centr2.vals.flt>0 & !is.nan(centr1.vals.flt) & centr1.vals.flt>0)
		centr1.vals.flt <- centr1.vals.flt[idx]
		centr2.vals.flt <- centr2.vals.flt[idx]
		corr.mat.flt.clean[centr1.name,centr2.name] <- cor(centr1.vals.flt, centr2.vals.flt, method="spearman")
		tlog(8,"Spearman correlation after cleaning: ", corr.mat.flt.clean[centr1.name,centr2.name])
		avg.centr2.vals.flt <- sapply(1:max(centr2.vals.flt), function(d) mean(centr1.vals.flt[centr2.vals.flt==d]))
		
#		# keep tail
#		thresholds <- quantile(centr2.vals.flt, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
#		threshold <- thresholds[7]	# exp=1.53
#		cut.centr1.vals.flt <- centr1.vals.flt[centr2.vals.flt>=threshold]
#		cut.centr2.vals.flt <- centr2.vals.flt[centr2.vals.flt>=threshold]
#		
#		# init parameters using a linear regression
#		fit <- lm(log(cut.centr1.vals.flt) ~ log(cut.centr2.vals.flt))
#		summary(fit)
#		params <- fit$coefficients
#		val1 <- exp(params[1]); names(val1) <- NULL
#		val2 <- params[2]; names(val2) <- NULL
#		val3 <- 0
#		
#		# perform NL regression
#		df <- data.frame(cut.centr2.vals.flt, cut.centr1.vals.flt)
#		fit <- nlsLM(cut.centr1.vals.flt ~ c1*cut.centr2.vals.flt^c2, 
#				start=list(c1=val1, c2=val2),
#				data=df,
#				control=list(maxiter=200))
#		summary(fit)
		
		# plot as an inset
		col <- pal[2]
		col.sec <- combine.colors(col, "WHITE", transparency=20)
		par(
			fig=inlay.coords[paste0(centr1.name,"_",centr2.name),], 
			new=TRUE,
			mgp=c(3,0.5,0)
		)
		# points
		#rect(0.06,0.56, 0.47, 0.97, border=NA, col="WHITE")
		plot(
			x=centr2.vals.flt, y=centr1.vals.flt, 
			xlab=NA, ylab=NA,
			log="xy", yaxt="n", xaxt="n",
			las=1, col=col,
			cex.lab=0.75, cex.axis=0.75, cex=0.75
		)
		if(max(centr2.vals.flt)/min(centr2.vals.flt)<100) axis(1, cex.axis=0.75) else eaxis(1, n.axp=1, cex.axis=0.75)
		if(max(centr1.vals.flt)/min(centr1.vals.flt)<100) axis(2, cex.axis=0.75) else eaxis(2, n.axp=1, cex.axis=0.75)
#		# mean
#		idx <- which(!is.nan(avg.centr2.vals.flt) & avg.centr2.vals.flt>0)
#		lines(	
#			x=idx, avg.centr2.vals.flt[idx],
#			col=col
#		)
#		# fitted line
#		threshold <- min(cut.centr2.vals.flt)
#		x <- seq(from=threshold, to=max(centr2.vals.flt), by=(max(centr2.vals.flt)-threshold)/100)
#		lines(x, predict(fit, list(cut.centr2.vals.flt=x)), col="BLACK", lty=2)
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

# display correlation matrices
tlog(0, "Spearman correlation matrices:")
tlog(2, "Raw unfiltered data:")
print(corr.mat.unf.raw)
tlog(2, "Clean unfiltered data:")
print(corr.mat.unf.clean)
tlog(2, "Raw filtered data:")
print(corr.mat.flt.raw)
tlog(2, "Clean filtered data:")
print(corr.mat.flt.clean)




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
