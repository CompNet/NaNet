# Additional plots regarding the relation between various nodal centrality
# measures and the number of occurrences of the characters.
# 
# Vincent Labatut
# 02/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/centr_vs_occ.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/include.R")
start.rec.log(text="CentrVsOcc")




###############################################################################
tlog(0, "Plotting centrality vs. occurrences")

# load corpus stats
data <- read.corpus.data()

# plot parameters
pal <- get.palette(2)

# measure names
centr.names <- c(MEAS_DEGREE, MEAS_BETWEENNESS, MEAS_CLOSENESS, MEAS_EIGENCNTR)
occ.proper.names <- c("panel"="Panel", "scene"="Scene", "page"="Page", "volume"="Volume")
occ.names <- names(occ.proper.names)

# correlation matrices
corr.mat.unf.raw <- matrix(1, nrow=length(centr.names), ncol=length(occ.names), dimnames=list(centr.names,occ.names))
corr.mat.unf.clean <- matrix(1, nrow=length(centr.names), ncol=length(occ.names), dimnames=list(centr.names,occ.names))
corr.mat.flt.raw <- matrix(1, nrow=length(centr.names), ncol=length(occ.names), dimnames=list(centr.names,occ.names))
corr.mat.flt.clean <- matrix(1, nrow=length(centr.names), ncol=length(occ.names), dimnames=list(centr.names,occ.names))

# inlay position
inlay.coords <- matrix(nrow=length(centr.names)*length(occ.names), ncol=4)
rownames(inlay.coords) <- apply(expand.grid(centr.names,occ.names), 1, function(row) paste(row, collapse="_"))
inlay.coords[paste0(MEAS_BETWEENNESS,"_page"),] <- c(0.53, 0.97, 0.05, 0.49)
inlay.coords[paste0(MEAS_CLOSENESS,"_page"),] <- c(0.51, 0.97, 0.05, 0.51)
inlay.coords[paste0(MEAS_DEGREE,"_page"),] <- c(0.52, 0.97, 0.05, 0.52)
inlay.coords[paste0(MEAS_EIGENCNTR,"_page"),] <- c(0.32, 0.97, 0.05, 0.70)
inlay.coords[paste0(MEAS_BETWEENNESS,"_panel"),] <- c(0.55, 0.97, 0.05, 0.47)
inlay.coords[paste0(MEAS_CLOSENESS,"_panel"),] <- c(0.54, 0.97, 0.05, 0.48)
inlay.coords[paste0(MEAS_DEGREE,"_panel"),] <- c(0.06, 0.46, 0.57, 0.97)
inlay.coords[paste0(MEAS_EIGENCNTR,"_panel"),] <- c(0.39, 0.97, 0.05, 0.63)
inlay.coords[paste0(MEAS_BETWEENNESS,"_scene"),] <- c(0.49, 0.97, 0.05, 0.53)
inlay.coords[paste0(MEAS_CLOSENESS,"_scene"),] <- c(0.48, 0.97, 0.05, 0.52)
inlay.coords[paste0(MEAS_DEGREE,"_scene"),] <- c(0.49, 0.97, 0.05, 0.53)
inlay.coords[paste0(MEAS_EIGENCNTR,"_scene"),] <- c(0.34, 0.97, 0.05, 0.68)
inlay.coords[paste0(MEAS_BETWEENNESS,"_volume"),] <- c(0.45, 0.97, 0.05, 0.57)
inlay.coords[paste0(MEAS_CLOSENESS,"_volume"),] <- c(0.50, 0.97, 0.05, 0.52)
inlay.coords[paste0(MEAS_DEGREE,"_volume"),] <- c(0.47, 0.97, 0.05, 0.55)
inlay.coords[paste0(MEAS_EIGENCNTR,"_volume"),] <- c(0.27, 0.97, 0.05, 0.75)

# loop over measures
tlog(0, "Loop over measures")
for(centr.name in centr.names)
{	tlog(2, "Processing centrality ",centr.name)
	
	# loop over modes
	tlog(2, "Loop over occurrence modes")
	for(occ.name in occ.names)
	{	tlog(4, "Processing mode ",occ.name)
		
		# get centrality values
		centr.vals.unf <- load.static.nodelink.stats.scenes(weights="none", measure=centr.name, filtered="unfiltered")
		centr.vals.flt <- load.static.nodelink.stats.scenes(weights="none", measure=centr.name, filtered="filtered")
		
		# get occurrence values
		file <- get.path.stats.corpus(object="characters", subfold="unfiltered", pref="_char_stats.csv")
		occ.vals.unf <- read.csv(file=file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)[,COL_FREQ]
		file <- get.path.stats.corpus(object="characters", subfold="filtered", pref="_char_stats.csv")
		occ.vals.flt <- read.csv(file=file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)[,COL_FREQ]
		
		#### handle unfiltered data
		tlog(6,"Dealing with the unfiltered data")
		corr.mat.unf.raw[centr.name,occ.name] <- cor(centr.vals.unf, occ.vals.unf, method="spearman")
		tlog(8,"Spearman correlation before cleaning: ", corr.mat.unf.raw[centr.name,occ.name])
		# filter out zero values and NaN
		idx <- which(!is.nan(occ.vals.unf) & occ.vals.unf>0 & !is.nan(centr.vals.unf) & centr.vals.unf>0)
		centr.vals.unf <- centr.vals.unf[idx]
		occ.vals.unf <- occ.vals.unf[idx]
		corr.mat.unf.clean[centr.name,occ.name] <- cor(centr.vals.unf, occ.vals.unf, method="spearman")
		tlog(8,"Spearman correlation after cleaning: ", corr.mat.unf.clean[centr.name,occ.name])
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
		plot.file <- get.path.stats.topo(net.type="static", mode="scenes", meas.name=MEAS_MULTI_NODES, weights="none", filtered="both", suf=paste0("occ_",occ.name,"_vs_",centr.name))
		tlog(8, "Plotting in file ",plot.file)
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
			log="xy", yaxt = "n",
			las=1, col=col,
			xlim=c(1,max(occ.vals.unf)*1.1)
		)
		if(centr.name==MEAS_CLOSENESS) eaxis(2, cex.axis=0.8) else eaxis(2, n.axp=1)
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
		tlog(6,"Dealing with the filtered data")
		corr.mat.flt.raw[centr.name,occ.name] <- cor(centr.vals.flt, occ.vals.flt, method="spearman")
		tlog(8,"Spearman correlation before cleaning: ", corr.mat.flt.raw[centr.name,occ.name])
		# filter out zero values and NaN
		idx <- which(!is.nan(occ.vals.flt) & occ.vals.flt>0 & !is.nan(centr.vals.flt) & centr.vals.flt>0)
		centr.vals.flt <- centr.vals.flt[idx]
		occ.vals.flt <- occ.vals.flt[idx]
		corr.mat.flt.clean[centr.name,occ.name] <- cor(centr.vals.flt, occ.vals.flt, method="spearman")
		tlog(8,"Spearman correlation after cleaning: ", corr.mat.flt.clean[centr.name,occ.name])
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
			new=TRUE,
			mgp=c(3,0.5,0)
		)
		# points
		#rect(0.06,0.56, 0.47, 0.97, border=NA, col="WHITE")
		plot(
			x=occ.vals.flt, y=centr.vals.flt, 
			xlab=NA, ylab=NA,
			log="xy", yaxt = "n", 
			las=1, col=col,
			xlim=c(1,max(occ.vals.flt)*1.1),
			cex.lab=0.75, cex.axis=0.75, cex=0.75
		)
		if(centr.name==MEAS_CLOSENESS) eaxis(2, cex.axis=0.75) else eaxis(2, n.axp=1, cex.axis=0.75)
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
		
		tlog(4, "Done with mode ",occ.name)
	}
	tlog(2, "Done with centrality ",centr.name)
}
tlog(0, "Done with the loop")

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
# check the outliers according to Eigencentrality
tlog(0,"Checking the outliers according to the Eigenvector centrality:")
centr.vals.unf <- load.static.nodelink.stats.scenes(weights="none", measure="eigenvector", filtered="unfiltered")
filtered <- data$char.stats[,COL_FILTER]=="Discard"
idx <- order(centr.vals.unf, decreasing=FALSE)
tlog(2,"List of characters by decreasing Eigenvector centrality:")
print(cbind(data$char.stats[idx,COL_NAME], filtered[idx], centr.vals.unf[idx]))
tlog(2,"Number of characters with a quasi-zero Eigencentrality: ",length(which(centr.vals.unf<1e-10)))
tlog(2,"Rank of the unfiltered characters: ")
ranks <- rank(centr.vals.unf)[!filtered]
print(cbind(data$char.stats[!filtered,COL_NAME][order(ranks)], ranks[order(ranks)]))




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
