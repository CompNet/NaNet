# Compares the centrality of Thorgal and Jolan on certain volumes of interest.
# 
# Vincent Labatut
# 03/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/char_tj_comp.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="CharComp")




################################################################################
# main parameters
filtered <- FALSE
filt.txt <- if(filtered) "filtered" else "unfiltered"

chars <- c("Thorgal Aegirsson","Jolan Thorgalsson")




################################################################################
tlog(0,"Load data and network")

# read raw data
tlog(2,"Reading previously computed corpus stats")
data <- read.corpus.data()
char.stats <- data$char.stats
volume.stats <- data$volume.stats
volume.chars <- data$volume.chars
scene.stats <- data$scene.stats

# monitored characters
chars.short <- char.stats[match(chars,char.stats[,COL_NAME]),COL_NAME_SHORT]
pal <- get.palette(5)[c(1,4)]

# monitored volumes
#vols <- c(
#	"28", "29",
#	"30", "31", "32",#"K1",
#	"33", #"L1", "K2",
#	"K3", #"L2", "J1", "L3",
#	"K4", "34", #"J2","L4",
#	"K5", #"L5", "J3", 
#	"K6", #"J4","L6",
#	"35"
#)
vols <- volume.stats[,COL_VOLUME]
vols <- vols[which(sapply(1:length(volume.chars), function(v) length(intersect(chars, volume.chars[[v]]))>0))]
vols <- vols[-(1:11)]								# remove earlier volumes to save some space
vols <- vols[-length(vols)]							# same with latter volumes
vols <- vols[-which(grepl("J", vols, fixed=TRUE))]	# remove Young Thorgal volumes
vols.idx <- match(vols, volume.stats[,COL_VOLUME])

# read volume stats
stats <- list() 
for(v in 1:nrow(volume.stats))
{	table.file <- get.path.stat.table(object="nodes", mode="scenes", weights="duration", vol=volume.stats[v,COL_VOLUME], filtered=filt.txt)
	tt <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	idx <- match(chars,rownames(tt))
	if(v==1)
	{	for(i in 1:length(chars))
		{	if(!is.na(idx[i]))
				stats[[chars[i]]] <- tt[idx[i],]
			else
				stats[[chars[i]]] <- rep(NA,ncol(tt))
		}
	}
	else
	{	for(i in 1:length(chars))
		{	if(!is.na(idx[i]))
				stats[[chars[i]]] <- cbind(stats[[chars[i]]], tt[idx[i],])
			else
				stats[[chars[i]]] <- cbind(stats[[chars[i]]], rep(NA,ncol(tt)))
		}
	}
}

# select measure
meass <- c(MEAS_DEGREE, MEAS_BETWEENNESS, MEAS_CLOSENESS, MEAS_EIGENCNTR, MEAS_STRENGTH)
meas <- meass[5]
w.name <- if(meas==MEAS_STRENGTH) "duration" else NA
			
# get values
vals <- c(stats[[1]][meas,vols.idx], stats[[2]][meas,vols.idx])
vals <- vals[!is.na(vals)]

# create the plot
plot.file <- get.path.topomeas.plot(net.type="static", mode="scenes", meas.name=meas, vol=TRUE, filtered=filt.txt, weights=w.name, plot.type=paste0("evolution_",paste0(chars.short,collapse="--")))
tlog(6, "Plotting in file \"",plot.file,"\"")
for(fformat in PLOT_FORMAT)
{	if(fformat==PLOT_FORMAT_PDF)
		pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white", width=15, height=5)
	else if(fformat==PLOT_FORMAT_PNG)
		png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=2400, height=800, units="px", pointsize=20, bg="white")
	par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		# init empty plot
		plot(
			NULL,
			xlim=c(1,length(vols)), ylim=range(vals),
			xlab="Volume", ylab=NODE_MEASURES[[meas]]$cname,
			xaxt="n"
		)
		axis(1,at=1:length(vols), labels=vols, cex.axis=0.75)
		# add mean value
		for(i in 1:length(chars))
			abline(h=mean(stats[[i]][meas,]), lty=2, col=pal[i])
		# add lines
		for(i in 1:length(chars))
		{	lines(
				x=1:length(vols), y=stats[[i]][meas,vols.idx],
				col=pal[i]
			)
		}
		# legend
		legend(
			title="Characters",
			x="topleft",
			fill=pal,
			legend=chars.short
		)
	# close file
	dev.off()
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
