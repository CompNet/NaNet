# Compares the centrality of Thorgal and Jolan on certain volumes of interest.
# 
# Vincent Labatut
# 03/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/char_tj_comp.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="CharComp")




################################################################################
# main parameters




################################################################################
tlog(0,"Load data and network")

# read raw data
tlog(2,"Reading previously computed corpus stats")
data <- read.corpus.data()
char.stats <- data$char.stats
volume.stats <- data$volume.stats
scene.stats <- data$scene.stats

# monitored characters
chars <- c("Thorgal Aegirsson","Jolan Thorgalsson")
idx <- match(chars,char.stats[,COL_NAME])
pal <- get.palette(5)[c(1,4)]

# monitored volumes
vols <- c("30","31","32","33","K3", "K4", "34", "K5", "K6", "35")
vols.idx <- match(vols, volume.stats[,COL_VOLUME])

# read volume stats
stats <- list() 
for(v in 1:nrow(volume.stats))
{	table.file <- get.path.stat.table(object="nodes", mode="scenes", weights="duration", vol=volume.stats[v,COL_VOLUME], filtered=FALSE)
	tt <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	if(v==1)
	{	stats[[chars[1]]] <- tt[idx[1],]
		stats[[chars[2]]] <- tt[idx[2],]
	}
	else
	{	stats[[chars[1]]] <- cbind(stats[[chars[1]]], tt[idx[1],])
		stats[[chars[2]]] <- cbind(stats[[chars[2]]], tt[idx[2],])
	}
}

# create the plot
meass <- c(MEAS_DEGREE, MEAS_BETWEENNESS, MEAS_CLOSENESS, MEAS_EIGENCNTR)
meas <- meass[1]
vals <- c(stats[[1]][meas,vols.idx], stats[[2]][meas,vols.idx])
vals <- vals[!is.na(vals)]
# init empty plot
plot(
	NULL,
	xlim=c(1,length(vols)), ylim=range(vals),
	xlab="Volume", ylab=NODE_MEASURES[[meas]]$cname
)
# add mean value
abline(h=mean(stats[[1]][meas,]), lty=2)
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
	legend=char.stats[idx,COL_NAME_SHORT]
)

















###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
