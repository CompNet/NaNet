# Plots the timeline of a specific character, showing the pages in which (s)he appears.
# This version of the script requires the explicit annotations.
# 
# Vincent Labatut
# 11/2023
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/explicit/char_timeline.R")
###############################################################################
library("plot.matrix")

SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="CharTimeline")




###############################################################################
# define targeted characters
char.names <- c("Thorgal Aegirsson", "Aaricia Gandalfsdottir", "Kriss de Valnor", "Jolan Thorgalsson", "Louve Thorgalsdottir", "Muff")
char.det <- "explicit"
tlog(0,"Dealing with characters ",paste0(char.names,collapse=", "))




###############################################################################
# read data
tlog(0,"Reading data (",char.det,")")
data <- read.corpus.data(char.det=char.det)
page.chars <- data$page.chars
scene.chars <- data$scene.chars
scene.stats <- data$scene.stats
volume.chars <- data$volume.chars
panel.stats <- data$panel.stats
panel.chars <- data$panel.chars




###############################################################################
tlog(0,"Complementing panel table")
# complement panel table with approx scene id
sc.ids <- get.approx.scenes.for.panels(scene.stats)
panel.stats <- cbind(panel.stats, sc.ids)
colnames(panel.stats)[ncol(panel.stats)] <- COL_SCENE_ID





###############################################################################
tlog(0,"Looping over temporal units")
for(tu in c("page", "scene", "volume"))
{	tlog(2,"Temporal unit: ",tu)
	if(tu=="page")
	{	tu.chars <- page.chars
		tu.col <- COL_PAGE_ID
		tu.xlab <- "Page"
		key.param <- 20
		tu.at <- seq(0, ncol(m), 100)
	}
	else if(tu=="scene")
	{	tu.chars <- scene.chars
		tu.col <- COL_SCENE_ID
		tu.xlab <- "Scene"
		key.param <- 20
		tu.at <- seq(0, ncol(m), 100)
	}
	else if(tu=="volume")
	{	tu.chars <- volume.chars
		tu.col <- COL_VOLUME_ID
		tu.xlab <- "Volume"
		key.param <- 1
		tu.at <- seq(0, ncol(m), 10)
	}
	
	# count characters
	tlog(4,"Counting characters")
	m <- matrix(0, nrow=length(char.names), ncol=length(tu.chars))
	rownames(m) <- char.names
	for(tu.id in 1:length(tu.chars))
	{	tlog(6,"Processing ",tu," ",tu.id,"/",length(tu.chars))
		panels <- which(panel.stats[,tu.col]==tu.id)
		for(char.name in char.names)
		{	nbr <- length(which(char.name==unlist(panel.chars[panels])))
			m[char.name,tu.id] <- nbr
		}
	}
	
	# setup breaks
	breaks <- c(0, 1, seq(2,max(m),(max(m)-2)%/%4))
	breaks <- c(breaks[-length(breaks)], max(m))
		
	# produce plot
	plot.file <- get.path.stats.corpus(object="characters", char.det=char.det, subfold="timeline", pref=paste0("timeline_",tu,"s"))
	pdf(paste0(plot.file,".pdf"), bg="white", width=15, height=3)
		par(mar=c(5,7,4,5))		# plot margins: Bottom=5 Left=4 Top=4 Right=2
		plot(m, border=NA, col=viridis, breaks=breaks, las=2, xlab=tu.xlab, ylab=NA, main="Number of occurrences (panels)", cex.axis=0.7, spacing.key=c(key.param,key.param,0), axis.col=NULL)
		axis(1, at=tu.at, las=2)
	dev.off()
}





###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
