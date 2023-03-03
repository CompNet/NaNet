# Extracts dynamic ASOIAF networks. The "publication" order follows the comic publication
# order. The "story" order matches parts of issues to novel chapter and reorder them
# accordingly, in order to match the books.
# 
# Vincent Labatut
# 02/2023
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/asoiaf/extr_dyn.R")
###############################################################################
SERIES <- "ASOIAF"
source("src/common/_include.R")
start.rec.log(text="ExtrDyn")




################################################################################
tlog(0,"Extract dynamic networks for ASOIAF")

# read raw data
tlog(2,"Reading previously computed corpus stats")
data <- read.corpus.data(char.det="implicit")
inter.df <- data$inter.df
char.stats <- data$char.stats
scene.stats <- data$scene.stats
scene.chars <- data$scene.chars
panel.stats <- data$panel.stats
page.stats <- data$page.stats
volume.stats <- data$volume.stats
scene.stats <- data$scene.stats

# extract dynamic networks using the comic publication order (=comic story order)
tlog(2,"Extracting publication-ordered dynamic networks")
for(filtered in c(FALSE,TRUE))
{	tlog(4,"Dealing with ",if(filtered) "" else "un","filtered networks")
	gg <- cum.graph.extraction(
			inter.df=inter.df,
			char.stats=char.stats, 
			scene.chars=scene.chars, scene.stats=scene.stats, 
			volume.stats=volume.stats, 
			filtered=filtered, 
			pub.order=TRUE,
			narr.unit="scene"
	)
	cum.write.graph(gs=gg, filtered=filtered, pub.order=TRUE, char.det="implicit")
	gg <- cum.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=TRUE, char.det="implicit")
}
	



###############################################################################
# extract dynamic networks using the novel publication order (slightly different from the comic's)
tlog(2,"Extracting novel-ordered dynamic networks")

# read map file
map.file <- file.path(DATA_FOLDER,"mapping.csv")
map <- read.csv(map.file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
# compute new page ranks
parts <- sapply(1:nrow(page.stats), function(p) map[map[,COL_VOLUME]==page.stats[p,COL_VOLUME] & map[,COL_PAGE_START]<=page.stats[p,COL_PAGE] & map[,COL_PAGE_END]>=page.stats[p,COL_PAGE], COL_RANK])
page.stats[,COL_RANK] <- rank(parts*(nrow(page.stats)+1) + page.stats[,COL_PAGE])
# compute other ranks based on new page ranks
panel.stats[,COL_RANK] <- rank(page.stats[panel.stats[,COL_PAGE_ID],COL_RANK]*(nrow(panel.stats)+1) + panel.stats[,COL_PANEL])
scene.stats[,COL_RANK] <- rank(page.stats[scene.stats[,COL_PAGE_START_ID],COL_RANK]*(nrow(scene.stats)+1) + scene.stats[,COL_SCENE_ID], ties.method="first")
inter.df[,COL_RANK] <- rank(page.stats[inter.df[,COL_PAGE_START_ID],COL_RANK]*(nrow(inter.df)+1) + 1:nrow(inter.df), ties.method="first")

# extract dynamic networks using the novel chapter order
tlog(2,"Extracting publication-ordered dynamic networks")
for(filtered in c(FALSE,TRUE))
{	tlog(4,"Dealing with ",if(filtered) "" else "un","filtered networks")
	gg <- cum.graph.extraction(
			inter.df=inter.df,
			char.stats=char.stats, 
			scene.chars=scene.chars, scene.stats=scene.stats, 
			volume.stats=volume.stats, 
			filtered=filtered, 
			pub.order=FALSE,
			narr.unit="scene"
	)
	cum.write.graph(gs=gg, filtered=filtered, pub.order=FALSE, char.det="implicit")
	gg <- cum.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=FALSE, char.det="implicit")
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
