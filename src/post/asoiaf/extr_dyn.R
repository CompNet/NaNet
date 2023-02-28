# Extracts dynamic ASOIAF networks.
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
char.stats <- data$char.stats
scene.stats <- data$scene.stats
scene.chars <- data$scene.chars
volume.stats <- data$volume.stats
scene.stats <- data$scene.stats

# extract dynamic networks using the comic publication order (=comic story order)
tlog(2,"Extracting publication-ordered dynamic networks")
for(filtered in c(FALSE,TRUE))
{	tlog(4,"Dealing with ",if(filtered) "" else "un","filtered networks")
	gg <- ns.graph.extraction(
			char.stats=char.stats, 
			scene.chars=scene.chars, scene.stats=scene.stats, 
			volume.stats=volume.stats, 
			filtered=filtered, 
			pub.order=pub.order
	)
	ns.write.graph(gs=gg, filtered=filtered, pub.order="publication", char.det="implicit")
}

# extract dynamic networks using the novel publication order (slightly different from the comic's)
tlog(2,"Extracting novel-ordered dynamic networks")





###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
