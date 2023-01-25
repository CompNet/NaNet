# Additional distribution plots regarding character degree and strength.
# The script focuses on the graphs corresponding to certain extraction 
# parameters of interest.
# 
# Vincent Labatut
# 01/2023
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/explicit/deg_plots.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="DegDistr")




###############################################################################
# distribution plots
tlog(0,"Producing degree distribution plots")

meas <- MEAS_DEGREE

# load precomputed data
file <- get.path.stat.table(object="nodes", mode="panel.window", char.det="implicit", net.type="static", window.size=1, overlap=0, weights="none", filtered="unfiltered", compare=FALSE)
tab <- as.matrix(read.csv(file, header=TRUE, check.names=FALSE, row.names=1))
vals <- tab[,meas]
#vals <- vals[vals>0] # remove isolates?

# test and plot distribution
plot.file <- get.path.stats.topo(mode="panel.window", char.det="implicit", net.type="static", meas.name=meas, window.size=1, overlap=0, weights="none", filtered="unfiltered", suf="histo")
plot.disc.distribution(
	vals=vals, 
	xlab=ALL_MEASURES[[meas]]$cname, main=NA, 
	freq=FALSE,
	log=TRUE, cols=ATT_COLORS_FILT["Discard"], 
	#leg.title=NA, leg.pos="topright", las=1, 
	export=FALSE, file=plot.file, 
	histo=FALSE, ccdf=TRUE, test=TRUE
)

######

# load precomputed data
file <- get.path.stat.table(object="nodes", mode="page.window", char.det="implicit", net.type="static", window.size=1, overlap=0, weights="none", filtered="unfiltered", compare=FALSE)
tab <- as.matrix(read.csv(file, header=TRUE, check.names=FALSE, row.names=1))
vals <- tab[,meas]
#vals <- vals[vals>0] # remove isolates?

# test and plot distribution
plot.file <- get.path.stats.topo(mode="page.window", char.det="implicit", net.type="static", meas.name=meas, window.size=1, overlap=0, weights="none", filtered="unfiltered", suf="histo")
plot.disc.distribution(
	vals=vals, 
	xlab=ALL_MEASURES[[meas]]$cname, main=NA, 
	freq=FALSE,
	log=TRUE, cols=ATT_COLORS_FILT["Discard"], 
	#leg.title=NA, leg.pos="topright", las=1, 
	export=FALSE, file=plot.file, 
	histo=FALSE, ccdf=TRUE, test=TRUE
)




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
