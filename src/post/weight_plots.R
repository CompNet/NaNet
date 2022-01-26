# Ad hoc plots regarding the distribution of edge weights.
# 
# Vincent Labatut
# 01/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/weight_plot.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="WeightDistr")




###############################################################################
# note: result of Clauset et al.'s method (already computed elsewhere)
laws <- c()
# unfiltered 
laws["Unfiltered-linkweight-occurrences"] <- "truncated"
laws["Unfiltered-linkweight-duration"] <- "good"
# filtered 
laws["Filtered-linkweight-occurrences"] <- "truncated"
laws["Filtered-linkweight-duration"] <- "good"




###############################################################################
# distribution plots
tlog(0,"Producing weight distribution plots")

# retrieve the graph and the (un)filtered characters
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
idx.keep <- which(!V(g)$Filtered)

# load numbers of occurrences of characters
file <-  get.path.stat.corpus(object="characters", desc="chars_distrib_scene_nbr_rawvals.csv")
sce.nbr <- as.matrix(read.csv(file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE))[,1]

meas <- c("linkweight")
wts <- c("duration","occurrences")
# process each type of weight	
for(wt in wts)
{	tlog(4,"Dealing with weight ",wt)
	
	# load precomputed data
	data <- list()
	# unfiltered
	file <- get.path.stat.table(object="links", mode="scenes", weights=wt, filtered=FALSE)
	tab <- as.matrix(read.csv(file, header=TRUE, check.names=FALSE, row.names=1))
	data[[1]] <- tab[,meas]
	# filtered
	file <- get.path.stat.table(object="links", mode="scenes", weights=wt, filtered=TRUE)
	tab <- as.matrix(read.csv(file, header=TRUE, check.names=FALSE, row.names=1))
	data[[2]] <- tab[,meas]
	names(data) <- c("Unfiltered","Filtered")
	
	# set params
	file <- get.path.comparison.plot(object="linkweight", mode="scenes", meas.name=meas, weights=wt, filtered=FALSE, plot.type="both_distrib")
	pal <- get.palette(length(data))
	ml <- paste0(ALL_MEASURES[[meas]]$cname, " distribution (",wt,")")
	xl <- paste0(ALL_MEASURES[[meas]]$cname," (",wt,")")
	
	# check distribution
	pl <- list()
	for(i in 1:length(data))
	{	power.law <- displ$new(data[[i]])
		est <- estimate_xmin(power.law)
		tmp <- power.law$setXmin(est)
		if(laws[paste0(names(data)[i],"-",meas,"-",wt)]=="truncated")
			pl[[i]] <- discpowerexp.fit(x=data[[i]],threshold=power.law$xmin)
		else if(laws[paste0(names(data)[i],"-",meas,"-",wt)]=="good")
			pl[[i]] <- power.law
		else
			pl[[i]] <- NA
	}
	print(pl)
	
	# plot distributions
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
		par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		plot.ccdf(data=data, main=NA, xlab=xl, log=TRUE, cols=pal, leg.title="Characters")
		for(i in 1:2)
		{	if(laws[paste0(names(data)[i],"-",meas,"-",wt)]=="truncated")
			{	x <- seq(pl[[2]]$threshold,max(data[[2]]))
				y <- 1 - cumsum(ddiscpowerexp(x=x,exponent=pl[[2]]$exponent,rate=pl[[2]]$rate,threshold=pl[[2]]$threshold))
				lines(x, y, col="BLACK", lty=2)
			}
			else if(laws[paste0(names(data)[i],"-",meas,"-",wt)]=="good")
				lines(pl[[i]], col="BLACK", lty=2)
		}
		dev.off()
	}
}




###############################################################################
# end logging
end.rec.log()
