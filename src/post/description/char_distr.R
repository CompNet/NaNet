# Additional plots regarding character stats in the corpus
# (raw data, not the networks).
# 
# Vincent Labatut
# 12/2021
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/char_distr.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/include.R")
start.rec.log(text="CharDistr") 




###############################################################################
# note: result of Clauset et al.'s method (already computed elsewhere)
laws <- c()
# unfiltered 
#	panels/char = good
laws["Unfiltered-characters-panel"] <- "good"
# 	pages/char = good
laws["Unfiltered-characters-page"] <- "good"
#	scenes/char = good
laws["Unfiltered-characters-scene"] <- "good"
#	volumes/char = none
laws["Unfiltered-characters-volume"] <- "none"
#	arcs/char = none
laws["Unfiltered-characters-arc"] <- "none"
# filtered 
#	panels/char = good
laws["Filtered-characters-panel"] <- "good"
#	pages/char = good
laws["Filtered-characters-page"] <- "good"
#	scenes/char = good
laws["Filtered-characters-scene"] <- "good"
#	volumes/char = truncated
laws["Filtered-characters-volume"] <- "truncated"
#	arcs/char = moderate
laws["Filtered-characters-arc"] <- "moderate"




###############################################################################
# distribution plots
tlog(0,"Producing distribution plots")

pal <- ATT_COLORS_FILT[c("Discard","Keep")]

# loop params
object <- "characters"
counts <- c("panel","page","scene","volume","arc")
col.counts <- c("panel"=COL_PANELS, "scene"=COL_SCENES, "page"=COL_PAGES, "volume"=COL_VOLUMES, "arc"=COL_ARCS)

# load the tables
file <- get.path.stats.corpus(object="characters", subfold="unfiltered", pref="_char_stats.csv")
tt1 <- read.csv(file=file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
file <- get.path.stats.corpus(object="characters", subfold="filtered", pref="_char_stats.csv")
tt2 <- read.csv(file=file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)

# process each count type
for(count in counts)
{	tlog(2,"Dealing with ",count)
	
	# load precomputed data
	data <- list()
	data[[1]] <- tt1[,col.counts[count]]
	data[[2]] <- tt2[,col.counts[count]]
	names(data) <- c("Unfiltered","Filtered")
	
	# set params
	file <- get.path.stats.corpus(object=object, subfold="both", pref=paste0("distrib_",count,"s-by-char"))
	tlog(4,"Producing files ",file)
	ml <- paste0("Distribution of ",count," number over characters")
	xl <- paste0("Number of ",count,"s by character")
	
	# check distribution
	pl <- list()
	for(i in 1:length(data))
	{	power.law <- displ$new(data[[i]])
		est <- estimate_xmin(power.law)
		tmp <- power.law$setXmin(est)
		if(laws[paste0(names(data)[i],"-",object,"-",count)]=="truncated")
			pl[[i]] <- discpowerexp.fit(x=data[[i]],threshold=power.law$xmin)
		if(laws[paste0(names(data)[i],"-",object,"-",count)] %in% c("good","moderate"))
			pl[[i]] <- power.law
	}
	print(pl)
	
	# plot distributions
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
		par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
		plot.ccdf(data=data, main=NA, xlab=xl, ylab="default", log=TRUE, cols=pal, leg.title="Characters")
		for(i in 1:2)
		{	if(laws[paste0(names(data)[i],"-",object,"-",count)]=="truncated")
			{	x <- seq(pl[[2]]$threshold,max(data[[2]]))
				y <- 1 - cumsum(ddiscpowerexp(x=x,exponent=pl[[2]]$exponent,rate=pl[[2]]$rate,threshold=pl[[2]]$threshold))
				lines(x, y, col="BLACK", lty=2)
			}
			if(laws[paste0(names(data)[i],"-",object,"-",count)] %in% c("good","moderate"))
				lines(pl[[i]], col="BLACK", lty=2)
		}
		dev.off()
	}
}




###############################################################################
# additional stats

# load corpus stats
data <- read.corpus.data()

# get the main characters
filt.names <- data$char.stats[data$char.stats[,COL_FILTER]=="Discard",COL_NAME]
if(length(filt.names)==0) stop("Empty list of filtered characters")
main.chars <- data$char.stats[data$char.stats[,COL_FILTER]=="Keep",COL_NAME]

# compute char by vol
char.unfilt.nbrs <- c()
char.filt.nbrs <- c()
for(chars in data$volume.chars)
{	char.unfilt.nbrs <- c(char.unfilt.nbrs, length(chars))
	char.filt.nbrs <- c(char.filt.nbrs, length(setdiff(chars,main.chars)))
}
tlog(0,"Char by volume: ",sum(char.unfilt.nbrs)/length(lines)," (unfiltered) vs. ",sum(char.filt.nbrs)/length(lines)," (filtered)")

# test distributions
file <- get.path.stats.corpus(object="volumes", pref="distrib_chars-by-volume_unfiltered_distrtest")
tlog(2,"Producing files ",file)
test.disc.distr(data=char.unfilt.nbrs, 			# good
	xlab="Number of characters by volume", return_stats=TRUE, 
	plot.file=file)
file <- get.path.stats.corpus(object="volumes", pref="distrib_chars-by-volume_filtered_distrtest")
tlog(2,"Producing files ",file)
test.disc.distr(data=char.filt.nbrs, 			# good
	xlab="Number of characters by volume", return_stats=TRUE, 
	plot.file=file)




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
