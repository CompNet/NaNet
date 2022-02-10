# Additional plots regarding character stats in the corpus
# (raw data, not the networks).
# 
# Vincent Labatut
# 12/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/char_distr.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="CharDistr")




###############################################################################
# note: result of Clauset et al.'s method (already computed elsewhere)
laws <- c()
# unfiltered 
#	volumes/char = none
laws["Unfiltered-characters-volume"] <- "none"
# 	pages/char = good
laws["Unfiltered-characters-page"] <- "good"
#	scenes/char = good
laws["Unfiltered-characters-scene"] <- "good"
#	panels/char = good
laws["Unfiltered-characters-panel"] <- "good"
# filtered 
#	volumes/char = truncated
laws["Filtered-characters-volume"] <- "truncated"
#	pages/char = good
laws["Filtered-characters-page"] <- "good"
#	scenes/char = good
laws["Filtered-characters-scene"] <- "good"
#	panels/char = good
laws["Filtered-characters-panel"] <- "good"




###############################################################################
# distribution plots
tlog(0,"Producing distribution plots")

# loop params
object <- "characters"
counts <- c("scene","volume","page","panel")

# process each count type
for(count in counts)
{	tlog(2,"Dealing with ",count)
	
	# load precomputed data
	data <- list()
	file <- get.path.stat.corpus(object=object, desc=paste0("chars_distrib_",count,"_nbr"))
	data[[1]] <- as.matrix(read.csv(file=paste0(file,"_rawvals.csv")))
	file <- get.path.stat.corpus(object=object, desc=paste0("chars_filtered_distrib_",count,"_nbr"))
	data[[2]] <- as.matrix(read.csv(file=paste0(file,"_rawvals.csv")))
	names(data) <- c("Unfiltered","Filtered")
	
	# set params
	file <- get.path.stat.corpus(object=object, desc=paste0("chars_both_distrib_",count,"_nbr"))
	pal <- get.palette(length(data))
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
		if(laws[paste0(names(data)[i],"-",object,"-",count)]=="good")
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
		plot.ccdf(data=data, main=NA, xlab=xl, log=TRUE, cols=pal, leg.title="Characters")
		for(i in 1:2)
		{	if(laws[paste0(names(data)[i],"-",object,"-",count)]=="truncated")
			{	x <- seq(pl[[2]]$threshold,max(data[[2]]))
				y <- 1 - cumsum(ddiscpowerexp(x=x,exponent=pl[[2]]$exponent,rate=pl[[2]]$rate,threshold=pl[[2]]$threshold))
				lines(x, y, col="BLACK", lty=2)
			}
			if(laws[paste0(names(data)[i],"-",object,"-",count)]=="good")
				lines(pl[[i]], col="BLACK", lty=2)
		}
		dev.off()
	}
}




###############################################################################
# additional stats

# load list of chars by volume
file <- get.path.stat.corpus(object="volumes", desc="volumes")
con <- file(paste0(file,"_chars.txt"),open="r")
	lines <- readLines(con) 
close(con)

# read the graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
# clean names
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
# get the main characters
filt.names <- V(g)$name[V(g)$Filtered]
if(length(filt.names)==0) stop("Empty list of filtered characters")
main.chars <- V(g)$name[!V(g)$Filtered]

# compute char by vol
char.unfilt.nbrs <- c()
char.filt.nbrs <- c()
for(line in lines)
{	chars <- strsplit(line,split=",",fixed=TRUE)[[1]]
	char.unfilt.nbrs <- c(char.unfilt.nbrs, length(chars))
	char.filt.nbrs <- c(char.filt.nbrs, length(setdiff(chars,main.chars)))
}
char.nbr <- char.nbr / length(lines)
tlog(0,"Char by volume: ",sum(char.unfilt.nbrs)/length(lines)," (unfiltered) vs. ",sum(char.filt.nbrs)/length(lines)," (filtered)")

# test distributions
file <- get.path.stat.corpus(object="volumes", desc="volumes_distrib_char_nbr_distrtest")
test.disc.distr(data=char.unfilt.nbrs, 			# good
		xlab="Number of characters by volume", return_stats=TRUE, 
		plot.file=file)
file <- get.path.stat.corpus(object="volumes", desc="volumes_distrib_char_filtered_nbr_distrtest")
test.disc.distr(data=char.filt.nbrs, 			# good
		xlab="Number of characters by volume", return_stats=TRUE, 
		plot.file=file)




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
