# Ad hoc plots regarding character stats in the corpus.
# 
# Vincent Labatut
# 12/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/char_distr.R")
###############################################################################
source("src/common/include.R")




# loop params
object <- "characters"
counts <- c("scene","volume","page","panel")

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
		pl[[i]] <- power.law
	}
	
	# plot distributions
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
		plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE, cols=pal, leg.title="Characters")
		for(i in 1:2)
			lines(pl[[i]], col="BLACK", lty=2)
		dev.off()
	}
}
