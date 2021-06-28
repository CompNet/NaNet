# Identificatio of the main characters in each volume. First, the script builds a ground 
# truth based on textual summaries of each volume, using character occurrence as a proxy
# for character importance. Second, it uses centrality measures to identify the main
# characters in the graph. Third, it compares both groups.
# 
# Vincent Labatut
# 06/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post.R")
###############################################################################
source("src/common/include.R")
library("stringr")




# load series data
data <- read.raw.data()

folder <- file.path(DATA_FOLDER,"summaries")
volumes <- data$volume.info[,COL_PAGES_VOLUME]

# load text files
lines <- c()
for(v in volumes)
{	con <- file(file.path(folder, paste0(v,".txt")), "r")
	lines <- c(lines, tolower(paste(readLines(con), collapse=" ")))
	close(con)
}

# look for characters
freq.tab <- matrix(NA, nrow=nrow(data$char.info), ncol=length(volumes))
colnames(freq.tab) <- volumes
rownames(freq.tab) <- data$char.info[,COL_CHAR_SHORT_NAME]
for(c in 1:nrow(data$char.info))
{	name <- tolower(data$char.info[c, COL_CHAR_SHORT_NAME])
	tlog(0,"Processing character \"",name,"\"")
	#grepl(name, lines, fixed=TRUE)
	freq.tab[c, ] <- str_count(lines, paste0("\\b",name,"\\b"))
}

# normalize frequencies
freq.tab <- apply(freq.tab, 2, function(vect) vect/sum(vect))

# show ground truth
for(v in volumes)
{	tlog(2, "Processing volume ",v)
	idx <- which(freq.tab[,v]>0)
	for(i in idx)
		tlog(4, data$char.info[i,COL_CHAR_SHORT_NAME], ": ", freq.tab[i, v])
	tlog(2, "")
}

# estimate from graph
est.tab <- matrix(NA, nrow=nrow(data$char.info), ncol=length(volumes))
colnames(est.tab) <- volumes
rownames(est.tab) <- data$char.info[,COL_CHAR_SHORT_NAME]
for(v in volumes)
{	tlog(2, "Processing volume ",v)
	g <- extract.static.graph.scenes(char.volumes=data$char.volumes, char.info=data$char.info, page.info=data$page.info, inter.df=data$inter.df, vol=v)
	vals <- strength(graph=g, mode="all", weights=E(g)$Durations)
	names(vals) <- V(g)$ShortName
	est.tab[names(vals), v] <- vals
}

# compare with ground truth
#threshold <- 5
meas.names <- c("Precision", "Recall", "F-measure", "Spearman's rho", "Spearman's p-value")
stats <- matrix(nrow=length(meas.names), ncol=length(volumes))
colnames(stats) <- volumes
rownames(stats) <- meas.names
for(v in volumes)
{	tlog(2, "Processing volume ",v)
	#ts <- min(which(freq.tab[,v]>0), threshold)
	
	# get ground truth
	gt <- freq.tab[freq.tab[,v]>0,v]
	gt <- names(gt[order(gt,decreasing=TRUE)])
	
	# get estimation
	#es <- est.tab[est.tab[,v]>0,v]
	es <- est.tab[,v]
	es <- names(es[order(es,decreasing=TRUE)])
	
	# compute Precision, Recall, F-measure for the top estimated characters
	es1 <- es[es>0]
	if(length(es)>=length(gt))
		es1 <- es[1:length(gt)]
	char.correct <- intersect(es1, gt)
	char.surnum <- setdiff(es1, gt)
	char.missing <- setdiff(gt, es1)
	tlog(4, "Correct characters: ",paste(char.correct,collapse=", "))
	tlog(4, "Superfluous characters: ",paste(char.surnum,collapse=", "))
	tlog(4, "Missing characters: ",paste(char.missing,collapse=", "))
	tp <- length(char.correct)
	fp <- length(char.surnum)
	fn <- length(char.missing)
	pre <- tp/(tp+fp)
	rec <- tp/(tp+fn)
	fmeas <- 2*(pre*rec)/(pre+rec)
	tlog(4, "Pre=",pre," Rec=",rec," F=",fmeas)
	stats[c("Precision","Recall","F-measure"),v] <- c(pre,rec,fmeas)
	
	# compute spearman's correlation for the GT characters
	lv <- sort(unique(union(es,gt)))
	es2 <- intersect(es, gt)
	print(cbind(gt,es2))
	es2 <- as.integer(factor(es2, levels=lv))
	gt2 <- as.integer(factor(gt, levels=lv))
	rcor <- cor.test(x=gt2, y=es2, method="spearman")
	tlog(4, "Spearman: cor=",rcor$estimate," p=",rcor$p.value)
	stats[c("Spearman's rho", "Spearman's p-value"),v] <- c(rcor$estimate,rcor$p.value)
}
