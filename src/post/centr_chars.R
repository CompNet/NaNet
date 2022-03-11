# Identification of the main characters in each volume. First, the script builds a ground 
# truth based on textual summaries of each volume, using character occurrence as a proxy
# for character importance. Second, it uses centrality measures to identify the main
# characters in the graph. Third, it compares both groups.
# 
# Vincent Labatut
# 06/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/centr_chars.R")
###############################################################################
source("src/common/include.R")
library("stringr")




# load series data
data <- read.raw.data()

folder <- file.path(DATA_FOLDER,"summaries")
volumes <- data$volume.stats[,COL_VOLUME]

# load text files
lines <- c()
for(v in volumes)
{	con <- file(file.path(folder, paste0(v,".txt")), "r")
		lines <- c(lines, tolower(paste(readLines(con), collapse=" ")))
	close(con)
}

# look for characters
freq.tab <- matrix(NA, nrow=nrow(data$char.stats), ncol=length(volumes))
colnames(freq.tab) <- volumes
rownames(freq.tab) <- data$char.stats[,COL_NAME_SHORT]
for(c in 1:nrow(data$char.stats))
{	name <- tolower(data$char.stats[c, COL_NAME_SHORT])
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
		tlog(4, data$char.stats[i,COL_NAME_SHORT], ": ", freq.tab[i, v])
	tlog(2, "")
}

# estimate from graph
est.tab <- matrix(NA, nrow=nrow(data$char.stats), ncol=length(volumes))
colnames(est.tab) <- volumes
rownames(est.tab) <- data$char.stats[,COL_NAME_SHORT]
for(v in volumes)
{	tlog(2, "Processing volume ",v)
	graph.file <- get.path.graph.file(mode="scenes", vol=v, filtered=FALSE, desc="static", ext=".graphml")
	g <- read_graph(file=graph.file, format="graphml")
	
#	vals <- degree(graph=g, mode="all")									# 0.55	0.25
#	vals <- strength(graph=g, mode="all", weights=E(g)$Duration)		# 0.55	0.22
#	vals <- strength(graph=g, mode="all", weights=E(g)$Occurrences)		# 0.56	0.24
	#
#	vals <- betweenness(graph=g, weights=NULL)							# 0.59	0.21
#	vals <- betweenness(graph=g, weights=E(g)$Duration)					# 0.41	0.19
#	vals <- betweenness(graph=g, weights=E(g)$Occurrences)				# 0.46	0.22
	#
#	vals <- closeness(graph=g, weights=NULL)							# 0.56	0.22
#	vals <- closeness(graph=g, weights=E(g)$Duration)					# 0.36	0.21
#	vals <- closeness(graph=g, weights=E(g)$Occurrences)				# 0.37	0.18
	#
#	vals <- harmonic_centrality(x=g, weights=NULL)						# 0.56	0.24
#	vals <- harmonic_centrality(x=g, weights=E(g)$Duration)				# 0.34	0.15
#	vals <- harmonic_centrality(x=g, weights=E(g)$Occurrences)			# 0.40	0.13
	#
#	vals <- eigen_centrality(graph=g, weights=NULL)$vector				# 0.49	0.23
#	vals <- eigen_centrality(graph=g, weights=E(g)$Duration)$vector		# 0.53	0.27
#	vals <- eigen_centrality(graph=g, weights=E(g)$Occurrences)$vector	# 0.53	0.28
	
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
	if(pre==0 && rec==0)
		fmeas <- 0
	else
		fmeas <- 2*pre*rec/(pre+rec)
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

print(rowMeans(stats))
# TODO certains volumes aboutissent à des résultats anormalement bas
# >> voir lesquels et pourquoi
