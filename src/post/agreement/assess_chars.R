# This script assesses the agreement over character occurrences.
#
# IMPORTANT:
# It is meant to be applied to the first version of the corpus, 
# i.e. the one from v1.0.2 of the GitHub repository. These data
# are also available directly on Zenodo: https://doi.org/10.5281/zenodo.6573491 
# 
# Vincent Labatut
# 06/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/agreement/assess_scenes.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="AgreementScenes")




###############################################################################
tlog(0, "Comparison of scene boundaries")

# Precision and Recall are computed by comparing the estimated vs. reference characters
# for each scene. Problem is: how to match the scenes? Estimation vs. reference, or the
# opposite? I arbitrarily proceed with the former option, assuming there is not much difference
# when the matching between scenes is good enough (which is the case here). 

# load corpus stats
data <- read.corpus.data()
panel.stats <- data$panel.stats

# read the table from the main corpus
tlog(2,"Reading the interaction file '",INTER_FILE,"'")
con <- file(INTER_FILE, open="r")
temp <- readLines(con)
close(con)
temp <- fix.encoding(strings=temp)
ref.lines <- strsplit(temp, split='\t', fixed=TRUE)
tlog(4,"File read: ",length(ref.lines)," lines)")

# volumes processed by the additional annotators
vols <- c("06","16","20","K1")
# annotators' names
annotators <- c("arthur","noe","elise")

# evaluation measures
TP <- "TruePositives"
FP <- "FalsePositives"
FN <- "FalseNegatives"
PRE <- "Precision"
REC <- "Recall"
FM <- "Fmeasure"
meas.names <- c(TP, FP, FN, PRE, REC, FM)

# init stat tables
stat.tab <- matrix(0, nrow=length(vols)+1, ncol=length(meas.names))
rownames(stat.tab) <- c(vols,"Total")
colnames(stat.tab) <- meas.names
stat.tabs <- list()
for(annotator in annotators)
	stat.tabs[[annotator]] <- stat.tab

# loop over volumes
tlog(2, "Looping over the re-annotated volumes")
for(v in 1:length(vols))
{	vol <- vols[v]
	tlog(4, "Processing volume ",vol," ",v,"/",length(vols))
	
	# select the appropriate part of the interaction table
	idx <- which(sapply(ref.lines,function(line) line[1])==vol)
	tlog(6, "Detected ",length(idx)," scenes in this volume (reference)")
	ref <- ref.lines[idx]
	
	# loop over annotators
	tlog(6, "Looping over annotators")
	for(annotator in annotators)
	{	# read annotator's file
		ann.file <- file.path(ANN_AGREEMENT_FOLDER, paste0(annotator,"_",vol,".txt"))
		tlog(8,"Reading annotator ",annotator,"'s file '",ann.file,"'")
		con <- file(ann.file, open="r")
		temp <- readLines(con)
		close(con)
		temp <- fix.encoding(strings=temp)
		est.lines <- strsplit(temp, split='\t', fixed=TRUE)
		tlog(8,"File read: ",length(est.lines)," lines)")
	
		# select lines
		idx <- which(sapply(est.lines,function(line) line[1])==vol)
		tlog(8, "Detected ",length(idx)," scenes in this volume (estimation)")
		est <- est.lines[idx]
		
		# loop over estimated scenes to compute TP and FP
		tlog(8, "Looping over estimated scenes")
		for(l in 1:length(est))
		{	tlog(10, "Processing scene ",l,"/",length(est))
			
			# match with the best reference scene (in terms of overlap)
			start.page <- as.integer(est[[l]][2])
			start.panel <- as.integer(est[[l]][3])
			start.panel.id <- as.integer(panel.stats[panel.stats[,COL_VOLUME]==vol & panel.stats[,COL_PAGE]==start.page & panel.stats[,COL_PANEL]==start.panel, COL_PANEL_ID])
			end.page <- as.integer(est[[l]][4])
			end.panel <- as.integer(est[[l]][5])
			end.panel.id <- as.integer(panel.stats[panel.stats[,COL_VOLUME]==vol & panel.stats[,COL_PAGE]==end.page & panel.stats[,COL_PANEL]==end.panel, COL_PANEL_ID])
			overlaps <- sapply(ref, function(line) 
					{	s.page <- as.integer(line[2])
						s.panel <- as.integer(line[3])
						s.panel.id <- as.integer(panel.stats[panel.stats[,COL_VOLUME]==vol & panel.stats[,COL_PAGE]==s.page & panel.stats[,COL_PANEL]==s.panel, COL_PANEL_ID])
						e.page <- as.integer(line[4])
						e.panel <- as.integer(line[5])
						e.panel.id <- as.integer(panel.stats[panel.stats[,COL_VOLUME]==vol & panel.stats[,COL_PAGE]==e.page & panel.stats[,COL_PANEL]==e.panel, COL_PANEL_ID])
						# est before ref
						if(start.panel.id<=s.panel.id)
							# ref inside est
							if(end.panel.id>=e.panel.id)
								res <- e.panel.id - s.panel.id + 1
							# ref not inside est
							else
								res <- end.panel.id - s.panel.id + 1
						# ref before est
						else
							# est inside ref
							if(e.panel.id>=end.panel.id)
								res <- end.panel.id - start.panel.id + 1
							# est not inside ref
							else
								res <- e.panel.id - start.panel.id + 1
						if(res<0)
							res <- 0
						return(res)
					})
			idx <- which.max(overlaps)
			tlog(12, "Position   : ",start.page,":",start.panel,"--",end.page,":",end.panel)
			tlog(12, "Best match : ",as.integer(ref[[idx]][2]),":",as.integer(ref[[idx]][3]),"--",as.integer(ref[[idx]][4]),":",as.integer(ref[[idx]][5]))
			
			# get characters
			if(length(est[[l]])>5)
				est.chars <- est[[l]][6:length(est[[l]])]
			else
				est.chars <- c()
			if(length(ref[[idx]])>5)
				ref.chars <- ref[[idx]][6:length(ref[[idx]])]
			else
				ref.chars <- c()
			
			# true positives
			tp.chars <- intersect(ref.chars, est.chars)
			tp <- length(tp.chars)
			tlog(12, "True positives: ",paste(tp.chars,collapse=", "))
			# overall
			stat.tab[vol,TP] <- stat.tab[vol,TP] + tp
			stat.tab["Total",TP] <- stat.tab["Total",TP] + tp
			# annotator-wise
			stat.tabs[[annotator]][vol,TP] <- stat.tabs[[annotator]][vol,TP] + tp
			stat.tabs[[annotator]]["Total",TP] <- stat.tabs[[annotator]]["Total",TP] + tp
			
			# false positives
			fp.chars <- setdiff(est.chars, ref.chars)
			fp <- length(fp.chars)
			tlog(12, "False positives: ",paste(fp.chars,collapse=", "))
			# overall
			stat.tab[vol,FP] <- stat.tab[vol,FP] + fp
			stat.tab["Total",FP] <- stat.tab["Total",FP] + fp
			# annotator-wise
			stat.tabs[[annotator]][vol,FP] <- stat.tabs[[annotator]][vol,FP] + fp
			stat.tabs[[annotator]]["Total",FP] <- stat.tabs[[annotator]]["Total",FP] + fp
		
			# false negatives
			fn.chars <- setdiff(ref.chars, est.chars)
			fn <- length(fn.chars)
			tlog(12, "False negatives: ",paste(fn.chars,collapse=", "))
			# overall
			stat.tab[vol,FN] <- stat.tab[vol,FN] + fn
			stat.tab["Total",FN] <- stat.tab["Total",FN] + fn
			# annotator-wise
			stat.tabs[[annotator]][vol,FN] <- stat.tabs[[annotator]][vol,FN] + fn
			stat.tabs[[annotator]]["Total",FN] <- stat.tabs[[annotator]]["Total",FN] + fn
		}
		
		# compute annotator-wise precision, recall, f-measure
		stat.tabs[[annotator]][vol,PRE] <- stat.tabs[[annotator]][vol,TP] / (stat.tabs[[annotator]][vol,TP] + stat.tabs[[annotator]][vol,FP])
		stat.tabs[[annotator]][vol,REC] <- stat.tabs[[annotator]][vol,TP] / (stat.tabs[[annotator]][vol,TP] + stat.tabs[[annotator]][vol,FN])
		stat.tabs[[annotator]][vol,FM]  <- 2 * stat.tabs[[annotator]][vol,PRE] * stat.tabs[[annotator]][vol,REC] / (stat.tabs[[annotator]][vol,PRE] + stat.tabs[[annotator]][vol,REC])
	}

	# compute overall precision, recall, and f-measure
	stat.tab[vol,PRE] <- stat.tab[vol,TP] / (stat.tab[vol,TP] + stat.tab[vol,FP])
	stat.tab[vol,REC] <- stat.tab[vol,TP] / (stat.tab[vol,TP] + stat.tab[vol,FN])
	stat.tab[vol,FM]  <- 2 * stat.tab[vol,PRE] * stat.tab[vol,REC] / (stat.tab[vol,PRE] + stat.tab[vol,REC]) 
}

# compute total precision, recall, and f-measure
tlog(2, "Computing total measures")
# overall values
stat.tab["Total",PRE] <- stat.tab["Total",TP] / (stat.tab["Total",TP] + stat.tab["Total",FP])
stat.tab["Total",REC] <- stat.tab["Total",TP] / (stat.tab["Total",TP] + stat.tab["Total",FN])
stat.tab["Total",FM]  <- 2 * stat.tab["Total",PRE] * stat.tab["Total",REC] / (stat.tab["Total",PRE] + stat.tab["Total",REC])
# annotator-wise values
for(annotator in annotators)
{	stat.tabs[[annotator]]["Total",PRE] <- stat.tabs[[annotator]]["Total",TP] / (stat.tabs[[annotator]]["Total",TP] + stat.tabs[[annotator]]["Total",FP])
	stat.tabs[[annotator]]["Total",REC] <- stat.tabs[[annotator]]["Total",TP] / (stat.tabs[[annotator]]["Total",TP] + stat.tabs[[annotator]]["Total",FN])
	stat.tabs[[annotator]]["Total",FM]  <- 2 * stat.tabs[[annotator]]["Total",PRE] * stat.tabs[[annotator]]["Total",REC] / (stat.tabs[[annotator]]["Total",PRE] + stat.tabs[[annotator]]["Total",REC])
}

tlog(2, "Measure matrices:")
print(stat.tabs)
print(stat.tab)




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
