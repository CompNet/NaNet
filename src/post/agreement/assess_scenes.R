# This script assesses the agreement over scene boundaries.
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

# Methods automatically detecting scene boundaries are assessed using a number of measures in the literature.
# We could use a more lenient measure, but we selected precision/recall/f-measure, which consider that
# a scene boundary is either correct or not. We do not allow any temporal tolerance.

# read the table from the main corpus
tlog(2,"Reading the interaction file '",INTER_FILE,"'")
con <- file(INTER_FILE, open="r")
temp <- readLines(con)
close(con)
temp <- fix.encoding(strings=temp)
ref.lines <- strsplit(temp, split='\t', fixed=TRUE)
tlog(4,"File read: ",length(ref.lines)," lines)")

# volumes processed by the additional annotaters
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
		tlog(6, "Detected ",length(idx)," scenes in this volume (estimation)")
		est <- est.lines[idx]
		
		# loop over estimated boundaries to compute TP and FP
		for(l in 2:length(est))	# ignore first and last boundaries (=start/end of book)
		{	start.page <- as.integer(est[[l]][2])
			start.panel <- as.integer(est[[l]][3])
			if(any(sapply(ref,function(line) line[2]==start.page && line[3]==start.panel)))
			{	# overall
				stat.tab[vol,TP] <- stat.tab[vol,TP] + 1
				stat.tab["Total",TP] <- stat.tab["Total",TP] + 1
				# annotator-wise
				stat.tabs[[annotator]][vol,TP] <- stat.tabs[[annotator]][vol,TP] + 1
				stat.tabs[[annotator]]["Total",TP] <- stat.tabs[[annotator]]["Total",TP] + 1
			}
			else
			{	# overall
				stat.tab[vol,FP] <- stat.tab[vol,FP] + 1
				stat.tab["Total",FP] <- stat.tab["Total",FP] + 1
				# annotator-wise
				stat.tabs[[annotator]][vol,FP] <- stat.tabs[[annotator]][vol,FP] + 1
				stat.tabs[[annotator]]["Total",FP] <- stat.tabs[[annotator]]["Total",FP] + 1
			}
		}
		
		# loop over reference boundaries to compute FN
		for(l in 2:length(ref))	# as before, ignore first and last boundaries, bc no choice there
		{	start.page <- as.integer(ref[[l]][2])
			start.panel <- as.integer(ref[[l]][3])
			if(!any(sapply(est,function(line) line[2]==start.page && line[3]==start.panel)))
			{	# overall
				stat.tab[vol,FN] <- stat.tab[vol,FN] + 1
				stat.tab["Total",FN] <- stat.tab["Total",FN] + 1
				# annotator-wise
				stat.tabs[[annotator]][vol,FN] <- stat.tabs[[annotator]][vol,FN] + 1
				stat.tabs[[annotator]]["Total",FN] <- stat.tabs[[annotator]]["Total",FN] + 1
			}
		}
		
		# compute annotator-wise precision, recall, f-measure
		stat.tabs[[annotator]][vol,PRE] <- stat.tabs[[annotator]][vol,TP] / (stat.tabs[[annotator]][vol,TP] + stat.tabs[[annotator]][vol,FP])
		stat.tabs[[annotator]][vol,REC] <- stat.tabs[[annotator]][vol,TP] / (stat.tabs[[annotator]][vol,TP] + stat.tabs[[annotator]][vol,FN])
		stat.tabs[[annotator]][vol,FM] <- 2 * stat.tabs[[annotator]][vol,PRE] * stat.tabs[[annotator]][vol,REC] / (stat.tabs[[annotator]][vol,PRE] + stat.tabs[[annotator]][vol,REC])
	}

	# compute overall precision, recall, and f-measure
	stat.tab[vol,PRE] <- stat.tab[vol,TP] / (stat.tab[vol,TP] + stat.tab[vol,FP])
	stat.tab[vol,REC] <- stat.tab[vol,TP] / (stat.tab[vol,TP] + stat.tab[vol,FN])
	stat.tab[vol,FM] <- 2 * stat.tab[vol,PRE] * stat.tab[vol,REC] / (stat.tab[vol,PRE] + stat.tab[vol,REC])
}

# compute total precision, recall, and f-measure
# overall values
stat.tab["Total",PRE] <- stat.tab["Total",TP] / (stat.tab["Total",TP] + stat.tab["Total",FP])
stat.tab["Total",REC] <- stat.tab["Total",TP] / (stat.tab["Total",TP] + stat.tab["Total",FN])
stat.tab["Total",FM] <- 2 * stat.tab["Total",PRE] * stat.tab["Total",REC] / (stat.tab["Total",PRE] + stat.tab["Total",REC])
# annotator-wise values
for(annotator in annotators)
{	stat.tabs[[annotator]]["Total",PRE] <- stat.tabs[[annotator]]["Total",TP] / (stat.tabs[[annotator]]["Total",TP] + stat.tabs[[annotator]]["Total",FP])
	stat.tabs[[annotator]]["Total",REC] <- stat.tabs[[annotator]]["Total",TP] / (stat.tabs[[annotator]]["Total",TP] + stat.tabs[[annotator]]["Total",FN])
	stat.tabs[[annotator]]["Total",FM] <- 2 * stat.tabs[[annotator]]["Total",PRE] * stat.tabs[[annotator]]["Total",REC] / (stat.tabs[[annotator]]["Total",PRE] + stat.tabs[[annotator]]["Total",REC])
}

print(stat.tabs)
print(stat.tab)




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
