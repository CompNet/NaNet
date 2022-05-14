# This script assesses the agreement over scene boundaries.
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
vols <- c("06","16","20") #,"K1")
# annotators' names
annotators <- c("arthur","noe") #,"elise")

# evaluation measures
meas.names <- c("TruePositives","FalsePositives","FalseNegatives","Precision","Recall","Fmeasure")

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
				stat.tab[vol,"TruePositives"] <- stat.tab[vol,"TruePositives"] + 1
				stat.tab["Total","TruePositives"] <- stat.tab["Total","TruePositives"] + 1
				# annotator-wise
				stat.tabs[[annotator]][vol,"TruePositives"] <- stat.tabs[[annotator]][vol,"TruePositives"] + 1
				stat.tabs[[annotator]]["Total","TruePositives"] <- stat.tabs[[annotator]]["Total","TruePositives"] + 1
			}
			else
			{	# overall
				stat.tab[vol,"FalsePositives"] <- stat.tab[vol,"FalsePositives"] + 1
				stat.tab["Total","FalsePositives"] <- stat.tab["Total","FalsePositives"] + 1
				# annotator-wise
				stat.tabs[[annotator]][vol,"FalsePositives"] <- stat.tabs[[annotator]][vol,"FalsePositives"] + 1
				stat.tabs[[annotator]]["Total","FalsePositives"] <- stat.tabs[[annotator]]["Total","FalsePositives"] + 1
			}
		}
		
		# loop over reference boundaries to compute FN
		for(l in 2:length(ref))	# as before, ignore first and last boundaries, bc no choice there
		{	start.page <- as.integer(ref[[l]][2])
			start.panel <- as.integer(ref[[l]][3])
			if(!any(sapply(est,function(line) line[2]==start.page && line[3]==start.panel)))
			{	# overall
				stat.tab[vol,"FalseNegatives"] <- stat.tab[vol,"FalseNegatives"] + 1
				stat.tab["Total","FalseNegatives"] <- stat.tab["Total","FalseNegatives"] + 1
				# annotator-wise
				stat.tabs[[annotator]][vol,"FalseNegatives"] <- stat.tabs[[annotator]][vol,"FalseNegatives"] + 1
				stat.tabs[[annotator]]["Total","FalseNegatives"] <- stat.tabs[[annotator]]["Total","FalseNegatives"] + 1
			}
		}
		
		# compute annotator-wise precision, recall, f-measure
		stat.tabs[[annotator]][vol,"Precision"] <- stat.tabs[[annotator]][vol,"TruePositives"] / (stat.tabs[[annotator]][vol,"TruePositives"] + stat.tabs[[annotator]][vol,"FalsePositives"])
		stat.tabs[[annotator]][vol,"Recall"] <- stat.tabs[[annotator]][vol,"TruePositives"] / (stat.tabs[[annotator]][vol,"TruePositives"] + stat.tabs[[annotator]][vol,"FalseNegatives"])
		stat.tabs[[annotator]][vol,"Fmeasure"] <- 2 * stat.tabs[[annotator]][vol,"Precision"] * stat.tabs[[annotator]][vol,"Recall"] / (stat.tabs[[annotator]][vol,"Precision"] + stat.tabs[[annotator]][vol,"Recall"])
	}

	# compute overall precision, recall, and f-measure
	stat.tab[vol,"Precision"] <- stat.tab[vol,"TruePositives"] / (stat.tab[vol,"TruePositives"] + stat.tab[vol,"FalsePositives"])
	stat.tab[vol,"Recall"] <- stat.tab[vol,"TruePositives"] / (stat.tab[vol,"TruePositives"] + stat.tab[vol,"FalseNegatives"])
	stat.tab[vol,"Fmeasure"] <- 2 * stat.tab[vol,"Precision"] * stat.tab[vol,"Recall"] / (stat.tab[vol,"Precision"] + stat.tab[vol,"Recall"])
}

# compute total precision, recall, and f-measure
# overall values
stat.tab["Total","Precision"] <- stat.tab["Total","TruePositives"] / (stat.tab["Total","TruePositives"] + stat.tab["Total","FalsePositives"])
stat.tab["Total","Recall"] <- stat.tab["Total","TruePositives"] / (stat.tab["Total","TruePositives"] + stat.tab["Total","FalseNegatives"])
stat.tab["Total","Fmeasure"] <- 2 * stat.tab["Total","Precision"] * stat.tab["Total","Recall"] / (stat.tab["Total","Precision"] + stat.tab["Total","Recall"])
# annotator-wise values
for(annotator in annotators)
{	stat.tabs[[annotator]]["Total","Precision"] <- stat.tabs[[annotator]]["Total","TruePositives"] / (stat.tabs[[annotator]]["Total","TruePositives"] + stat.tabs[[annotator]]["Total","FalsePositives"])
	stat.tabs[[annotator]]["Total","Recall"] <- stat.tabs[[annotator]]["Total","TruePositives"] / (stat.tabs[[annotator]]["Total","TruePositives"] + stat.tabs[[annotator]]["Total","FalseNegatives"])
	stat.tabs[[annotator]]["Total","Fmeasure"] <- 2 * stat.tabs[[annotator]]["Total","Precision"] * stat.tabs[[annotator]]["Total","Recall"] / (stat.tabs[[annotator]]["Total","Precision"] + stat.tabs[[annotator]]["Total","Recall"])
}

print(stat.tabs)
print(stat.tab)




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
