# Generates an alternate version of the annotation file, meant to be
# completed manually.
# 
# Vincent Labatut
# 09/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("D:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/explicit/annotation_chars.R")
###############################################################################
source("src/common/_include.R")




###############################################################################
# Reads the original interaction file and produces a based version for the
# annotation of the explicit character occurrences. All the characters appearing
# in a scene are associated to each panel constituting this scene. The annotation
# is then conducted by removing the characters that are not actuall shown in the
# panels.
###############################################################################
init.annotation.file <- function()
{	# load series data
	data <- read.corpus.data()
	panel.stats <- data$panel.stats
	panel.chars <- data$panel.chars
	
	# keep only the relevant info
	data <- panel.stats[,c(COL_VOLUME, COL_PAGE, COL_PANEL, COL_PANEL_ID)]
	# add the character lists
	chars <- sapply(panel.chars, function(str) paste0(str,collapse=";"))
	data <- cbind(data, chars)
	colnames(data)[ncol(data)] <- COL_CHARS
	
	# record new table
	tab.file <- file.path(ANN_EXPLICIT_FOLDER, "chars_by_panel.csv")
	write.csv(x=data, file=tab.file, row.names=FALSE, fileEncoding="UTF-8")#, col.names=TRUE)
}




###############################################################################
 init.annotation.file()
