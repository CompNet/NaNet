# Additional processing.
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
	freq.tab[c, ] <- str_count(lines, fixed(name))
}
