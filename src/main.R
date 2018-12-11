# TODO: Add comment
# 
# Vincent Labatut
# 11/2018
###############################################################################
library("igraph")

source("src/graph_extraction.R")





###############################################################################
# init file paths
DATA_FOLDER <- file.path("data","Test")
#DATA_FOLDER <- file.path("data","Ralph_Azham")
OUT_FOLDER <- file.path(DATA_FOLDER,"networks")
pages.file <- file.path(DATA_FOLDER,"pages.csv")
volumes.file <- file.path(DATA_FOLDER,"volumes.csv")
inter.file <- file.path(DATA_FOLDER,"interactions.txt")


###############################################################################
# read the file describing the volumes
volumes.info <- read.csv(pages.file, header=TRUE, check.names=FALSE)


# read the file describing the pages
pages.info <- read.csv(pages.file, header=TRUE, check.names=FALSE)
Start <- cumsum(c(1,pages.info[,2]))	# get the number of the panel starting each page since the beginning
Start <- Start[1:(length(Start)-1)]
pages.info <- cbind(pages.info,Start)


# read the file describing the interactions
con <- file(inter.file, open="r")
temp <- readLines(con)
close(con)
lines <- strsplit(temp, split='\t', fixed=TRUE)


###############################################################################
## get the list of all characters
#all.chars <- c()
#for(line in lines)
#{	chars <- line[3:length(line)]
#	all.chars <- union(all.chars,chars)
#}
#all.chars <- sort(all.chars)


# extract the edge list
inter.df <- data.frame(
		From=character(), To=character(), 
		Start=integer(), End=integer(), 
		stringsAsFactors=FALSE)
Encoding(inter.df$From) <- "UTF-8"
Encoding(inter.df$To) <- "UTF-8"
for(line in lines)
{	# get segment bounds
	start <- strsplit(line[1], split='.', fixed=TRUE)[[1]]
	start.page <- as.integer(start[1])
	start.panel <- as.integer(start[2])
	start.abs <- pages.info[start.page,"Start"] + start.panel - 1
	end <- strsplit(line[2], split='.', fixed=TRUE)[[1]]
	end.page <- as.integer(end[1])
	end.panel <- as.integer(end[2])
	end.abs <- pages.info[end.page,"Start"] + end.panel - 1
	# compute segment length (in pages)
	page.length <- end.page - start.page + 1
	# get all combinations of characters
	chars <- line[3:length(line)]
	chars <- gsub("[()]", "", chars)	# remove parenthesis (representing ghost characters)
	chars <- sort(chars[which(chars!="" & chars!=" ")])
	chars <- t(combn(x=chars,m=2))
	# add segment to data frame
	df <- data.frame(From=(chars[,1]), To=chars[,2], 
			Start=as.integer(rep(start.abs,nrow(chars))), End=as.integer(rep(end.abs,nrow(chars))),
			stringsAsFactors=FALSE)
	inter.df <- rbind(inter.df, df)
}


###############################################################################
# extract the segment-based static graph
g <- extract.static.graph.from.segments(inter.df)
#plot(g, layout=layout_with_fr(g))
g <- extract.static.graph.from.panel.window(inter.df, window.size=10, overlap=2)
g <- extract.static.graph.from.page.window(inter.df, pages.info, window.size=2, overlap=1)

# TODO
# - define function for graph extraction
#   >> Test that on a small toy dataset
# - extract one graph for each volume? cycle?
# - extract dynamic nets
# - define a specfic function for the conversion process
