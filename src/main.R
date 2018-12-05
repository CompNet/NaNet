# TODO: Add comment
# 
# Vincent Labatut
# 11/2018
###############################################################################
library("igraph")

source("src/graph_extraction.R")





###############################################################################
# init file paths
FATA_FOLDER <- file.path("data","Ralph_Azham")
pages.file <- file.path(FATA_FOLDER,"pages.csv")
volumes.file <- file.path(FATA_FOLDER,"volumes.csv")
inter.file <- file.path(FATA_FOLDER,"interactions.txt")


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
		StartPanel=integer(), EndPanel=integer(), 
		StartPage=integer(), EndPage=integer(), 
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
			StartPanel=as.integer(rep(start.abs,nrow(chars))), EndPanel=as.integer(rep(end.abs,nrow(chars))),
			StartPage=as.integer(rep(start.page,nrow(chars))), EndPage=as.integer(rep(end.page,nrow(chars))),
			stringsAsFactors=FALSE)
	inter.df <- rbind(inter.df, df)
}


###############################################################################
# extract the segment-based static graph
g <- extract.static.graph.from.segments(inter.df)
#plot(g, layout=layout_with_fr(g))
g <- extract.static.graph(inter.df)

# TODO
# - define function for graph extraction
#   - distinguish segment- and page-based
#     >> must add pages to inter.df 
#   - also window-based ?
# - extract one graph for each volume? cycle?
# - extract dynamic nets
# - define a specfic function for the conversion process
