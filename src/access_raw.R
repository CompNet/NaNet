# This script contains functions used to read/write raw data.
# 
# Vincent Labatut
# 01/2019
###############################################################################




###############################################################################
# Reads the table describing the pages and their lengths expressed in panels.
#
# returns: a dataframe listing the pages and their length, containing the following
#		   columns:
#			- Volume: volume in the BD series.
#			- Page: page in the volume.
#			- Panels: number of panels in the page.
#			- Start: absolute number of the first panel in the page
###############################################################################
read.page.table <- function()
{	tlog(2,"Trying to read the page file (",PAGES_FILE,")")
	
	# read the proper table
	pages.info <- read.csv(PAGES_FILE, header=TRUE, check.names=FALSE)
	
	# renumber pages consecutively
	page.ids <- 1:nrow(pages.info)
	pages.info <- cbind(pages.info,page.ids)
	colnames(pages.info)[ncol(pages.info)] <- COL_PAGES_PAGE_ID
	
	# get the number of the panel starting each page since the beginning
	start.panel.ids <- cumsum(c(1,pages.info[,COL_PAGES_PANELS]))
	start.panel.ids <- start.panel.ids[1:(length(Start)-1)]
	# add this a new column
	pages.info <- cbind(pages.info,start.panel.ids)
	colnames(pages.info)[ncol(pages.info)] <- COL_PAGES_START_PANEL_ID
	
	tlog(2,"Reading of the page file completed")
	return(pages.info)	
}	


	
###############################################################################
###############################################################################
write.volume.table <- function(pages.info)
{	# TODO
	
}
	



###############################################################################
# Reads the raw data contained in several tables, and returns them under the
# form of data frames.
#
# returns: a list of 3 dataframes, volumes.info (information related to the
#          volumes), pages.info (information related to the pages), inter.df
#          (interactions between the characters).
###############################################################################
read.raw.data <- function()
{	tlog(1,"Reading data files")
	
	# read the file describing the volumes
	tlog(2,"Trying to read the volume file (",INTER_FILE,")")
	volumes.info <- read.csv(VOLUMES_FILE, header=TRUE, check.names=FALSE)
	tlog(2,"Reading of the volume file completed")
	
	# read the file describing the pages
	pages.info <- read.page.table()
	
	# read the file describing the interactions
	tlog(2,"Trying to read the interaction file (",INTER_FILE,")")
	con <- file(INTER_FILE, open="r")
	temp <- readLines(con)
	close(con)
	lines <- strsplit(temp, split='\t', fixed=TRUE)
	tlog(2,"Reading of the interaction file completed")
	
	## get the list of all characters
	#all.chars <- c()
	#for(line in lines)
	#{	chars <- line[3:length(line)]
	#	all.chars <- union(all.chars,chars)
	#}
	#all.chars <- sort(all.chars)
	
	# extract the edge list
	tlog(2,"Converting interactions to dataframe")
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
		if(is.na(start.abs))
			stop(paste0("Problem with line:\"",paste(line,collapse=","),"\""))
		end <- strsplit(line[2], split='.', fixed=TRUE)[[1]]
		end.page <- as.integer(end[1])
		end.panel <- as.integer(end[2])
		end.abs <- pages.info[end.page,"Start"] + end.panel - 1
		if(is.na(end.abs))
			stop(paste0("Problem with line:\"",paste(line,collapse=","),"\""))
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
	tlog(2,"Conversion of the interaction raw data completed")
	
	# build result and return
	result <- list (pages.info=pages.info, volumes.info=volumes.info, inter.df=inter.df)
	return(result)
}
