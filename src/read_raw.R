# This script contains functions used to read the raw data.
# 
# Vincent Labatut
# 01/2019
###############################################################################




###############################################################################
# Reads the table describing the volumes of the BD series: starting page, ending
# page, etc.
#
# returns: a dataframe listing the volume information, with the following columns:
#			- COL_VOLS_VOLUME: volume in the BD series.
#			- COL_VOLS_START_PAGE: starting page relatively to the volume.
#			- COL_VOLS_END_PAGE: ending page relatively to the volume.
#			- COL_VOLS_LENGTH: number of (effective) pages in the volume.
#			- COL_VOLS_TITLE: title of the volume.
#			- COL_VOLS_VOLUME_ID: absolute number of the volume in the whole series.
###############################################################################
read.volume.table <- function(pages.info)
{	tlog(2,"Trying to read the volume file (",VOLUMES_FILE,")")
	
	# read the proper table
	volumes.info <- read.csv(VOLUMES_FILE, header=TRUE, check.names=FALSE)
	
	# renumber volumes consecutively
	vol.ids <- 1:nrow(volumes.info)
	volumes.info <- cbind(volumes.info, vol.ids)
	colnames(volumes.info)[ncol(volumes.info)] <- COL_VOLS_VOLUME_ID
	
	tlog(2,"Reading of the volume file completed")
	return(volumes.info)
}




###############################################################################
# Reads the table describing the pages constituting the BD series,
# performs some verifications and add some columns required for subsequent
# processing.
#
# volumes.info: table describing the series volumes.
# 
# returns: a dataframe listing the pages information, with the following columns:
#			- COL_PAGES_VOLUME: volume in the BD series.
#			- COL_PAGES_PAGE: page in the above volume.
#			- COL_PAGES_PANELS: number of panels in this page.
#			- COL_PAGES_PAGE_ID: absolute number of the page in the whole series.
#			- COL_PAGES_START_PANEL_ID: absolute number of the page's first panel in the whole series.
###############################################################################
read.page.table <- function(volumes.info)
{	tlog(2,"Trying to read the page file (",PAGES_FILE,")")
	
	# read the proper table
	pages.info <- read.csv(PAGES_FILE, header=TRUE, check.names=FALSE)
	
	# check that each relative page number matches the interval defined in the volume table 
	vol.idx <- match(pages.info[,COL_PAGES_VOLUME],volumes.info[,COL_VOLS_VOLUME])
	pg.idx <- which(pages.info[,COL_PAGES_PAGE]<volumes.info[vol.idx,COL_VOLS_START_PAGE]
		| pages.info[,COL_PAGES_PAGE]>volumes.info[vol.idx,COL_VOLS_END_PAGE])
	if(length(pg.idx)>0)
	{	msg <- apply(as.matrix(pages.info[pg.idx,]),1, function(r) paste(r,collapse=","))
		msg <- apply(cbind(pg.idx,msg),1, function(r) paste(r,collapse=": "))
		msg <- c(paste(colnames(pages.info),collapse=","),msg)
		msg <- paste(msg,collapse="\n")
		stop(paste0("The following pages are out of bounds, compared to the volume information:\n",msg))
	}
	
	# renumber pages consecutively
	page.ids <- 1:nrow(pages.info)
	pages.info <- cbind(pages.info,page.ids)
	colnames(pages.info)[ncol(pages.info)] <- COL_PAGES_PAGE_ID
	
	# get the number of the panel starting each page since the beginning
	start.panel.ids <- cumsum(c(1,pages.info[,COL_PAGES_PANELS]))
	start.panel.ids <- start.panel.ids[1:(length(start.panel.ids)-1)]
	# add this as a new column
	pages.info <- cbind(pages.info,start.panel.ids)
	colnames(pages.info)[ncol(pages.info)] <- COL_PAGES_START_PANEL_ID
	
	tlog(2,"Reading of the page file completed")
	return(pages.info)	
}	


	
###############################################################################
# Reads the table describing the interactions between characters, and coverts
# them into an edge list while performing some verifications.
#
# pages.info: table describing all the pages constituting the BD series.
# 
# returns: a dataframe listing the interactions, with the following columns:
#			- 
###############################################################################
read.inter.table <- function(pages.info)
{	# read the proper table
	tlog(2,"Trying to read the interaction file (",INTER_FILE,")")
	con <- file(INTER_FILE, open="r")
	temp <- readLines(con)
	close(con)
	lines <- strsplit(temp, split='\t', fixed=TRUE)
	tlog(2,"Reading of the interaction file completed")
	
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
	volumes.info <- read.volume.table()
	# read the file describing the pages
	pages.info <- read.page.table(volumes.info)
	# read the file describing the interactions
	
	## get the list of all characters
	#all.chars <- c()
	#for(line in lines)
	#{	chars <- line[3:length(line)]
	#	all.chars <- union(all.chars,chars)
	#}
	#all.chars <- sort(all.chars)
	
	
	# build result and return
	result <- list (pages.info=pages.info, volumes.info=volumes.info, inter.df=inter.df)
	return(result)
}
