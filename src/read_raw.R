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
###############################################################################
read.volume.table <- function(pages.info)
{	tlog(2,"Trying to read the volume file (",VOLUMES_FILE,")")
	
	# read the proper table
	volumes.info <- read.csv(VOLUMES_FILE, header=TRUE, check.names=FALSE)
	
	# nothing to do here, for now
	
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
#			- COL_PAGES_VOLUME_ID: absolute number of this volume.
#			- COL_PAGES_PAGE: page in the above volume.
#			- COL_PAGES_PANELS: number of panels in this page.
#			- COL_PAGES_START_PANEL_ID: absolute number of the page's first panel in the whole series.
###############################################################################
read.page.table <- function(volumes.info)
{	tlog(2,"Trying to read the page file (",PAGES_FILE,")")
	
	# read the proper table
	pages.info <- read.csv(PAGES_FILE, header=TRUE, check.names=FALSE)
	
	# check that each relative page number matches the interval defined in the volume table 
	vol.ids <- match(pages.info[,COL_PAGES_VOLUME],volumes.info[,COL_VOLS_VOLUME])
	err.pg.idx <- which(pages.info[,COL_PAGES_PAGE]<volumes.info[vol.ids,COL_VOLS_START_PAGE]
		| pages.info[,COL_PAGES_PAGE]>volumes.info[vol.ids,COL_VOLS_END_PAGE])
	if(length(err.pg.idx)>0)
	{	msg <- apply(as.matrix(pages.info[err.pg.idx,]),1, function(r) paste(r,collapse=","))
		msg <- apply(cbind(err.pg.idx,msg),1, function(r) paste(r,collapse=": "))
		msg <- c(paste(colnames(pages.info),collapse=","),msg)
		msg <- paste(msg,collapse="\n")
		stop(paste0("ERROR while reading file \"",PAGES_FILE,"\". The following pages are out of bounds, compared to the volume information:\n",msg))
	}
	
	# get the number of the panel starting each page since the beginning
	start.panel.ids <- cumsum(c(1,pages.info[,COL_PAGES_PANELS]))
	start.panel.ids <- start.panel.ids[1:(length(start.panel.ids)-1)]
	# add this as a new column
	pages.info <- cbind(pages.info,start.panel.ids)
	colnames(pages.info)[ncol(pages.info)] <- COL_PAGES_START_PANEL_ID
	
	# also add the volume id (ie absolute number over the whole series)
	pages.info <- cbind(pages.info,vol.ids)
	colnames(pages.info)[ncol(pages.info)] <- COL_PAGES_VOLUME_ID
	
	tlog(2,"Reading of the page file completed")
	return(pages.info)	
}	


	
###############################################################################
# Reads the table describing the interactions between characters, and coverts
# them into an edge list while performing some verifications.
#
# volumes.info: table describing the series volumes.
# pages.info: table describing all the pages constituting the BD series.
# 
# returns: a dataframe listing the interactions, with the following columns:
#			- COL_INTER_FROM: first character concerned by the interaction.
#			- COL_INTER_TO: second character concerned by the interaction.
#			- COL_INTER_START: absolute id of the panel where the interaction starts.
#			- COL_INTER_END: absolute id of the panel where the interaction ends.
###############################################################################
read.inter.table <- function(volume.info, pages.info)
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
	cn <- c(COL_INTER_FROM_CHAR, COL_INTER_TO_CHAR, COL_INTER_START_PANEL_ID, COL_INTER_END_PANEL_ID)
	colnames(inter.df) <- cn
	for(line in lines)
	{	# get volume
		volume <- line[1]
		volume.id <- which(volumes.info[,COL_VOLS_VOLUME]==volume)
		
		# get start page and panel
		start.page <- line[2]
		start.page.id <- which(pages.info[,COL_PAGES_PAGE]==start.page & pages.info[,COL_PAGES_VOLUME_ID]==volume.id)
		if(length(start.page.id)==0)
			stop(paste0("ERROR while reading file \"",INTER_FILE,"\". Starting page not found in line:\"",paste(line,collapse=","),"\""))
		start.panel <-  as.integer(line[3])
		if(start.panel>pages.info[start.page.id,COL_PAGES_PANELS])
			stop(paste0("ERROR while reading file \"",INTER_FILE,"\". Starting panel is out of page in line:\"",paste(line,collapse=","),"\""))
		start.panel.id <- pages.info[start.page.id,COL_PAGES_START_PANEL_ID] + start.panel - 1
		
		# get end page and panel
		end.page <- line[4]
		end.page.id <- which(pages.info[,COL_PAGES_PAGE]==end.page & pages.info[,COL_PAGES_VOLUME_ID]==volume.id)
		if(length(end.page.id)==0)
			stop(paste0("ERROR while reading file \"",INTER_FILE,"\". Ending page not found in line:\"",paste(line,collapse=","),"\""))
		end.panel <- as.integer(line[5])
		if(end.panel>pages.info[end.page.id,COL_PAGES_PANELS])
			stop(paste0("ERROR while reading file \"",INTER_FILE,"\". Ending panel is out of page in line:\"",paste(line,collapse=","),"\""))
		end.panel.id <- pages.info[end.page.id,COL_PAGES_START_PANEL_ID] + end.panel - 1
		
		# check that the end is after the start
		if(start.panel.id>end.panel.id)
			stop(paste0("ERROR while reading file \"",INTER_FILE,"\". Starting panel located after ending panel in line:\"",paste(line,collapse=","),"\""))
		
		# compute segment length (in pages)
		page.length <- end.page.id - start.page.id + 1
		
		# get all combinations of characters
		chars <- line[6:length(line)]
		chars <- gsub("[()]", "", chars)	# remove parentheses (representing ghost characters)
		chars <- sort(chars[which(chars!="" & chars!=" ")])
		chars <- t(combn(x=chars,m=2))
		
		# add segment to data frame
		tmp.df <- data.frame(From=(chars[,1]), To=chars[,2], 
				Start=as.integer(rep(start.panel.id,nrow(chars))), End=as.integer(rep(end.panel.id,nrow(chars))),
				stringsAsFactors=FALSE)
		colnames(tmp.df) <- cn
		inter.df <- rbind(inter.df, tmp.df)
	}
	
	tlog(2,"Conversion of the interaction raw data completed")
	return(inter.df)
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
	inter.df <- read.inter.table(volume.info, pages.info)
	
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
