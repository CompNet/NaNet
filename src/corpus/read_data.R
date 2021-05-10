# This script contains functions used to read the raw data and build a matrix
# representing character interactions.
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
#			- COL_VOLS_PAGE_START: starting page relatively to the volume.
#			- COL_VOLS_PAGE_END: ending page relatively to the volume.
#			- COL_VOLS_LENGTH: number of (effective) pages in the volume.
#			- COL_VOLS_TITLE: title of the volume.
###############################################################################
read.volume.table <- function(page.info)
{	tlog(2,"Trying to read the volume file (",VOLUME_FILE,")")
	
	# read the proper table
	volume.info <- read.csv(VOLUME_FILE, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	
	# add volume id
	volume.info <- cbind(1:nrow(volume.info), volume.info)
	colnames(volume.info)[1] <- COL_VOLS_VOLUME_ID
	
	tlog(2,"Reading of the volume file completed")
	return(volume.info)
}




###############################################################################
# Reads the table describing the pages constituting the BD series,
# performs some verifications and add some columns required for subsequent
# processing.
#
# volume.info: table describing the series volumes.
# 
# returns: a dataframe listing the pages information, with the following columns:
#			- COL_PAGES_VOLUME: volume in the BD series.
#			- COL_PAGES_VOLUME_ID: absolute number of this volume.
#			- COL_PAGES_PAGE: page in the above volume.
#			- COL_PAGES_PANELS: number of panels in this page.
#			- COL_PAGES_START_PANEL_ID: absolute number of the page's first panel in the whole series.
###############################################################################
read.page.table <- function(volume.info)
{	tlog(2,"Trying to read the page file (",PAGE_FILE,")")
	
	# read the proper table
	page.info <- read.csv(PAGE_FILE, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	
	# check that each relative page number matches the interval defined in the volume table 
	vol.ids <- match(page.info[,COL_PAGES_VOLUME],volume.info[,COL_VOLS_VOLUME])
	err.pg.idx <- which(page.info[,COL_PAGES_PAGE]<volume.info[vol.ids,COL_VOLS_PAGE_START]
		| page.info[,COL_PAGES_PAGE]>volume.info[vol.ids,COL_VOLS_PAGE_END])
	if(length(err.pg.idx)>0)
	{	tmp.msg <- apply(as.matrix(page.info[err.pg.idx,]),1, function(r) paste(r,collapse=","))
		tmp.msg <- apply(cbind(err.pg.idx,tmp.msg),1, function(r) paste(r,collapse=": "))
		tmp.msg <- c(paste(colnames(page.info),collapse=","),tmp.msg)
		tmp.msg <- paste(tmp.msg,collapse="\n")
		msg <- paste0("ERROR while reading file \"",PAGE_FILE,"\". The following pages are out of bounds, compared to the volume information:\n",tmp.msg)
		tlog(3,msg)
		stop(msg)
	}
	
	# get the number of the panel starting each page since the beginning
	start.panel.ids <- cumsum(c(1,page.info[,COL_PAGES_PANELS]))
	start.panel.ids <- start.panel.ids[1:(length(start.panel.ids)-1)]
	# add this as a new column
	page.info <- cbind(page.info,start.panel.ids)
	colnames(page.info)[ncol(page.info)] <- COL_PAGES_START_PANEL_ID
	
	# also add the volume id (i.e. absolute number over the whole series)
	page.info <- cbind(page.info,vol.ids)
	colnames(page.info)[ncol(page.info)] <- COL_PAGES_VOLUME_ID
	
	tlog(2,"Reading of the page file completed")
	return(page.info)	
}	


	
###############################################################################
# Reads the table describing the interactions between characters, and coverts
# them into an edge list while performing some verifications.
#
# volume.info: table describing the series volumes.
# page.info: table describing all the pages constituting the BD series.
# 
# returns: a dataframe listing the interactions, with the following columns:
#			- COL_INTER_FROM_CHAR: first character concerned by the interaction.
#			- COL_INTER_TO_CHAR: second character concerned by the interaction.
#			- COL_INTER_START_PANEL_ID: absolute id of the panel where the interaction starts.
#			- COL_INTER_END_PANEL_ID: absolute id of the panel where the interaction ends.
###############################################################################
read.inter.table <- function(volume.info, page.info)
{	char.scenes <- list()
	
	# init stats table for scenes
	stats.scenes <- data.frame(
			character(), integer(),
			integer(), integer(),
			integer(), integer(), 
			integer(), integer(),
			integer(), integer(), 
			integer(), integer(), integer(),
			logical(), logical(), logical(),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(stats.scenes) <- c(
			COL_STATS_VOLUME, COL_STATS_VOLUME_ID,
			COL_STATS_START_PAGE, COL_STATS_START_PAGE_ID, 
			COL_STATS_START_PANEL, COL_STATS_START_PANEL_ID,
			COL_STATS_END_PAGE, COL_STATS_END_PAGE_ID, 
			COL_STATS_END_PANEL, COL_STATS_END_PANEL_ID, 
			COL_STATS_PANELS, COL_STATS_PAGES, COL_STATS_CHARS,
			COL_STATS_MATCH_START, COL_STATS_MATCH_END, COL_STATS_MATCH_BOTH
	)
	
	# read the proper table
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
	prev.end.panel.id <- NA
	for(l in 2:length(lines))
	{	line <- lines[[l]]
		# get volume
		volume <- line[1]
		volume.id <- which(volume.info[,COL_VOLS_VOLUME]==volume)
		
		# get start page and panel
		start.page <- line[2]
		start.page.id <- which(page.info[,COL_PAGES_PAGE]==start.page & page.info[,COL_PAGES_VOLUME_ID]==volume.id)
		if(length(start.page.id)==0)
		{	msg <- paste0("ERROR while reading file \"",INTER_FILE,"\". Starting page not found in line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			stop(msg)
		}
		start.panel <-  as.integer(line[3])
		if(start.panel>page.info[start.page.id,COL_PAGES_PANELS])
		{	msg <- paste0("ERROR while reading file \"",INTER_FILE,"\". Starting panel is out of page in line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			stop(msg)
		}
		start.panel.id <- page.info[start.page.id,COL_PAGES_START_PANEL_ID] + start.panel - 1
		if(!is.na(prev.end.panel.id) & start.panel.id>(prev.end.panel.id+1))
		{	msg <- paste0("WARNING while reading file \"",INTER_FILE,"\". Missing panel(s) between this scene and the previous one, at line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			#warning(msg)
		}
		
		# get end page and panel
		end.page <- line[4]
		end.page.id <- which(page.info[,COL_PAGES_PAGE]==end.page & page.info[,COL_PAGES_VOLUME_ID]==volume.id)
		if(length(end.page.id)==0)
		{	msg <- paste0("ERROR while reading file \"",INTER_FILE,"\". Ending page not found in line: \"",paste(line,collapse=","),"\"") 
			tlog(3,msg)
			stop(msg)
		}
		end.panel <- as.integer(line[5])
		if(end.panel>page.info[end.page.id,COL_PAGES_PANELS])
		{	msg <- paste0("ERROR while reading file \"",INTER_FILE,"\". Ending panel is out of page in line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			stop(msg)
		}
		end.panel.id <- page.info[end.page.id,COL_PAGES_START_PANEL_ID] + end.panel - 1
		prev.end.panel.id <- end.panel.id
		
		# check that the end is after the start
		if(start.panel.id>end.panel.id)
		{	msg <- paste0("ERROR while reading file \"",INTER_FILE,"\". Starting panel located after ending panel in line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			stop(msg)
		}

		# get all combinations of characters
		if(length(line)<6)
			chars <- c()
		else
		{	chars <- line[6:length(line)]
			chars <- gsub("[()]", "", chars)	# remove parentheses (representing ghost characters)
			chars <- sapply(strsplit(chars,"/"), function(v)	# remove "/" corresponding to disguises
					{	if(length(v)==0)
							return("")
						else if(length(v)==1)
							return(trimws(v))
						else if(length(v)==2)
							return(trimws(v[2]))
						else
						{	msg <- paste0("ERROR when splitting the names: ",v)
							tlog(4,msg)
							stop(msg)
						}
					})
			chars <- sort(chars[which(chars!="" & chars!=" ")])
			if(length(chars)<=1)
			{	if(length(chars)==0)
					msg <- paste0("WARNING there is no character in the scene described in line: \"",paste(line,collapse=","),"\"")
				else #if(length(chars)==1)
					msg <- paste0("WARNING there is less than two characters in the scene described in line: \"",paste(line,collapse=","),"\"")
				tlog(3,msg)
				#warning(msg)
			}
			else
			{	if(length(chars)>length(unique(chars)))
				{	msg <- paste0("WARNING the same character(s) appear(s) several times in line: \"",paste(line,collapse=","),"\"")
					tlog(3,msg)
					#stop(msg)
					chars <- unique(chars)
				}
				if(length(chars)<=1)
				{	msg <- paste0("WARNING after having removed the multi-occurring character, only this character remains in the scene described in line: \"",paste(line,collapse=","),"\"")
					tlog(3,msg)
					#warning(msg)
				}
				else
				{	chars.mat <- t(combn(x=chars,m=2))
				
					# add scene to data frame
					tmp.df <- data.frame(From=(chars.mat[,1]), To=chars.mat[,2], 
							Start=as.integer(rep(start.panel.id,nrow(chars.mat))), End=as.integer(rep(end.panel.id,nrow(chars.mat))),
							stringsAsFactors=FALSE)
					colnames(tmp.df) <- cn
					inter.df <- rbind(inter.df, tmp.df)
				}
			}
		}
		char.scenes <- c(char.scenes, list(chars))
		
		# update the scene stats
		panel.nbr <- end.panel.id - start.panel.id + 1
		page.nbr <- end.page.id - start.page.id + 1
		match.start <- start.panel==1
		match.end <- end.panel==page.info[end.page.id,COL_PAGES_PANELS]
		row.scene <- data.frame(
			volume, volume.id,
			start.page, start.page.id, 
			start.panel, start.panel.id,
			end.page, end.page.id, 
			end.panel, end.panel.id,
			panel.nbr, page.nbr, length(chars),
			match.start, match.end, match.start && match.end,
			stringsAsFactors=FALSE, check.names=FALSE
		)
		colnames(row.scene) <- c(
			COL_STATS_VOLUME, COL_STATS_VOLUME_ID, 
			COL_STATS_START_PAGE, COL_STATS_START_PAGE_ID, 
			COL_STATS_START_PANEL, COL_STATS_START_PANEL_ID,
			COL_STATS_END_PAGE, COL_STATS_END_PAGE_ID, 
			COL_STATS_END_PANEL, COL_STATS_END_PANEL_ID, 
			COL_STATS_PANELS, COL_STATS_PAGES, COL_STATS_CHARS,
			COL_STATS_MATCH_START, COL_STATS_MATCH_END, COL_STATS_MATCH_BOTH
		)
		stats.scenes <- rbind(stats.scenes, row.scene)
	}
	
	tlog(2,"Conversion of the interaction raw data completed")
	result <- list(inter.df=inter.df, stats.scenes=stats.scenes, char.scenes=char.scenes)
	return(result)
}



###############################################################################
# Reads the table describing the characters of the BD series: the name is compulsory,
# then the other columns depend on the series itself. The function also checks
# whether some character misses from the table based on the previously loaded
# interactions, and vice-versa.
#
# char.scenes: list of characters for each scene.
#
# returns: a dataframe listing the character information, with the following columns:
#			- COL_VOLS_VOLUME: volume in the BD series.
#			- COL_VOLS_PAGE_START: starting page relatively to the volume.
#			- COL_VOLS_PAGE_END: ending page relatively to the volume.
#			- COL_VOLS_LENGTH: number of (effective) pages in the volume.
#			- COL_VOLS_TITLE: title of the volume.
###############################################################################
read.char.table <- function(char.scenes)
{	if(file.exists(CHAR_FILE))
	{	tlog(2,"Trying to read the character file (",CHAR_FILE,")")
		
		# read the proper table
		char.info <- read.csv(CHAR_FILE, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
		table.chars <- char.info[,COL_CHAR_NAME]
		table.chars <- sort(table.chars)
		
		# add the frequency column
		char.info <- cbind(char.info, rep(0, nrow(char.info)))
		colnames(char.info)[ncol(char.info)] <- COL_CHAR_FREQ
		idx <- sapply(char.scenes, function(char.scene) length(char.scene)>1)	# only consider scenes with several characters
		tt <- table(unlist(char.scenes[idx]))
		char.info[match(names(tt),char.info[,COL_CHAR_NAME]),COL_CHAR_FREQ] <- tt
		
		# check multiple name use
		x <- table(table.chars)
		pb.chars <- names(x)[x!=1]
		if(length(pb.chars)>0)
		{	msg <- paste0("ERROR: The following names are used multiple times in file \"",CHAR_FILE,"\": ",paste(pb.chars,collapse=","))
			tlog(3,msg)
			stop(msg)
		}
		
		# get the list of characters from the scenes
		scene.chars <- sort(unique(unlist(char.scenes)))
		
		# check whether some characters miss from the table
		pb.chars <- setdiff(scene.chars,table.chars)
		if(length(pb.chars)>0)
		{	#cat(paste(pb.chars,collapse="\n"))
			msg <- paste0("ERROR: The following names are missing from file \"",CHAR_FILE,"\": ",paste(pb.chars,collapse=","))
			tlog(3,msg)
			stop(msg)
		}
		
		# check whether some characters miss at all
		pb.char <- setdiff(table.chars, scene.chars)
		if(length(pb.chars)>0)
		{	#cat(paste(pb.chars,collapse="\n"))
			msg <- paste0("WARNING: The following names are defined in file \"",CHAR_FILE,"\", but appear in no scene: ",paste(pb.chars,collapse=","))
			tlog(3,msg)
			stop(msg)
			#warning(msg)
		}
		
		tlog(2,"Reading of the character file completed")
	}
	else
	{	msg <- "No character file found"
		tlog(2,msg)
		stop(msg)
		
#		# just get the character names from the interactions
#		inter.chars <- c(inter.df[,c(COL_INTER_FROM_CHAR,COL_INTER_TO_CHAR)])
#		inter.chars <- sort(unique(inter.chars))
#		
#		char.info <- data.frame(name=c("lkj","lkj","oiu","uyt"))
#		cn <- c(COL_CHAR_NAME)
#		colnames(char.info) <- cn
	}
	
	return(char.info)
}




###############################################################################
# Reads the raw data contained in several tables, and returns them under the
# form of data frames.
#
# returns: a list of 4 dataframes, volume.info (information related to the
#          volumes), page.info (information related to the pages), inter.df
#          (interactions between the characters), and char.info (character
#		   information).
###############################################################################
read.raw.data <- function()
{	tlog(1,"Reading data files")
	
	# read the file describing the volumes
	volume.info <- read.volume.table()
	# read the file describing the pages
	page.info <- read.page.table(volume.info)
	
	# read the file describing the interactions
	tmp <- read.inter.table(volume.info, page.info)
	stats.scenes <- tmp$stats.scenes
	char.scenes <- tmp$char.scenes
	inter.df <- tmp$inter.df
	
	# read the file describing the characters
	char.info <- read.char.table(char.scenes)
	
	# update stats
	compute.stats(volume.info, page.info, char.info, stats.scenes, char.scenes)
	
	# build result and return
	result <- list(page.info=page.info, volume.info=volume.info, inter.df=inter.df, char.info=char.info)
	return(result)
}
