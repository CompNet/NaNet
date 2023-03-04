# This script contains functions used to read the raw data and build a matrix
# representing character interactions.
# 
# Vincent Labatut
# 01/2019
###############################################################################




###############################################################################
# Reads the table describing the characters of the BD series: the name is compulsory,
# then the other columns depend on the series itself. The function also checks
# whether some character misses from the table based on the previously loaded
# interactions, and vice-versa.
#
# returns: a list of dataframes.
###############################################################################
read.char.table <- function()
{	if(file.exists(CHAR_FILE))
	{	tlog(2,"Trying to read the character file (",CHAR_FILE,")")
		
		# read the character table
		char.stats <- read.csv(CHAR_FILE, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
		char.nbr <- nrow(char.stats)
		for(col in c(COL_NAME, COL_NAME_SHORT))
			char.stats[,col] <- fix.encoding(strings=char.stats[,col])
		table.chars <- char.stats[,COL_NAME]
		table.chars <- sort(table.chars)
		
		# complete the shortname column (using fullnames if no short name specified)
		if(!(COL_NAME_SHORT %in% colnames(char.stats)))
		{	char.stats <- cbind(char.stats, char.stats[,COL_NAME])
			colnames(char.stats)[ncol(char.stats)] <- COL_NAME_SHORT
		}
		else
		{	idx <- which(char.stats[,COL_NAME_SHORT]=="")
			char.stats[idx,COL_NAME_SHORT] <- char.stats[idx,COL_NAME]
		}
		
		# check multiple name use
		x <- table(table.chars)
		pb.chars <- names(x)[x!=1]
		if(length(pb.chars)>0)
		{	msg <- paste0("ERROR: The following names are used multiple times in file \"",CHAR_FILE,"\": ",paste(pb.chars,collapse=","))
			tlog(3,msg)
			stop(msg)
		}
		
		# add stats cols
		df <- data.frame(
			as.integer(rep(0,char.nbr)),
			as.integer(rep(0,char.nbr)),
			as.integer(rep(0,char.nbr)),
			as.integer(rep(0,char.nbr)),
			as.integer(rep(0,char.nbr)),
			as.integer(rep(0,char.nbr)),
			stringsAsFactors=FALSE, check.names=FALSE
		)
		colnames(df) <- c(
			COL_FREQ,
			COL_ARCS,
			COL_VOLUMES,
			COL_PAGES,
			COL_PANELS,
			COL_SCENES
		)
		char.stats <- cbind(char.stats, df)
		tlog(2,"Reading of the character file completed")
	}
	else
	{	msg <- "ERROR: No character file found"
		tlog(2,msg)
		stop(msg)
		
#		# just get the character names from the interactions
#		inter.chars <- c(inter.df[,c(COL_FROM_CHR,COL_CHAR_TO)])
#		inter.char <- sort(unique(iner.chars))
#		
#		char.stats <- data.frame(name=c("lkj","lkj","oiu","uyt"))
#		cn <- c(COL_NAME)
#		colnames(char.stats) <- cn
	}
	
	return(char.stats)
}




###############################################################################
# Reads the table describing the volumes of the BD series: starting page, ending
# page, etc.
#
# returns: a list of dataframes.
###############################################################################
read.volume.table <- function()
{	tlog(2,"Trying to read the volume file (",VOLUME_FILE,")")
	
	# read the proper table
	volume.stats <- read.csv(VOLUME_FILE, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	vol.nbr <- nrow(volume.stats)
	
	# add volume id
	volume.stats <- cbind(1:nrow(volume.stats), volume.stats)
	colnames(volume.stats)[1] <- COL_VOLUME_ID
	
	# add stats cols
	df <- data.frame(
		integer(vol.nbr), integer(vol.nbr),
		integer(vol.nbr), integer(vol.nbr),
		integer(vol.nbr), integer(vol.nbr),
		integer(vol.nbr),
		integer(vol.nbr),
		integer(vol.nbr),
		stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(df) <- c(
		COL_PAGE_START_ID, COL_PAGE_END_ID,
		COL_PANEL_START_ID, COL_PANEL_END,
		COL_SCENE_START_ID, COL_SCENE_END_ID,
		COL_PANELS,
		COL_SCENES,
		COL_CHARS
	)
	volume.stats <- cbind(volume.stats, df)
	
	# clean text columns
	for(col in c(COL_SERIES, COL_TITLE, COL_ARC))
		volume.stats[,col] <- fix.encoding(strings=volume.stats[,col])
	
	tlog(2,"Reading of the volume file completed")
	return(volume.stats)
}




###############################################################################
# Initializes the table describing arcs, based on the table describing volumes
# that was previously read from a file.
#
# volume.stats: table describing the series volumes.
# 
# returns: a list of dataframes.
###############################################################################
init.arc.table <- function(
	volume.stats)
{	tlog(2,"Initializing the arc table")
	
	arc.titles <- unique(volume.stats[,COL_ARC])
	arc.nbr <- length(arc.titles)
	tlog(4,"Retrieving unique arc titles from the volume table: found ",arc.nbr)
	
	# init stats table for arcs
	arc.stats <- data.frame(
		integer(arc.nbr),
		character(arc.nbr), 
		integer(arc.nbr),
		integer(arc.nbr),
		integer(arc.nbr),
		integer(arc.nbr),
		integer(arc.nbr),
		integer(arc.nbr),
		stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(arc.stats) <- c(
		COL_ARC_ID, 
		COL_TITLE, 
		COL_RANK,
		COL_VOLUMES,
		COL_PAGES,
		COL_PANELS,
		COL_SCENES,
		COL_CHARS
	)

	# filling the table
	tlog(4,"Filling the table")
	arc.stats[,COL_ARC_ID] <- 1:arc.nbr
	arc.stats[,COL_TITLE] <- arc.titles
	
	# add arc id column to volume table
	tlog(4,"Add an Arc id column to the volume table")
	arc.idx <- match(volume.stats[,COL_ARC],arc.stats[,COL_TITLE])
	volume.stats <- cbind(volume.stats, arc.stats[arc.idx,COL_ARC_ID])
	colnames(volume.stats)[ncol(volume.stats)] <- COL_ARC_ID
	
	# fill a few columns of the arc table
	vol.ids <- lapply(1:arc.nbr, function(a) which(volume.stats[,COL_ARC_ID]==a))
	arc.stats[,COL_RANK] <- rank(sapply(vol.ids, function(vols) min(volume.stats[vols,COL_RANK])))
	arc.stats[,COL_VOLUMES] <- sapply(vol.ids, length)
	arc.stats[,COL_PAGES] <- sapply(vol.ids, function(vols) sum(volume.stats[vols,COL_PAGES]))
	
	tlog(2,"Arc table initialization complete")
	res <- list(
		volume.stats=volume.stats, 
		arc.stats=arc.stats
	)
	return(res)
}




###############################################################################
# Reads the table describing the pages constituting the BD series,
# performs some verifications and add some columns required for subsequent
# processing.
#
# volume.stats: table describing the series volumes.
# arc.stats: table describing the series narrative arcs.
# 
# returns: a list of dataframes.
###############################################################################
read.page.table <- function(
	volume.stats, 
	arc.stats)
{	tlog(2,"Processing the page table")
	
	# read the proper table
	tlog(4,"Reading the page file (",PAGE_FILE,")")
	page.stats <- read.csv(PAGE_FILE, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	pg.nbr <- nrow(page.stats)
	
	# check that each relative page number matches the interval defined in the volume table 
	vol.ids <- match(page.stats[,COL_VOLUME], volume.stats[,COL_VOLUME])
	err.pg.idx <- which(page.stats[,COL_PAGE]<volume.stats[vol.ids,COL_PAGE_START]
		| page.stats[,COL_PAGE]>volume.stats[vol.ids,COL_PAGE_END])
	if(length(err.pg.idx)>0)
	{	tmp.msg <- apply(as.matrix(page.stats[err.pg.idx,]),1, function(r) paste(r,collapse=","))
		tmp.msg <- apply(cbind(err.pg.idx,tmp.msg),1, function(r) paste(r,collapse=": "))
		tmp.msg <- c(paste(colnames(page.stats),collapse=","),tmp.msg)
		tmp.msg <- paste(tmp.msg,collapse="\n")
		msg <- paste0("ERROR while reading file \"",PAGE_FILE,"\". The following pages are out of bounds, compared to the volume information:\n",tmp.msg)
		tlog(3,msg)
		stop(msg)
	}
	
	# add the page id (unique for the whole series)
	page.stats <- cbind(1:nrow(page.stats), page.stats)
	colnames(page.stats)[1] <- COL_PAGE_ID
	
	# add the volume id
	page.stats <- cbind(page.stats,vol.ids)
	colnames(page.stats)[ncol(page.stats)] <- COL_VOLUME_ID
	
	# add the arc id
	arc.ids <- volume.stats[vol.ids,COL_ARC_ID]
	page.stats <- cbind(page.stats,arc.ids)
	colnames(page.stats)[ncol(page.stats)] <- COL_ARC_ID
	
	# add the id of the panel starting the page
	start.panel.ids <- cumsum(c(1,page.stats[,COL_PANELS]))
	start.panel.ids <- start.panel.ids[1:(length(start.panel.ids)-1)]
	page.stats <- cbind(page.stats,start.panel.ids)
	colnames(page.stats)[ncol(page.stats)] <- COL_PANEL_START_ID
	
	# add the id of the panel ending the page
	end.panel.ids <- c(start.panel.ids[2:pg.nbr]-1, start.panel.ids[pg.nbr]+page.stats[pg.nbr,COL_PANELS]-1)
	page.stats <- cbind(page.stats,end.panel.ids)
	colnames(page.stats)[ncol(page.stats)] <- COL_PANEL_END_ID
	
	# add stats cols
	df <- data.frame(
		integer(pg.nbr),
		integer(pg.nbr),
		integer(pg.nbr),
		stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(df) <- c(
		COL_RANK,
		COL_SCENES,
		COL_CHARS
	)
	page.stats <- cbind(page.stats, df)
	
	# init story rank
	page.stats[,COL_RANK] <- rank(volume.stats[page.stats[,COL_VOLUME_ID],COL_RANK]*(nrow(page.stats)+1) + page.stats[,COL_PAGE])
	
	# complete volume table
	tlog(4,"Completing the volume table")
	# page ids
	page.ids <- t(sapply(1:nrow(volume.stats), function(v) range(which(page.stats[,COL_VOLUME_ID]==v))))
	volume.stats[,c(COL_PAGE_START_ID, COL_PAGE_END_ID)] <- page.ids
	# panel ids
	volume.stats[,COL_PANEL_START_ID] <- page.stats[volume.stats[,COL_PAGE_START_ID],COL_PANEL_START_ID]
	volume.stats[,COL_PANEL_END_ID] <- page.stats[volume.stats[,COL_PAGE_END_ID],COL_PANEL_END_ID]
		
	tlog(2,"Reading of the page file completed")
	res <- list(
		page.stats=page.stats, 
		volume.stats=volume.stats, 
		arc.stats=arc.stats
	)
	return(res)	
}	




###############################################################################
# Initializes the table describing panels, based on the other tables.
#
# page.stats: table describing all the pages constituting the series.
# char.stats: table describing all the characters.
# volume.stats: table describing the series volumes.
# arc.stats: table describing the series narrative arcs.
# 
# returns: a list of dataframes.
###############################################################################
init.panel.table <- function(
	page.stats, 
	char.stats, 
	volume.stats, 
	arc.stats)
{	tlog(2,"Initializing the panel table")
	
	panel.nbr <- sum(page.stats[,COL_PANELS])
	panel.ids <- 1:panel.nbr
	
	panel.stats <- data.frame(
		integer(panel.nbr), integer(panel.nbr), integer(panel.nbr), integer(panel.nbr), 
		integer(panel.nbr), integer(panel.nbr), character(panel.nbr), 
		integer(panel.nbr),
		integer(panel.nbr),
		logical(panel.nbr), logical(panel.nbr), logical(panel.nbr),
		stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(panel.stats) <- c(
		COL_PANEL_ID, COL_PAGE_ID, COL_VOLUME_ID, COL_ARC_ID,
		COL_PANEL, COL_PAGE, COL_VOLUME,  
		COL_RANK, 
		COL_CHARS, 
		COL_MATCH_START, COL_MATCH_END, COL_MATCH_BOTH
	)
	
	# filling the table
	tlog(4,"Filling the table")
	panel.stats[,COL_PANEL_ID] <- panel.ids
	panel.stats[,COL_PANEL] <- unlist(sapply(1:nrow(page.stats), function(p) 1:page.stats[p,COL_PANELS]))
	panel.stats[,COL_PAGE_ID] <- sapply(1:panel.nbr, function(p) which(page.stats[,COL_PANEL_START_ID]<=panel.ids[p] & page.stats[,COL_PANEL_END_ID]>=panel.ids[p]))
	panel.stats[,COL_PAGE] <- page.stats[panel.stats[,COL_PAGE_ID], COL_PAGE]
	panel.stats[,COL_VOLUME_ID] <- page.stats[panel.stats[,COL_PAGE_ID], COL_VOLUME_ID]
	panel.stats[,COL_VOLUME] <- page.stats[panel.stats[,COL_PAGE_ID], COL_VOLUME]
	panel.stats[,COL_ARC_ID] <- page.stats[panel.stats[,COL_PAGE_ID], COL_ARC_ID]
	panel.stats[,COL_RANK] <- rank(page.stats[panel.stats[,COL_PAGE_ID],COL_RANK]*(nrow(panel.stats)+1) + panel.stats[,COL_PANEL])
	panel.stats[,COL_MATCH_START] <- panel.stats[,COL_PANEL]==1
	panel.stats[,COL_MATCH_END] <- panel.stats[,COL_PANEL]==page.stats[panel.stats[,COL_PAGE_ID],COL_PANELS]
	panel.stats[,COL_MATCH_BOTH] <- panel.stats[,COL_MATCH_START] & panel.stats[,COL_MATCH_END]
	
	# updating the other tables
	tlog(4,"Updating the other tables")
	volume.stats[, COL_PANELS] <- sapply(1:nrow(volume.stats), function(v) length(which(panel.stats[,COL_VOLUME_ID]==v)))
	arc.stats[, COL_PANELS] <- sapply(1:nrow(arc.stats), function(a) length(which(panel.stats[,COL_ARC_ID]==a)))
	
	tlog(2,"Panel table initialization complete")
	res <- list(
		panel.stats=panel.stats,
		page.stats=page.stats, 
		char.stats=char.stats,
		volume.stats=volume.stats, 
		arc.stats=arc.stats
	)
	return(res)
}



	
###############################################################################
# Reads the table describing the interactions between characters for each scene, 
# and coverts them into an edge list while performing some verifications. 
#
# char.det: character detection mode ("implicit" or "explicit").
# panel.stats: table describing all the panels constituting the series.
# page.stats: table describing all the pages constituting the series.
# char.stats: table describing all the characters.
# volume.stats: table describing the series volumes.
# arc.stats: table describing the series narrative arcs.
# 
# returns: a list of dataframes.
###############################################################################
read.inter.table <- function(
	char.det,
	panel.stats, 
	page.stats, 
	char.stats, 
	volume.stats, 
	arc.stats)
{	# set input file
	if(char.det=="implicit")
		inter.file <- INTER_IMPL_FILE
	else
		inter.file <- INTER_EXPL_FILE
	
	# read the text file
	tlog(2,"Reading the interaction file (",inter.file,")")
	con <- file(inter.file, open="r")
	temp <- readLines(con)
	close(con)
	temp <- fix.encoding(strings=temp)
	lines <- strsplit(temp, split="\t", fixed=TRUE)
	tlog(4,"File read: ",length(lines)," lines)")
	
	# init scene table
	scene.stats <- data.frame(
		integer(), 
		integer(), 
		character(), integer(),
		integer(), integer(),
		integer(), integer(), 
		integer(), integer(),
		integer(), integer(), 
		integer(), 
		integer(), integer(), integer(),
		logical(), logical(), logical(),
		stringsAsFactors=FALSE, check.names=FALSE
	)
	scene.stats.cn <- c(
		COL_SCENE_ID,
		COL_ARC_ID,
		COL_VOLUME, COL_VOLUME_ID,
		COL_PAGE_START, COL_PAGE_START_ID, 
		COL_PANEL_START, COL_PANEL_START_ID,
		COL_PAGE_END, COL_PAGE_END_ID, 
		COL_PANEL_END, COL_PANEL_END_ID, 
		COL_RANK,
		COL_PANELS, COL_PAGES, COL_CHARS,
		COL_MATCH_START, COL_MATCH_END, COL_MATCH_BOTH
	)
	colnames(scene.stats) <- scene.stats.cn
	
	# init interaction dataframe
	inter.df <- data.frame(
		character(), character(),
		integer(),
		integer(),
		character(), integer(),
		integer(), integer(), 
		integer(), integer(), 
		integer(), integer(), 
		integer(), integer(), 
		integer(), integer(), 
		integer(), 
		stringsAsFactors=FALSE, check.names=FALSE)
	inter.df.cn <- c(
		COL_CHAR_FROM, COL_CHAR_TO, 
		COL_SCENE_ID,
		COL_ARC_ID,
		COL_VOLUME, COL_VOLUME_ID,
		COL_PAGE_START, COL_PAGE_START_ID, 
		COL_PANEL_START, COL_PANEL_START_ID,
		COL_PAGE_END, COL_PAGE_END_ID, 
		COL_PANEL_END, COL_PANEL_END_ID,
		COL_RANK,
		COL_PANELS, COL_PAGES)
	colnames(inter.df) <- inter.df.cn
	Encoding(inter.df[,COL_CHAR_FROM]) <- "UTF-8"
	Encoding(inter.df[,COL_CHAR_TO]) <- "UTF-8"
	
	# init character lists
	panel.chars <- vector(mode="list", length=nrow(panel.stats))
	page.chars <- vector(mode="list", length=nrow(page.stats))
	scene.chars <- list()
	volume.chars <- vector(mode="list", length=nrow(volume.stats))
	arc.chars <- vector(mode="list", length=nrow(arc.stats))
	
	# loop over the scenes
	tlog(2,"Looping over the lines (=scenes)")
	prev.end.panel.id <- 0
	for(l in 2:length(lines))	# skipping header line
	{	line <- lines[[l]]
		scene.id <- l - 1
		
		# get volume
		volume <- line[1]
		volume.id <- which(volume.stats[,COL_VOLUME]==volume)
		# get arc
		arc.id <- volume.stats[volume.id,COL_ARC_ID]
		
		# get start page and panel
		start.page <- line[2]
		start.page.id <- which(page.stats[,COL_PAGE]==start.page & page.stats[,COL_VOLUME_ID]==volume.id)
		if(length(start.page.id)==0)
		{	msg <- paste0("ERROR while reading file \"",inter.file,"\". Starting page not found in line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			stop(msg)
		}
		start.panel <-  as.integer(line[3])
		if(start.panel>page.stats[start.page.id,COL_PANELS])
		{	msg <- paste0("ERROR while reading file \"",inter.file,"\". Starting panel is out of page in line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			stop(msg)
		}
		start.panel.id <- page.stats[start.page.id,COL_PANEL_START_ID] + start.panel - 1
		if(prev.end.panel.id>0 && start.panel.id>(prev.end.panel.id+1))
		{	msg <- paste0("WARNING while reading file \"",inter.file,"\". Missing panel(s) between this scene and the previous one, at line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			#warning(msg)
		}
		if(prev.end.panel.id>start.panel.id)
		{	msg <- paste0("WARNING while reading file \"",inter.file,"\". Panel overlap (",prev.end.panel.id," vs. ",start.panel.id,") between two consecutive scenes, at line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
		}
		
		# get end page and panel
		end.page <- line[4]
		end.page.id <- which(page.stats[,COL_PAGE]==end.page & page.stats[,COL_VOLUME_ID]==volume.id)
		if(length(end.page.id)==0)
		{	msg <- paste0("ERROR while reading file \"",inter.file,"\". Ending page not found in line: \"",paste(line,collapse=","),"\"") 
			tlog(3,msg)
			stop(msg)
		}
		end.panel <- as.integer(line[5])
		if(end.panel>page.stats[end.page.id,COL_PANELS])
		{	msg <- paste0("ERROR while reading file \"",inter.file,"\". Ending panel is out of page in line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			stop(msg)
		}
		end.panel.id <- page.stats[end.page.id,COL_PANEL_START_ID] + end.panel - 1
		prev.end.panel.id <- max(prev.end.panel.id, end.panel.id)	# max in order to consider overlapping/parallel scenes
		
		# check that the end is after the start
		if(start.panel.id>end.panel.id)
		{	msg <- paste0("ERROR while reading file \"",inter.file,"\". Starting panel located after ending panel in line: \"",paste(line,collapse=","),"\"")
			tlog(3,msg)
			stop(msg)
		}
		
		# get durations
		panel.nbr <- end.panel.id - start.panel.id + 1
		page.nbr <- end.page.id - start.page.id + 1

		# get all combinations of characters
		if(length(line)<6)
			chars <- c()
		else
		{	# retrieve the character names
			chars <- line[6:length(line)]
			chars <- gsub("[()]", "", chars)	# remove parentheses (representing ghost characters)
			chars <- sapply(strsplit(chars,"/"), function(v)	# remove "/" corresponding to disguises
			{	if(length(v)==0)
					return("")
				else if(length(v)==1)
					return(trimws(v))
				else if(length(v)==2)
					return(trimws(v[2]))
				else
				{	msg <- paste0("ERROR while reading file \"",inter.file,"\". Problem when splitting the names: ",v)
					tlog(4,msg)
					stop(msg)
				}
			})
			chars <- sort(chars[which(chars!="" & chars!=" ")])
			
			# check the character names
			if(length(chars)==0)
				msg <- paste0("WARNING while reading file \"",inter.file,"\". There is no character in the scene described in line: \"",paste(line,collapse=","),"\"")
			else 
			{	# check whether some characters miss from the table
				pb.chars <- setdiff(chars,char.stats[,COL_NAME])
				if(length(pb.chars)>0)
				{	msg <- paste0("ERROR: while reading file \"",inter.file,"\". The following names appear in scene \"",paste(line,collapse=","),"\" but miss from file \"",CHAR_FILE,"\": ",paste(pb.chars,collapse=","))
					tlog(3,msg)
					#stop(msg)
				}
				else if(length(chars)==1)
				{	msg <- paste0("WARNING while reading file \"",inter.file,"\". There are fewer than two characters in the scene described in line: \"",paste(line,collapse=","),"\"")
					tlog(3,msg)
					#warning(msg)
				}
				else
				{	if(length(chars)>length(unique(chars)))
					{	msg <- paste0("WARNING while reading file \"",inter.file,"\". The same character(s) appear(s) several times in line: \"",paste(line,collapse=","),"\"")
						tlog(3,msg)
						#stop(msg)
						chars <- unique(chars)
					}
					if(length(chars)<=1)
					{	msg <- paste0("WARNING while reading file \"",inter.file,"\". After having removed the multi-occurring character, only this character remains in the scene described in line: \"",paste(line,collapse=","),"\"")
						tlog(3,msg)
						#warning(msg)
					}
					else
					{	chars.mat <- t(combn(x=chars,m=2))
						rr <- nrow(chars.mat)
						
						# add interactions to dataframe
						tmp.df <- data.frame(
							chars.mat[,1], chars.mat[,2], 
							as.integer(rep(scene.id,rr)), as.integer(rep(arc.id,rr)), rep(volume,rr), as.integer(rep(volume.id,rr)),
							as.integer(rep(start.page,rr)), as.integer(rep(start.page.id,rr)), 
							as.integer(rep(start.panel,rr)), as.integer(rep(start.panel.id,rr)),
							as.integer(rep(end.page,rr)), as.integer(rep(end.page.id,rr)), 
							as.integer(rep(end.panel,rr)), as.integer(rep(end.panel.id,rr)),
							as.integer(NA),
							as.integer(rep(panel.nbr,rr)), as.integer(rep(page.nbr,rr)),
							stringsAsFactors=FALSE, check.names=FALSE)
						colnames(tmp.df) <- inter.df.cn
						inter.df <- rbind(inter.df, tmp.df)
					}
				}
			}
		}
		
		# update scene stats
		match.start <- start.panel==1
		match.end <- end.panel==page.stats[end.page.id,COL_PANELS]
		row.scene <- data.frame(
			as.integer(scene.id), 
			as.integer(arc.id), volume, as.integer(volume.id),
			as.integer(start.page), as.integer(start.page.id), 
			as.integer(start.panel), as.integer(start.panel.id),
			as.integer(end.page), as.integer(end.page.id), 
			as.integer(end.panel), as.integer(end.panel.id),
			as.integer(NA),
			as.integer(panel.nbr), as.integer(page.nbr), as.integer(length(chars)),
			match.start, match.end, match.start && match.end,
			stringsAsFactors=FALSE, check.names=FALSE
		)
		colnames(row.scene) <- scene.stats.cn
		scene.stats <- rbind(scene.stats, row.scene)
		
		# update character lists
		scene.chars <- c(scene.chars, list(chars))
		if(length(chars)>0)
		{	for(p in start.panel.id:end.panel.id) 
				panel.chars[[p]] <- sort(union(panel.chars[[p]], chars))
			for(p in start.page.id:end.page.id)
				page.chars[[p]] <- sort(union(page.chars[[p]], chars))
			volume.chars[[volume.id]] <- sort(union(volume.chars[[volume.id]], chars))
			arc.chars[[arc.id]] <- sort(union(arc.chars[[arc.id]], chars))
		}
		
		# update panel stats
		panel.stats[start.panel.id:end.panel.id,COL_CHARS] <- sapply(panel.chars[start.panel.id:end.panel.id], length)
	}
	
	# update scene and interaction ranks
	scene.stats[,COL_RANK] <- rank(volume.stats[scene.stats[,COL_VOLUME_ID],COL_RANK]*(nrow(scene.stats)+1) + scene.stats[,COL_SCENE_ID], ties.method="first")
	inter.df[,COL_RANK] <- rank(volume.stats[inter.df[,COL_VOLUME_ID],COL_RANK]*(nrow(inter.df)+1) + 1:nrow(inter.df), ties.method="first")
	
	# check unused characters
	pb.chars <- setdiff(char.stats[,COL_NAME], unique(unlist(scene.chars)))
	if(length(pb.chars)>0)
	{	#cat(paste(pb.chars,collapse="\n"))
		msg <- paste0("WARNING: while reading file \"",inter.file,"\". The following names are defined in file \"",CHAR_FILE,"\", but appear in no scene: ",paste(pb.chars,colapse=","))
		tlog(3,msg)
		#stop(msg)
		#warning(msg)
	}
	
	tlog(2,"Reading of the interaction file completed")
	res <- list(
		inter.df=inter.df,										# interactions
		panel.stats=panel.stats, panel.chars=panel.chars,		# panels
		page.stats=page.stats, page.chars=page.chars,			# pages
		scene.stats=scene.stats, scene.chars=scene.chars,		# scenes
		char.stats=char.stats,									# characters 
		volume.stats=volume.stats, volume.chars=volume.chars,	# volumes 
		arc.stats=arc.stats, arc.chars=arc.chars				# arcs
	)
	return(res)	
}




###############################################################################
# Records the tables and lists extracted from the raw data.
#
# inter.df: table of pairwise interactions.
# panel.stats: table describing all the panels constituting the series.
# panel.chars: list of characters involved in each panel.
# page.stats: table describing all the pages constituting the series.
# page.chars: list of characters involved in each page.
# scene.stats: table describing all the scene constituting the series.
# scene.chars: list of characters involved in each scene.
# char.stats: table describing all the characters.
# volume.stats: table describing the series volumes.
# volume.chars: list of characters involved in each volume.
# arc.stats: table describing the series narrative arcs.
# arc.chars: list of characters involved in each arc.
#
# returns: a list of dataframes.
###############################################################################
update.all.tables <- function(
	inter.df, 
	panel.stats, panel.chars, 
	page.stats, page.chars, 
	scene.stats, scene.chars, 
	char.stats, 
	volume.stats, volume.chars, 
	arc.stats, arc.chars)
{	tlog(2,"Updating all tables")
	
	# update scene start/end in volume table
	tlog(4,"Updating volume table")
	volume.stats[,c(COL_SCENE_START_ID,COL_SCENE_END_ID)] <- t(sapply(1:nrow(volume.stats), function(v) range(which(scene.stats[,COL_VOLUME_ID]==v)))) 
	
	# update character counts in stats tables
	tlog(4,"Computing character counts")
	page.stats[,COL_CHARS] <- sapply(page.chars, length)
	volume.stats[,COL_CHARS] <- sapply(volume.chars, length)
	arc.stats[,COL_CHARS] <- sapply(arc.chars, length)
	
	# update scene counts in stats tables
	tlog(4,"Computing scene counts")
	page.stats[,COL_SCENES] <- sapply(1:nrow(page.stats), function(p) length(which(scene.stats[,COL_PAGE_START_ID]<=p & scene.stats[,COL_PAGE_END_ID]>=p)))
	volume.stats[,COL_SCENES] <- sapply(1:nrow(volume.stats), function(v) length(which(scene.stats[,COL_VOLUME_ID]==v)))
	arc.stats[,COL_SCENES] <- sapply(1:nrow(arc.stats), function(a) length(which(scene.stats[,COL_ARC_ID]==a)))
	
	# update narrative unit counts in char stats table
	tlog(4,"Computing narrative unit counts")
	tt <- table(unlist(panel.chars)); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PANELS] <- tt
	tt <- table(unlist(page.chars)); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PAGES] <- tt
	tt <- table(unlist(scene.chars)); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_SCENES] <- tt
	tt <- table(unlist(volume.chars)); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_VOLUMES] <- tt
	tt <- table(unlist(arc.chars)); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_ARCS] <- tt
	
	# update frequency column in char stats table
	tlog(4,"Computing character frequency")
	idx <- sapply(scene.chars, function(char.scene) length(char.scene)>1)	# only consider scenes with several characters
	tt <- table(unlist(scene.chars[idx]))
	char.stats[match(names(tt),char.stats[,COL_NAME]),COL_FREQ] <- tt
	
	tlog(2,"Table update completed")
	res <- list(
		inter.df=inter.df,										# interactions
		panel.stats=panel.stats, panel.chars=panel.chars,		# panels
		page.stats=page.stats, page.chars=page.chars,			# pages
		scene.stats=scene.stats, scene.chars=scene.chars,		# scenes
		char.stats=char.stats,									# characters 
		volume.stats=volume.stats, volume.chars=volume.chars,	# volumes 
		arc.stats=arc.stats, arc.chars=arc.chars				# arcs
	)
	return(res)	
}




###############################################################################
# Records the tables and lists extracted from the raw data.
#
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# inter.df: table of pairwise interactions.
# panel.stats: table describing all the panels constituting the series.
# panel.chars: list of characters involved in each panel.
# page.stats: table describing all the pages constituting the series.
# page.chars: list of characters involved in each page.
# scene.stats: table describing all the scene constituting the series.
# scene.chars: list of characters involved in each scene.
# char.stats: table describing all the characters.
# chapter.stats: table describing the series chapters (optionnal).
# chapter.chars: list of characters involved in each chapter (optionnal).
# volume.stats: table describing the series volumes.
# volume.chars: list of characters involved in each volume.
# arc.stats: table describing the series narrative arcs.
# arc.chars: list of characters involved in each arc.
###############################################################################
write.corpus.data <- function(
	char.det, 
	inter.df, 
	panel.stats, panel.chars, 
	page.stats, page.chars, 
	scene.stats, scene.chars, 
	char.stats, 
	chapter.stats, chapter.chars, 
	volume.stats, volume.chars, 
	arc.stats, arc.chars)
{	tlog(2,"Writing statistics and character lists")
	
	# interactions
	file <- get.path.stats.corpus(char.det=char.det, pref="_interactions")
	tlog(4,"Writing interaction file \"",file,"\"")
	write.csv(x=inter.df, file=paste0(file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
	
	# panel stats
	file <- get.path.stats.corpus(object="panels", char.det=char.det, pref="_panel_stats")
	tlog(4,"Writing panel stats \"",file,"\"")
	write.csv(x=panel.stats, file=paste0(file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
	# panel chars
	tab <- cbind(
		1:nrow(panel.stats),
		sapply(panel.chars, function(chars) paste(chars,collapse="\t"))
	)
	colnames(tab) <- c(COL_PANEL_ID, COL_CHARS)
	file <- get.path.stats.corpus(object="panels", char.det=char.det, pref="_panel_chars")
	tlog(4,"Writing panel chars \"",file,"\"")
	write.table(tab, file=paste0(file,".txt"), fileEncoding="UTF-8", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	
	# page stats
	file <- get.path.stats.corpus(object="pages", char.det=char.det, pref="_page_stats")
	tlog(4,"Writing page stats \"",file,"\"")
	write.csv(x=page.stats, file=paste0(file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
	# page chars
	tab <- cbind(
		1:nrow(page.stats),
		sapply(page.chars, function(chars) paste(chars,collapse="\t"))
	)
	colnames(tab) <- c(COL_PAGE_ID, COL_CHARS)
	file <- get.path.stats.corpus(object="pages", char.det=char.det, pref="_page_chars")
	tlog(4,"Writing page chars \"",file,"\"")
	write.table(tab, file=paste0(file,".txt"), fileEncoding="UTF-8", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	
	# scene stats
	file <- get.path.stats.corpus(object="scenes", char.det=char.det, pref="_scene_stats")
	tlog(4,"Writing scene stats \"",file,"\"")
	write.csv(x=scene.stats, file=paste0(file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
	# scene chars
	tab <- cbind(
		1:nrow(scene.stats),
		sapply(scene.chars, function(chars) paste(chars,collapse="\t"))
	)
	colnames(tab) <- c(COL_SCENE_ID, COL_CHARS)
	file <- get.path.stats.corpus(object="scenes", char.det=char.det, pref="_scene_chars")
	tlog(4,"Writing scene chars \"",file,"\"")
	write.table(tab, file=paste0(file,".txt"), fileEncoding="UTF-8", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	
	# characters
	file <- get.path.stats.corpus(object="characters", char.det=char.det, subfold="unfiltered", pref="_char_stats")
	tlog(4,"Writing character stats \"",file,"\"")
	write.csv(x=char.stats, file=paste0(file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
	
	# possibly deal with chapters
	if(hasArg(chapter.stats))
	{	# chapter stats
		file <- get.path.stats.corpus(object="chapters", char.det=char.det, pref="_chapter_stats")
		tlog(4,"Writing chapter stats \"",file,"\"")
		write.csv(x=chapter.stats, file=paste0(file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
		# chapter chars
		tab <- cbind(
			1:nrow(chapter.stats),
			sapply(chapter.chars, function(chars) paste(chars,collapse="\t"))
		)
		colnames(tab) <- c(COL_CHAP_ID, COL_CHARS)
		file <- get.path.stats.corpus(object="chapter", char.det=char.det, pref="_chapter_chars")
		tlog(4,"Writing chapter chars \"",file,"\"")
		write.table(tab, file=paste0(file,".txt"), fileEncoding="UTF-8", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	}
	
	# volume stats
	file <- get.path.stats.corpus(object="volumes", char.det=char.det, pref="_volume_stats")
	tlog(4,"Writing volume stats \"",file,"\"")
	write.csv(x=volume.stats, file=paste0(file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
	# volume chars
	tab <- cbind(
		1:nrow(volume.stats),
		sapply(volume.chars, function(chars) paste(chars,collapse="\t"))
	)
	colnames(tab) <- c(COL_VOLUME_ID, COL_CHARS)
	file <- get.path.stats.corpus(object="volumes", char.det=char.det, pref="_volume_chars")
	tlog(4,"Writing volume chars \"",file,"\"")
	write.table(tab, file=paste0(file,".txt"), fileEncoding="UTF-8", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	
	# arc stats
	file <- get.path.stats.corpus(object="arcs", char.det=char.det, pref="_arc_stats")
	tlog(4,"Writing arc stats \"",file,"\"")
	write.csv(x=arc.stats, file=paste0(file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
	# arc chars
	tab <- cbind(
		1:nrow(arc.stats),
		sapply(arc.chars, function(chars) paste(chars,collapse="\t"))
	)
	colnames(tab) <- c(COL_ARC_ID, COL_CHARS)
	file <- get.path.stats.corpus(object="arcs", char.det=char.det, pref="_arc_chars")
	tlog(4,"Writing arc chars \"",file,"\"")
	write.table(tab, file=paste0(file,".txt"), fileEncoding="UTF-8", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)

	tlog(2,"Recording complete")
}




###############################################################################
# Reads a file containing a list of lists of character names.
#
# file: file to read.
#
# returns: list of character name vectors.
###############################################################################
read.char.list <- function(file)
{	# read the text file
	con <- file(file, open="r")
	temp <- readLines(con)
	close(con)
	
	# clean the text
	temp <- temp[2:length(temp)]
	temp <- fix.encoding(strings=temp)
	
	# split using tabs
	lines <- strsplit(temp, split='\t', fixed=TRUE)
	
	# build list
	res <- lapply(lines, function(line)
			{	if(length(line)==1)
					list()
				else
					line[2:length(line)]
			})
	return(res)
}




###############################################################################
# Reads the tables and lists previously extracted from the raw data and recorded
# using function write.corpus.data.
#
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
#
# returns: list containing all the tables and character lists.
###############################################################################
read.corpus.data <- function(char.det)
{	tlog(2,"Reading statistics and character lists")
	result <- list()
	
	# interactions
	file <- get.path.stats.corpus(char.det=char.det, pref="_interactions")
	tlog(2,"Reading interaction file \"",file,"\"")
	inter.df <- read.csv(file=paste0(file,".csv"), header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	for(col in c(COL_CHAR_FROM, COL_CHAR_TO))
		inter.df[,col] <- fix.encoding(strings=inter.df[,col])
	result$inter.df <- inter.df
	
	# panel stats
	file <- get.path.stats.corpus(object="panels", char.det=char.det, pref="_panel_stats")
	tlog(2,"Reading panel stats file \"",file,"\"")
	panel.stats <- read.csv(file=paste0(file,".csv"), header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	result$panel.stats <- panel.stats
	# panel chars
	file <- get.path.stats.corpus(object="panels", char.det=char.det, pref="_panel_chars")
	tlog(2,"Reading panel chars file \"",file,"\"")
	panel.chars <- read.char.list(file=paste0(file,".txt"))
	result$panel.chars <- panel.chars
	
	# page stats
	file <- get.path.stats.corpus(object="pages", char.det=char.det, pref="_page_stats")
	tlog(2,"Reading page stats file \"",file,"\"")
	page.stats <- read.csv(file=paste0(file,".csv"), header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	result$page.stats <- page.stats
	# page chars
	file <- get.path.stats.corpus(object="pages", char.det=char.det, pref="_page_chars")
	tlog(2,"Reading page chars file \"",file,"\"")
	page.chars <- read.char.list(file=paste0(file,".txt"))
	result$page.chars <- page.chars
	
	# scene stats
	file <- get.path.stats.corpus(object="scenes", char.det=char.det, pref="_scene_stats")
	tlog(2,"Reading scene stats file \"",file,"\"")
	scene.stats <- read.csv(file=paste0(file,".csv"), header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	result$scene.stats <- scene.stats
	# scene chars
	file <- get.path.stats.corpus(object="scenes", char.det=char.det, pref="_scene_chars")
	tlog(2,"Reading scene chars file \"",file,"\"")
	scene.chars <- read.char.list(file=paste0(file,".txt"))
	result$scene.chars <- scene.chars
	
	# characters
	file <- get.path.stats.corpus(object="characters", char.det=char.det, subfold="unfiltered", pref="_char_stats")
	tlog(2,"Reading char stats file \"",file,"\"")
	char.stats <- read.csv(file=paste0(file,".csv"), header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	for(col in c(COL_NAME, COL_NAME_SHORT))
		char.stats[,col] <- fix.encoding(strings=char.stats[,col])
	result$char.stats <- char.stats
	
	# possibly deal with chapters
	file <- get.path.stats.corpus(object="chapters", char.det=char.det, pref="_chapter_stats")
	if(file.exists(file))
	{	# chapter stats
		tlog(2,"Reading chapter stats file \"",file,"\"")
		chapter.stats <- read.csv(file=paste0(file,".csv"), header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
		for(col in c(COL_TITLE, COL_VOLUME, COL_ARC))
			chapter.stats[,col] <- fix.encoding(strings=chapter.stats[,col])
		result$chapter.stats <- chapter.stats
		# chapter chars
		file <- get.path.stats.corpus(object="chapters", char.det=char.det, pref="_chapter_chars")
		tlog(2,"Reading chapter chars file \"",file,"\"")
		chapter.chars <- read.char.list(file=paste0(file,".txt"))
		result$chapter.chars <- chapter.chars
	}
	
	# volume stats
	file <- get.path.stats.corpus(object="volumes", char.det=char.det, pref="_volume_stats")
	tlog(2,"Reading volume stats file \"",file,"\"")
	volume.stats <- read.csv(file=paste0(file,".csv"), header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	for(col in c(COL_SERIES, COL_TITLE, COL_ARC))
		volume.stats[,col] <- fix.encoding(strings=volume.stats[,col])
	result$volume.stats <- volume.stats
	# volume chars
	file <- get.path.stats.corpus(object="volumes", char.det=char.det, pref="_volume_chars")
	tlog(2,"Reading volume chars file \"",file,"\"")
	volume.chars <- read.char.list(file=paste0(file,".txt"))
	result$volume.chars <- volume.chars
	
	# arc stats
	file <- get.path.stats.corpus(object="arcs", char.det=char.det, pref="_arc_stats")
	tlog(2,"Reading arc stats file \"",file,"\"")
	arc.stats <- read.csv(file=paste0(file,".csv"), header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	for(col in c(COL_TITLE))
		arc.stats[,col] <- fix.encoding(strings=arc.stats[,col])
	result$arc.stats <- arc.stats
	# arc chars
	file <- get.path.stats.corpus(object="arcs", char.det=char.det, pref="_arc_chars")
	tlog(2,"Reading arc chars file \"",file,"\"")
	arc.chars <- read.char.list(file=paste0(file,".txt"))
	result$arc.chars <- arc.chars
	
	# return result list
	return(result)
}




###############################################################################
# Reads the raw data contained in several tables, and returns them under the
# form of data frames. It focuses on the implicit version of the annotations.
#
# char.det: character detection mode ("implicit" or "explicit").
#
# returns: a list of dataframes.
###############################################################################
read.raw.data <- function(char.det)
{	tlog(1,"Reading data files")
	
	# read the file describing the characters
	char.stats <- read.char.table()
	
	# read the file describing the volumes
	volume.stats <- read.volume.table()
	
	# init arc table based on volume table
	tmp <- init.arc.table(volume.stats)
	volume.stats <- tmp$volume.stats
	arc.stats <- tmp$arc.stats
	
	# read the file describing the pages
	tmp <- read.page.table(volume.stats, arc.stats)
	page.stats <- tmp$page.stats
	volume.stats <- tmp$volume.stats
	arc.stats <- tmp$arc.stats
	
	# init panel table based on other tables
	tmp <- init.panel.table(page.stats, char.stats, volume.stats, arc.stats)
	panel.stats <- tmp$panel.stats
	page.stats <- tmp$page.stats
	volume.stats <- tmp$volume.stats
	arc.stats <- tmp$arc.stats
	
	# read the file describing the interactions
	tmp <- read.inter.table(char.det, panel.stats, page.stats, char.stats, volume.stats, arc.stats)
	inter.df <- tmp$inter.df
	panel.stats <- tmp$panel.stats; panel.chars <- tmp$panel.chars
	page.stats <- tmp$page.stats; page.chars <- tmp$page.chars
	char.stats <- tmp$char.stats
	scene.stats <- tmp$scene.stats; scene.chars <- tmp$scene.chars
	volume.stats <- tmp$volume.stats; volume.chars <- tmp$volume.chars
	arc.stats <- tmp$arc.stats; arc.chars <- tmp$arc.chars
	
	# update missing stats
	tmp <- update.all.tables(
		inter.df=inter.df,
		panel.stats=panel.stats, panel.chars=panel.chars,
		page.stats=page.stats, page.chars=page.chars,
		scene.stats=scene.stats, scene.chars=scene.chars,
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.chars=volume.chars, 
		arc.stats=arc.stats, arc.chars=arc.chars
	)
	inter.df <- tmp$inter.df
	panel.stats <- tmp$panel.stats; panel.chars <- tmp$panel.chars
	page.stats <- tmp$page.stats; page.chars <- tmp$page.chars
	char.stats <- tmp$char.stats
	scene.stats <- tmp$scene.stats; scene.chars <- tmp$scene.chars
	volume.stats <- tmp$volume.stats; volume.chars <- tmp$volume.chars
	arc.stats <- tmp$arc.stats; arc.chars <- tmp$arc.chars
	
## identify filtered characters
#	idx.freq <- char.stats[,COL_FREQ]>3
#	tt <- table(unlist(inter.df))
#	idx.cooc <- tt[char.stats[,COL_NAME]]>1
#	idx <- which(idx.freq & idx.cooc)
#	char.stats[,COL_FILTER] <- rep("Discard",nrow(char.stats))
#	char.stats[idx,COL_FILTER] <- "Keep"
## cannot select the giant component without the graph >> do the filtering later
	
	# record the tables and lists
	write.corpus.data(
		char.det=char.det, 
		inter.df=inter.df,
		panel.stats=panel.stats, panel.chars=panel.chars,
		page.stats=page.stats, page.chars=page.chars,
		scene.stats=scene.stats, scene.chars=scene.chars,
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.chars=volume.chars, 
		arc.stats=arc.stats, arc.chars=arc.chars
	)
	
	# build result and return
	result <- list(
		inter.df=inter.df,										# interactions
		panel.stats=panel.stats, panel.chars=panel.chars,		# panels
		page.stats=page.stats, page.chars=page.chars,			# pages
		scene.stats=scene.stats, scene.chars=scene.chars,		# scenes
		char.stats=char.stats,									# characters 
		volume.stats=volume.stats, volume.chars=volume.chars,	# volumes 
		arc.stats=arc.stats, arc.chars=arc.chars				# arcs
	)
	return(result)
}
