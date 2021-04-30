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
#			- COL_VOLS_START_PAGE: starting page relatively to the volume.
#			- COL_VOLS_END_PAGE: ending page relatively to the volume.
#			- COL_VOLS_LENGTH: number of (effective) pages in the volume.
#			- COL_VOLS_TITLE: title of the volume.
###############################################################################
read.volume.table <- function(page.info)
{	tlog(2,"Trying to read the volume file (",VOLUME_FILE,")")
	
	# read the proper table
	volume.info <- read.csv(VOLUME_FILE, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	
	# nothing to do here, for now
	
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
	err.pg.idx <- which(page.info[,COL_PAGES_PAGE]<volume.info[vol.ids,COL_VOLS_START_PAGE]
		| page.info[,COL_PAGES_PAGE]>volume.info[vol.ids,COL_VOLS_END_PAGE])
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
# inter.df: dataframe containing all the character interactions.
#
# returns: a dataframe listing the character information, with the following columns:
#			- COL_VOLS_VOLUME: volume in the BD series.
#			- COL_VOLS_START_PAGE: starting page relatively to the volume.
#			- COL_VOLS_END_PAGE: ending page relatively to the volume.
#			- COL_VOLS_LENGTH: number of (effective) pages in the volume.
#			- COL_VOLS_TITLE: title of the volume.
###############################################################################
read.char.table <- function(inter.df)
{	if(file.exists(CHAR_FILE))
	{	tlog(2,"Trying to read the character file (",CHAR_FILE,")")
	
		# read the proper table
		char.info <- read.csv(CHAR_FILE, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
		table.chars <- char.info[,COL_CHAR_NAME]
		table.chars <- sort(table.chars)
		
		# check multiple name use
		x <- table(table.chars)
		pb.chars <- names(x)[x!=1]
		if(length(pb.chars)>0)
		{	msg <- paste0("ERROR: The following names are used multiple times in file \"",CHAR_FILE,"\": ",paste(pb.chars,collapse=","))
			tlog(3,msg)
			stop(msg)
		}
		
		# get the list of characters from the interactions
		inter.chars <- c(inter.df[,COL_INTER_FROM_CHAR],inter.df[,COL_INTER_TO_CHAR])
		inter.chars <- sort(unique(inter.chars))
		
		# check whether some characters miss from the table
		pb.chars <- setdiff(inter.chars,table.chars)
		if(length(pb.chars)>0)
		{	#cat(paste(pb.chars,collapse="\n"))
			msg <- paste0("ERROR: The following names are missing from file \"",CHAR_FILE,"\": ",paste(pb.chars,collapse=","))
			tlog(3,msg)
			stop(msg)
		}
		
		# check whether some characters miss from the interactions
		pb.chars <- setdiff(table.chars,inter.chars)
		if(length(pb.chars)>0)
		{	#cat(paste(pb.chars,collapse="\n"))
			msg <- paste0("WARNING: The following names are defined in file \"",CHAR_FILE,"\", but never interact: ",paste(pb.chars,collapse=","))
			tlog(3,msg)
			#warning(msg)
		}
		
		tlog(2,"Reading of the character file completed")
	}
	else
	{	tlog(2,"No character file found")
		
		# just get the character names from the interactions
		inter.chars <- c(inter.df[,c(COL_INTER_FROM_CHAR,COL_INTER_TO_CHAR)])
		inter.chars <- sort(unique(inter.chars))
		
		char.info <- data.frame(name=c("lkj","lkj","oiu","uyt"))
		cn <- c(COL_CHAR_NAME)
		colnames(char.info) <- cn
	}
	
	return(char.info)
}




###############################################################################
# Computes and records some statistics regarding the parsed corpus.
#
# volume.info: table describing the series volumes.
# page.info: table describing all the pages constituting the BD series.
# stats.scenes: previously computed scene statistics.
# char.scenes: information about the scenes.
###############################################################################
update.stats <- function(volume.info, page.info, stats.scenes, char.scenes)
{	tlog(2,"Computing corpus stats")
	pages.end.panel.ids <- c(
			page.info[2:nrow(page.info),COL_PAGES_START_PANEL_ID]-1,
			page.info[nrow(page.info),COL_PAGES_START_PANEL_ID]+page.info[nrow(page.info),COL_PAGES_PANELS]
		)
	
		
		
		
	##### panels stats
	tlog(3,"Computing panel stats")
	panel.nbr <- max(c(stats.scenes[,COL_STATS_START_PANEL_ID],stats.scenes[,COL_STATS_END_PANEL_ID]))
	
	# init stats table for panels
	stats.panels <- data.frame(
			character(panel.nbr), integer(panel.nbr), 
			integer(panel.nbr), integer(panel.nbr), 
			integer(panel.nbr), integer(panel.nbr),
			integer(panel.nbr),
			logical(panel.nbr), logical(panel.nbr), logical(panel.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(stats.panels) <- c(
			COL_STATS_VOLUME, COL_STATS_VOLUME_ID,
			COL_STATS_PAGE, COL_STATS_PAGE_ID,
			COL_STATS_PANEL, COL_STATS_PANEL_ID, 
			COL_STATS_CHARS, 
			COL_STATS_MATCH_START, COL_STATS_MATCH_END, COL_STATS_MATCH_BOTH
	)
	
	# compute the stats for each panel
	tlog(4,"Processing each panel separately")
	char.panels <- lapply(1:panel.nbr, function(x) c())
	for(p in 1:panel.nbr)
	{	tlog(5,"Processing panel id ",p,"/",panel.nbr)
		
		# find the scenes containing this panel
		ss <- which(stats.scenes[,COL_STATS_START_PANEL_ID]<=p & stats.scenes[,COL_STATS_END_PANEL_ID]>=p)
		cur.page.id <- which(page.info[,COL_PAGES_START_PANEL_ID]<=p & pages.end.panel.ids>=p)
		pos <- p - page.info[cur.page.id, COL_PAGES_START_PANEL_ID] + 1
		match.start <- pos==1
		match.end <- pos==page.info[cur.page.id,COL_PAGES_PANELS]
		for(s in ss)
		{	if(length(char.scenes[[s]])>0)
				char.panels[[p]] <- union(char.panels[[p]], char.scenes[[s]])
		}
		
		# update the table
		stats.panels[p, COL_STATS_VOLUME] <- stats.scenes[ss[1],COL_STATS_VOLUME]
		stats.panels[p, COL_STATS_VOLUME_ID] <- stats.scenes[ss[1],COL_STATS_VOLUME_ID]
		stats.panels[p, COL_STATS_PAGE] <- page.info[cur.page.id,COL_PAGES_PAGE]
		stats.panels[p, COL_STATS_PAGE_ID] <- cur.page.id
		stats.panels[p, COL_STATS_PANEL] <- pos
		stats.panels[p, COL_STATS_PANEL_ID] <- p
		stats.panels[p, COL_STATS_CHARS] <- length(char.panels[[p]])
		stats.panels[p, COL_STATS_MATCH_START] <- match.start
		stats.panels[p, COL_STATS_MATCH_END] <- match.end
		stats.panels[p, COL_STATS_MATCH_BOTH] <- match.start && match.end
	}
	
	# record stats
	tlog(4,"Recording in ",STATS_PANELS_FILE)
	write.csv(x=stats.panels, STATS_PANELS_FILE, row.names=FALSE)#, col.names=TRUE)
	
	# distributions of character numbers
	vals <- table(stats.panels[,COL_STATS_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_CHARS, COL_STATS_PANELS, "Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "panels_distrib_char_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "panels_distrib_char_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "panels_distrib_char_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Character number distribution over panels"
		xl <- "Number of characters by panel"
		h <- hist(
				stats.panels[,COL_STATS_CHARS],
				breaks=0:max(stats.panels[,COL_STATS_CHARS]),
				col="RED",
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
		)
#		x <- h$breaks[2:length(h$breaks)]
#		y <- h$counts
#		idx <- which(y>0)
#		x <- x[idx]
#		y <- y[idx]
#		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
		
	# distribution of panel positions
	vals <- c()
	vals["Both"] <- length(which(stats.panels[, COL_STATS_MATCH_BOTH]))
	vals["Starts page"] <- length(which(stats.panels[, COL_STATS_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(stats.panels[, COL_STATS_MATCH_END])) - vals["Both"]
	vals["None"] <- nrow(stats.panels) - vals["Both"] - vals["Starts page"] - vals["Ends page"]
	perc <- vals/sum(vals)*100
	df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
	colnames(df) <- c("Position","Frequency","Proportion")
	write.csv(x=df, file.path(STAT_CORPUS_FOLDER, "panels_distrib_positions.csv"), row.names=FALSE)#, col.names=TRUE)
	#
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "panels_distrib_positions.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "panels_distrib_positions.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		barplot(
			height=perc,
			main="Distribution of panel positions (%)",
			col="RED"
		)
	dev.off()
	
	
	
	
	##### page stats
	tlog(3,"Computing page stats")
	page.nbr <- max(c(stats.scenes[,COL_STATS_START_PAGE_ID],stats.scenes[,COL_STATS_END_PAGE_ID]))
	
	# list the characters by page
	tlog(3,"Listing the characters by page")
	char.pages <- lapply(1:page.nbr, function(x) c())
	for(s in 1:nrow(stats.scenes))
	{	tlog(5,"Processing scene id ",s,"/",nrow(stats.scenes))
		
		# find the pages containing the scene
		start.page.id <- stats.scenes[s,COL_STATS_START_PAGE_ID]
		end.page.id <- stats.scenes[s,COL_STATS_END_PAGE_ID]
		page.ids <- start.page.id:end.page.id
		for(page.id in page.ids)
		{	if(length(char.scenes[[s]])>0)
				char.pages[[page.id]] <- union(char.pages[[page.id]], char.scenes[[s]])
		}
	}
	
	# init stats table for pages
	stats.pages <- data.frame(
			character(page.nbr), integer(page.nbr),
			integer(page.nbr), integer(page.nbr),
			integer(page.nbr), 
			integer(page.nbr), 
			integer(page.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(stats.pages) <- c(
			COL_STATS_VOLUME, COL_STATS_VOLUME_ID,
			COL_STATS_PAGE, COL_STATS_PAGE_ID,
			COL_STATS_SCENES, 
			COL_STATS_PANELS, 
			COL_STATS_CHARS
	)
	
	# compute the stats for each page
	tlog(4,"Processing each page separately")
	for(p in 1:page.nbr)
	{	tlog(5,"Processing page id ",p,"/",page.nbr)
		
		# number of scenes overlapping the page
		scn <- length(which(
					stats.scenes[,COL_STATS_START_PAGE_ID]<=p
					& stats.scenes[,COL_STATS_END_PAGE_ID]>=p
				))
		
		stats.pages[p, COL_STATS_VOLUME] <- page.info[p,COL_PAGES_VOLUME]
		stats.pages[p, COL_STATS_VOLUME_ID] <- page.info[p,COL_PAGES_VOLUME_ID]
		stats.pages[p, COL_STATS_PAGE] <- page.info[p,COL_PAGES_PAGE]
		stats.pages[p, COL_STATS_PAGE_ID] <- p
		stats.pages[p, COL_STATS_SCENES] <- scn
		stats.pages[p, COL_STATS_PANELS] <- page.info[p,COL_PAGES_PANELS]
		stats.pages[p, COL_STATS_CHARS] <- length(char.pages[[p]])
	}
	
	# record stats
	tlog(4,"Recording in ",STATS_PAGES_FILE)
	write.csv(x=stats.pages, file=STATS_PAGES_FILE, row.names=FALSE)#, col.names=TRUE)
	
	# distributions of scene numbers
	vals <- table(stats.pages[,COL_STATS_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_SCENES, COL_STATS_PAGES,"Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "pages_distrib_scene_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "pages_distrib_scene_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "pages_distrib_scene_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Scene number distribution over pages"
		xl <- "Number of scenes by page"
		h <- hist(
				stats.pages[,COL_STATS_SCENES],
				breaks=0:max(stats.pages[,COL_STATS_SCENES]),
				col="RED",
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
		)
#		x <- h$breaks[2:length(h$breaks)]
#		y <- h$counts
#		idx <- which(y>0)
#		x <- x[idx]
#		y <- y[idx]
#		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
	# distributions of panel numbers
	vals <- table(stats.pages[,COL_STATS_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PANELS, COL_STATS_PAGES,"Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "pages_distrib_panel_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "pages_distrib_panel_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "pages_distrib_panel_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Panel number distribution over pages"
		xl <- "Number of panels by page"
		h <- hist(
				stats.pages[,COL_STATS_PANELS],
				breaks=0:max(stats.pages[,COL_STATS_PANELS]),
				col="RED",
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
		)
#		x <- h$breaks[2:length(h$breaks)]
#		y <- h$counts
#		idx <- which(y>0)
#		x <- x[idx]
#		y <- y[idx]
#		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
	# distributions of character numbers
	vals <- table(stats.pages[,COL_STATS_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_CHARS, COL_STATS_PAGES, "Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "pages_distrib_char_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "pages_distrib_char_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "pages_distrib_char_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Character number distribution over pages"
		xl <- "Number of characters by page"
		h <- hist(
				stats.pages[,COL_STATS_CHARS],
				breaks=0:max(stats.pages[,COL_STATS_CHARS]),
				col="RED",
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
		)
#		x <- h$breaks[2:length(h$breaks)]
#		y <- h$counts
#		idx <- which(y>0)
#		x <- x[idx]
#		y <- y[idx]
#		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
	
	
	
	##### scene stats
	tlog(3,"Computing scene stats")
	scene.nbr <- nrow(stats.scenes)
	# remove id columns
#	idx <- match(colnames(stats.scenes), c(
#					COL_STATS_VOLUME_ID, 
#					COL_STATS_START_PAGE_ID, COL_STATS_START_PANEL_ID, 
#					COL_STATS_END_PAGE_ID, COL_STATS_END_PANEL_ID))
#	stats.scenes <- stats.scenes[,-idx]
	
	# record scene stats
	tlog(4,"Recording in ",STATS_SCENES_FILE)
	write.csv(x=stats.scenes, file=STATS_SCENES_FILE, row.names=FALSE)#, col.names=TRUE)
	
	# distributions of panel numbers
	vals <- table(stats.scenes[,COL_STATS_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PANELS, COL_STATS_SCENES,"Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "scenes_distrib_panel_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "scenes_distrib_panel_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "scenes_distrib_panel_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Panel number distribution over scenes"
		xl <- "Number of panels by scene"
		h <- hist(
				stats.scenes[,COL_STATS_PANELS],
				breaks=0:max(stats.scenes[,COL_STATS_PANELS]),
#				col="RED",
#				xlab=xl,
#				main=ml,
#				freq=FALSE,
				plot=FALSE
		)
		x <- h$breaks[2:length(h$breaks)]
		y <- h$counts
		idx <- which(y>0)
		x <- x[idx]
		y <- y[idx]
		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
	# distributions of character numbers
	vals <- table(stats.scenes[,COL_STATS_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_CHARS, COL_STATS_SCENES, "Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "scenes_distrib_char_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "scenes_distrib_char_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "scenes_distrib_char_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Character number distribution over scenes"
		xl <- "Number of characters by scene"
		h <- hist(
				stats.scenes[,COL_STATS_CHARS],
				breaks=0:max(stats.scenes[,COL_STATS_CHARS]),
				col="RED",
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
		)
#		x <- h$breaks[2:length(h$breaks)]
#		y <- h$counts
#		idx <- which(y>0)
#		x <- x[idx]
#		y <- y[idx]
#		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
	# distributions of page numbers
	vals <- table(stats.scenes[,COL_STATS_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PANELS, COL_STATS_PAGES,"Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "scenes_distrib_page_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "scenes_distrib_page_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "scenes_distrib_page_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Page number distribution over scenes"
		xl <- "Number of pages by scene"
		h <- hist(
				stats.scenes[,COL_STATS_PAGES],
				breaks=0:max(stats.scenes[,COL_STATS_PAGES]),
				col="RED",
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
		)
		x <- h$breaks[2:length(h$breaks)]
		y <- h$counts
		idx <- which(y>0)
		x <- x[idx]
		y <- y[idx]
		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
	# distribution of scene positions
	vals <- c()
	vals["Both"] <- length(which(stats.scenes[, COL_STATS_MATCH_BOTH]))
	vals["Starts page"] <- length(which(stats.scenes[, COL_STATS_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(stats.scenes[, COL_STATS_MATCH_END])) - vals["Both"]
	vals["None"] <- nrow(stats.scenes) - vals["Both"] - vals["Starts page"] - vals["Ends page"]
	perc <- vals/sum(vals)*100
	df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
	colnames(df) <- c("Position","Frequency","Proportion")
	write.csv(x=df, file.path(STAT_CORPUS_FOLDER, "scenes_distrib_positions.csv"), row.names=FALSE)#, col.names=TRUE)
	#
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "scenes_distrib_positions.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "scenes_distrib_positions.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		barplot(
			height=perc,
			main="Distribution of scene positions (%)",
			col="RED"
		)
	dev.off()
	
	
	
	
	#### character stats
	tlog(3,"Computing character stats")
	unique.chars <- sort(unique(unlist(char.scenes)))
	char.nbr <- length(unique.chars)
	
	#  identify the characters in each volume
	tlog(4,"Identify the character in each volume")
	volume.nbr <- nrow(volume.info)
	char.volumes <- lapply(1:volume.nbr, function(x) c())
	for(v in 1:volume.nbr)
	{	tlog(5,"Processing volume id ",v,"/",nrow(volume.info))
		
		# find the pages contained in the volume
		idx.pg <- which(page.info[,COL_PAGES_VOLUME_ID]==v)
		for(p in idx.pg)
		{	if(length(char.pages[[p]])>0)
				char.volumes[[v]] <- union(char.volumes[[v]], char.pages[[p]])
		}
	}
	
	# init stats table for pages
	stats.chars <- data.frame(
			character(char.nbr),
			integer(char.nbr),
			integer(char.nbr),
			integer(char.nbr),
			integer(char.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(stats.chars) <- c(
			COL_STATS_CHAR, 
			COL_STATS_VOLUMES, 
			COL_STATS_PAGES, 
			COL_STATS_SCENES, 
			COL_STATS_PANELS
	)
	
	# compute the stats for each character
	tlog(4,"Processing each character separately")
	for(c in 1:char.nbr)
	{	tlog(5,"Processing character \"",unique.chars[c],"\" (",c,"/",char.nbr,")")
		
		stats.chars[c, COL_STATS_CHAR] <- unique.chars[c]
		stats.chars[c, COL_STATS_VOLUMES] <- sum(sapply(char.volumes, function(char.volume) unique.chars[c] %in% char.volume))
		stats.chars[c, COL_STATS_PAGES] <- sum(sapply(char.pages, function(char.page) unique.chars[c] %in% char.page))
		stats.chars[c, COL_STATS_SCENES] <- sum(sapply(char.scenes, function(char.scene) unique.chars[c] %in% char.scene))
		stats.chars[c, COL_STATS_PANELS] <- sum(sapply(char.panels, function(char.panel) unique.chars[c] %in% char.panel))
	}
	
	# record stats
	tlog(4,"Recording in ",STATS_CHARS_FILE)
	write.csv(x=stats.chars, file=STATS_CHARS_FILE, row.names=FALSE)#, col.names=TRUE)
	
	# distributions of volume numbers
	vals <- table(stats.chars[,COL_STATS_VOLUMES])
	vals <- data.frame(names(vals), vals, 100*vals/sum(vals), stringsAsFactors=FALSE, check.names=FALSE)
	colnames(vals) <- c(COL_STATS_VOLUMES, COL_STATS_CHARS,"Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "chars_distrib_volume_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "chars_distrib_volume_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "chars_distrib_volume_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Volume number distribution over characters"
		xl <- "Number of volumes by character"
		h <- hist(
				stats.chars[,COL_STATS_VOLUMES],
				breaks=0:max(stats.chars[,COL_STATS_VOLUMES]),
#				col="RED",
#				xlab=xl,
#				main=ml,
#				freq=FALSE,
				plot=FALSE
		)
		x <- h$breaks[2:length(h$breaks)]
		y <- h$counts
		idx <- which(y>0)
		x <- x[idx]
		y <- y[idx]
		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
	# distributions of page numbers
	vals <- table(stats.chars[,COL_STATS_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PAGES, COL_STATS_CHARS,"Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "chars_distrib_page_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "chars_distrib_page_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "chars_distrib_page_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Page number distribution over characters"
		xl <- "Number of pages by character"
		h <- hist(
				stats.chars[,COL_STATS_PAGES],
				breaks=0:max(stats.chars[,COL_STATS_PAGES]),
#				col="RED",
#				xlab=xl,
#				main=ml,
#				freq=FALSE,
				plot=FALSE
		)
		x <- h$breaks[2:length(h$breaks)]
		y <- h$counts
		idx <- which(y>0)
		x <- x[idx]
		y <- y[idx]
		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
	# distributions of scene numbers
	vals <- table(stats.chars[,COL_STATS_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_SCENES, COL_STATS_CHARS,"Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "chars_distrib_scene_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "chars_distrib_scene_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "chars_distrib_scene_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Scene number distribution over characters"
		xl <- "Number of scenes by character"
		h <- hist(
			stats.chars[,COL_STATS_SCENES],
			breaks=0:max(stats.chars[,COL_STATS_SCENES]),
#			col="RED",
#			xlab=xl,
#			main=ml,
#			freq=FALSE,
			plot=FALSE
		)
		x <- h$breaks[2:length(h$breaks)]
		y <- h$counts
		idx <- which(y>0)
		x <- x[idx]
		y <- y[idx]
		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
	# distributions of panel numbers
	vals <- table(stats.chars[,COL_STATS_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PANELS, COL_STATS_CHARS,"Proportion")
	write.csv(x=vals, file.path(STAT_CORPUS_FOLDER, "chars_distrib_panel_nbr.csv"), row.names=FALSE)#, col.names=TRUE)
	#pdf(file=file.path(STAT_CORPUS_FOLDER, "chars_distrib_panel_nbr.pdf"), bg="white")
	png(filename=file.path(STAT_CORPUS_FOLDER, "chars_distrib_panel_nbr.png"), width=800, height=800, units="px", pointsize=20, bg="white")
		ml <- "Panel number distribution over characters"
		xl <- "Number of panels by character"
		h <- hist(
				stats.chars[,COL_STATS_PANELS],
				breaks=0:max(stats.chars[,COL_STATS_PANELS]),
#				col="RED",
#				xlab=xl,
#				main=ml,
#				freq=FALSE,
				plot=FALSE
		)
		x <- h$breaks[2:length(h$breaks)]
		y <- h$counts
		idx <- which(y>0)
		x <- x[idx]
		y <- y[idx]
		plot(x, y, col="RED", xlab=xl, ylab="Density", main=ml, log="xy")
	dev.off()
	
		
	
	
	##### stats by volume
	tlog(3,"Computing volume stats")
	
	# init stats table for volume
	stats.volumes <- data.frame(
			character(volume.nbr), integer(volume.nbr),
			integer(volume.nbr), integer(volume.nbr), integer(volume.nbr), 
			integer(volume.nbr), integer(volume.nbr), integer(volume.nbr), 
			integer(volume.nbr), integer(volume.nbr), integer(volume.nbr), integer(volume.nbr), 
			integer(volume.nbr), integer(volume.nbr), integer(volume.nbr), integer(volume.nbr), 
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(stats.volumes) <- c(
			COL_STATS_VOLUME, COL_STATS_VOLUME_ID,
			COL_STATS_PAGES, COL_STATS_PAGES_BY_SCENE, COL_STATS_PAGES_BY_CHAR,
			COL_STATS_SCENES, COL_STATS_SCENES_BY_PAGE, COL_STATS_SCENES_BY_CHAR, 
			COL_STATS_PANELS, COL_STATS_PANELS_BY_PAGE, COL_STATS_PANELS_BY_SCENE, COL_STATS_PANELS_BY_CHAR, 
			COL_STATS_CHARS, COL_STATS_CHARS_BY_PAGE, COL_STATS_CHARS_BY_SCENE, COL_STATS_CHARS_BY_PANEL
	)
	
	# compute the stats for each volume
	tlog(4,"Processing each volume separately")
	for(v in 1:volume.nbr)
	{	tlog(5,"Processing volume id ",v,"/",nrow(volume.info))
		
		# corresponding pages
		idx.pg <- which(page.info[,COL_PAGES_VOLUME_ID]==v)
		pgn <- volume.info[v,COL_VOLS_END_PAGE] - volume.info[v,COL_VOLS_START_PAGE] + 1
		
		# corresponding characters
		char.volume <- c()
		for(i in idx.pg)
			char.volume <- union(char.volume, char.pages[[i]])
		chn <- length(char.volume)
		
		# corresponding scenes
		idx.sc <- which(stats.scenes[,COL_STATS_VOLUME_ID]==v)
		scn <- length(idx.sc)
		
		# corresponding panels
		idx.pn <- which(stats.panels[,COL_STATS_VOLUME_ID]==v)
		pnn <- sum(page.info[idx.pg,COL_PAGES_PANELS])
	
		# char stats
		char.volume.pgn <- rep(0,chn)
		names(char.volume.pgn) <- char.volume
		for(p in idx.pg)
		{	for(cc in char.pages[[p]])
				char.volume.pgn[cc] <- char.volume.pgn[cc] + 1
		}
		char.volume.scn <- rep(0,chn)
		names(char.volume.scn) <- char.volume
		for(s in idx.sc)
		{	for(cc in char.scenes[[s]])
				char.volume.scn[cc] <- char.volume.scn[cc] + 1
		}
		char.volume.pnl <- rep(0,chn)
		names(char.volume.pnl) <- char.volume
		for(p in idx.pn)
		{	for(cc in char.panels[[p]])
				char.volume.pnl[cc] <- char.volume.pnl[cc] + 1
		}
		
		# update stat table
		stats.volumes[v, COL_STATS_VOLUME] <- volume.info[v,COL_VOLS_VOLUME]
		stats.volumes[v, COL_STATS_VOLUME_ID] <- v
		#
		stats.volumes[v, COL_STATS_PAGES] <- pgn
		stats.volumes[v, COL_STATS_PAGES_BY_SCENE] <- sum(stats.scenes[idx.sc,COL_STATS_PAGES])/scn
		stats.volumes[v, COL_STATS_PAGES_BY_CHAR] <- sum(char.volume.pgn)/chn
		#
		stats.volumes[v, COL_STATS_SCENES] <- scn
		stats.volumes[v, COL_STATS_SCENES_BY_PAGE] <- sum(stats.pages[idx.pg,COL_STATS_SCENES])/pgn
		stats.volumes[v, COL_STATS_SCENES_BY_CHAR] <- sum(char.volume.scn)/chn
		#
		stats.volumes[v, COL_STATS_PANELS] <- pnn
		stats.volumes[v,COL_STATS_PANELS_BY_PAGE] <- pnn/pgn
		stats.volumes[v,COL_STATS_PANELS_BY_SCENE] <- sum(stats.scenes[idx.sc,COL_STATS_PANELS])/scn
		stats.volumes[v, COL_STATS_PANELS_BY_CHAR] <- sum(char.volume.pnl)/chn
		#
		stats.volumes[v, COL_STATS_CHARS] <- chn
		stats.volumes[v,COL_STATS_CHARS_BY_PAGE] <- sum(stats.pages[idx.pg,COL_STATS_CHARS])/pgn
		stats.volumes[v,COL_STATS_CHARS_BY_SCENE] <- sum(stats.scenes[idx.sc,COL_STATS_CHARS])/scn
		stats.volumes[v, COL_STATS_CHARS_BY_PANEL] <- sum(stats.panels[idx.pn,COL_STATS_CHARS])/pnn
	}
	
	# record stats
	tlog(4,"Recording in ",STATS_VOLUMES_FILE)
	write.csv(x=stats.volumes, file=STATS_VOLUMES_FILE, row.names=FALSE)#, col.names=TRUE)
	
	# evolution of the stats by volume
	vol.cols <- c(
		COL_STATS_PAGES, COL_STATS_PAGES_BY_SCENE, COL_STATS_PAGES_BY_CHAR,
		COL_STATS_SCENES, COL_STATS_SCENES_BY_PAGE, COL_STATS_SCENES_BY_CHAR, 
		COL_STATS_PANELS, COL_STATS_PANELS_BY_PAGE, COL_STATS_PANELS_BY_SCENE, COL_STATS_PANELS_BY_CHAR, 
		COL_STATS_CHARS, COL_STATS_CHARS_BY_PAGE, COL_STATS_CHARS_BY_SCENE, COL_STATS_CHARS_BY_PANEL
	)
	vol.titles <- c(
		"total number of pages", "average number of pages by scene", "average number of pages by character", 
		"total number of scenes", "average number of scenes by page", "average number of scenes by character", 
		"total number of panels", "average number of panels by page", "average number of panels by scene", "average number of panels by character", 
		"total number of characters", "average number of characters by page", "average number of characters by scene", "average number of characters by panel"
	)
	vol.fnames <- c(
		"num_pages", "num_pages_by_scene", "num_pages_by_char",
		"num_scenes", "num_scenes_by_page", "num_scenes_by_char",
		"num_panels", "num_panels_by_page", "num_panels_by_scene", "num_panels_by_char", 
		"num_chars", "num_chars_by_page", "num_chars_by_scene", "num_chars_by_panel"
	)
	tlog(4,"Generating the plots")
	for(m in 1:length(vol.cols))
	{	tlog(5,"Processing column \"",vol.cols[m],"\" (",m,"/",length(vol.cols),")")
		
		#pdf(file=file.path(STAT_CORPUS_FOLDER, "panels_distrib_positions.pdf"), bg="white")
		png(filename=file.path(STAT_CORPUS_FOLDER, paste0("volumes_evol_",vol.fnames[m],".png")), width=800, height=800, units="px", pointsize=20, bg="white")
			barplot(
				height=stats.volumes[,vol.cols[m]],
				names.arg=stats.volumes[,COL_STATS_VOLUME],
				main=paste0("Evolution of the ",vol.titles[m]),
				col="RED"
			)
		dev.off()
	}
	
	# TODO plot all volume distributions on the same plot?
	
	
	
	
	##### overall stats
	tlog(3,"Computing overall stats")
	stats.overall <- data.frame(
			integer(1), integer(1), 
			integer(1), integer(1), integer(1), integer(1), 
			integer(1), integer(1), integer(1), integer(1), 
			integer(1), integer(1), integer(1), integer(1), integer(1), 
			integer(1), integer(1), integer(1), integer(1), integer(1), 
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(stats.overall) <- c(
			COL_STATS_VOLUMES, COL_STATS_VOLUMES_BY_CHAR,
			COL_STATS_PAGES, COL_STATS_PAGES_BY_VOLUME, COL_STATS_PAGES_BY_SCENE, COL_STATS_PAGES_BY_CHAR,
			COL_STATS_SCENES, COL_STATS_SCENES_BY_VOLUME, COL_STATS_SCENES_BY_PAGE, COL_STATS_SCENES_BY_CHAR, 
			COL_STATS_PANELS, COL_STATS_PANELS_BY_VOLUME, COL_STATS_PANELS_BY_PAGE, COL_STATS_PANELS_BY_SCENE, COL_STATS_PANELS_BY_CHAR, 
			COL_STATS_CHARS, COL_STATS_CHARS_BY_VOLUME, COL_STATS_CHARS_BY_PAGE, COL_STATS_CHARS_BY_SCENE, COL_STATS_CHARS_BY_PANEL
	)
	
	# compute stats
	stats.overall[1,COL_STATS_VOLUMES] <- volume.nbr
	stats.overall[1,COL_STATS_VOLUMES_BY_CHAR] <- sum(stats.chars[,COL_STATS_VOLUMES])/char.nbr
	#
	stats.overall[1,COL_STATS_PAGES] <- page.nbr
	stats.overall[1,COL_STATS_PAGES_BY_VOLUME] <- sum(stats.volumes[,COL_STATS_PAGES])/volume.nbr
	stats.overall[1,COL_STATS_PAGES_BY_SCENE] <- sum(stats.scenes[,COL_STATS_PAGES])/scene.nbr
	stats.overall[1,COL_STATS_PAGES_BY_CHAR] <- sum(stats.chars[,COL_STATS_PAGES])/char.nbr
	#
	stats.overall[1,COL_STATS_SCENES] <- scene.nbr
	stats.overall[1,COL_STATS_SCENES_BY_VOLUME] <- sum(stats.volumes[,COL_STATS_SCENES])/volume.nbr
	stats.overall[1,COL_STATS_SCENES_BY_PAGE] <- sum(stats.pages[,COL_STATS_SCENES])/page.nbr
	stats.overall[1,COL_STATS_SCENES_BY_CHAR] <- sum(stats.chars[,COL_STATS_SCENES])/char.nbr
	#
	stats.overall[1,COL_STATS_PANELS] <- panel.nbr
	stats.overall[1,COL_STATS_PANELS_BY_VOLUME] <- panel.nbr/volume.nbr
	stats.overall[1,COL_STATS_PANELS_BY_PAGE] <- panel.nbr/page.nbr
	stats.overall[1,COL_STATS_PANELS_BY_SCENE] <- sum(stats.scenes[,COL_STATS_PANELS])/scene.nbr
	stats.overall[1,COL_STATS_PANELS_BY_CHAR] <- sum(stats.chars[,COL_STATS_PANELS])/char.nbr
	#
	stats.overall[1,COL_STATS_CHARS] <- char.nbr
	stats.overall[1,COL_STATS_CHARS_BY_VOLUME] <- sum(stats.volumes[,COL_STATS_CHARS])/volume.nbr
	stats.overall[1,COL_STATS_CHARS_BY_PAGE] <- sum(stats.pages[,COL_STATS_CHARS])/page.nbr
	stats.overall[1,COL_STATS_CHARS_BY_SCENE] <- sum(stats.scenes[,COL_STATS_CHARS])/scene.nbr
	stats.overall[1,COL_STATS_CHARS_BY_PANEL] <- sum(stats.panels[,COL_STATS_CHARS])/panel.nbr
	
	# record stats
	tlog(4,"Recording in ",STATS_OVERALL_FILE)
	write.csv(x=stats.overall, file=STATS_OVERALL_FILE, row.names=FALSE)#, col.names=TRUE)
	
	# TODO stats by narrative arc?

	# density plots
	xvals <- stats.scenes[,COL_STATS_CHARS]
	yvals <- stats.scenes[,COL_STATS_PANELS]
	xlab <- "Number of characters"
	ylab <- "Number of panels"
	p=ggplot(stats.scenes, aes(x=xvals, y=yvals)) +
		geom_hex(binwidth=2) + 
		coord_fixed() +
		scale_fill_viridis(begin=0.1, limits=c(0,NA),) + 
		theme_bw() +
		theme_classic() +	# base_size=18
		labs(fill="Count",x=xlab, y=ylab) +
		theme(legend.position="left") +
		geom_point(aes(x=xvals, y=yvals), alpha=0)
	ggMarginal(p, type="histogram", xparams=list(binwidth=2), yparams=list(binwidth=2), fill="#3a548c")

#	xvals <- stats.chars[,COL_STATS_SCENES]
#	yvals <- stats.chars[,COL_STATS_PANELS]
#	xlab <- "Number of scenes"
#	ylab <- "Number of panels"
#	p=ggplot(stats.chars, aes(x=xvals, y=yvals)) +
#		geom_hex(binwidth=100) + 
#		coord_fixed() +
#		scale_fill_viridis(begin=0.1, limits=c(0,NA),) + 
#		theme_bw() +
#		theme_classic() +	# base_size=18
#		labs(fill="Count",x=xlab, y=ylab) +
#		theme(legend.position="left") +
#		geom_point(aes(x=xvals, y=yvals), alpha=0)
#	ggMarginal(p, type="histogram", xparams=list(binwidth=100), yparams=list(binwidth=100), fill="#3a548c")
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
	char.info <- read.char.table(inter.df)
	
	# update stats
	update.stats(volume.info, page.info, stats.scenes, char.scenes)
	
	# build result and return
	result <- list(page.info=page.info, volume.info=volume.info, inter.df=inter.df, char.info=char.info)
	return(result)
}
