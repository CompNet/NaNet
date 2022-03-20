# This script contains functions used to compute some statistics and generate
# plots related to the corpus.
# 
# Vincent Labatut
# 05/2021
###############################################################################




###############################################################################
# Computes some additional statistics regarding the panels.
#
# panel.stats: table describing all the panels constituting the series.
# panel.chars: list of characters involved in each panel.
# char.stats: table describing all the characters.
#
# returns: a list of tables.
###############################################################################
compute.stats.panel <- function(
		panel.stats, panel.chars, 
		char.stats, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA)
{	object <- "panels"
	
	
	##################
	# attribute-independent stats
	tlog(4,"Computing attribute-independent panel stats")
	# specific volume
	if(!is.na(cur.vol))
	{	vname <- paste0(cur.vol,"_",volume.stats[cur.vol,COL_VOLUME])
		# keep only the panels of the current volume
		panel.idx <- which(panel.stats[,COL_VOLUME_ID]==cur.vol)
		panel.stats <- panel.stats[panel.idx,,drop=F]
	}
	# specific arc
	else if(!is.na(cur.arc))
	{	vname <- NA
		# keep only the panels of the current arc
		panel.idx <- which(panel.stats[,COL_ARC_ID]==cur.arc)
		panel.stats <- panel.stats[panel.idx,,drop=F]
	}
	if(!is.na(cur.vol || !is.na(cur.arc)))
	{	# record stats
		file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="_panel_stats")
		tlog(5,"Writing panel stats \"",file,"\"")
		write.csv(x=panel.stats, file=paste0(file,".csv"), row.names=FALSE)
		# record chars
		tab <- cbind(
			1:nrow(panel.stats),
			sapply(panel.chars[panel.idx], function(chars) paste(chars,collapse="\t"))
		)
		colnames(tab) <- c(COL_PANEL_ID, COL_CHARS)
		file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="_panel_chars")
		tlog(4,"Writing panel chars \"",file,"\"")
		write.table(tab, file=paste0(file,".txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	}
	# whole series
	else
	{	vname <- NA
		panel.idx <- 1:length(panel.chars)
	}
	
	
	##################
	# attribute-based stats
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	panel.stats.atts <- list()
	if(att.nbr==0)
	{	tlog(4,"No attribute: nothing else to compute")
	}
	else
	{	tlog(4,"Processing attribute-related panel stats")
		
		# loop over attributes
		tlog(5,"Loop over attributes")
		for(a in 1:att.nbr)
		{	att <- atts[a]
			tlog(6,"Processing attribute \"",att,"\" (",a,"/",att.nbr,")")
			
			# retrieve unique values
			uniq <- names(table(char.stats[,att]))	#, useNA="always"))
			# compute distribution for each panel
			mat <- t(sapply(panel.chars[panel.idx], function(chars) table(factor(char.stats[match(chars,char.stats[,COL_NAME]), att],levels=uniq))))
			
			# add panel id to matrix
			mat <- cbind(1:length(panel.idx), mat)
			colnames(mat)[1] <- COL_PANEL_ID
			# add matrix to result list
			panel.stats.atts[[att]] <- mat
			
			# record matrix
			file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="_panel_stats", att=att)
			tlog(7,"Creating file \"",file,"\"")
			write.csv(x=panel.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		}
	}
	
	result <- list(
		panel.stats=panel.stats, panel.stats.atts=panel.stats.atts, panel.chars=panel.chars[panel.idx]
	)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the pages.
#
# page.stats: table describing all the pages constituting the series.
# page.chars: list of characters involved in each page.
# char.stats: table describing all the characters.
#
# returns: a list of tables.
###############################################################################
compute.stats.page <- function(
		page.stats, page.chars, 
		char.stats, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA)
{	object <- "pages"
	
	
	##################
	# attribute-independent stats
	tlog(4,"Computing attribute-independent page stats")
	# specific volume
	if(!is.na(cur.vol))
	{	vname <- paste0(cur.vol,"_",volume.stats[cur.vol,COL_VOLUME])
		# keep only the pages of the current volume
		page.idx <- which(page.stats[,COL_VOLUME_ID]==cur.vol)
		page.stats <- page.stats[page.idx,,drop=F]
	}
	# specific arc
	else if(!is.na(cur.arc))
	{	vname <- NA
		# keep only the pages of the current arc
		page.idx <- which(page.stats[,COL_ARC_ID]==cur.arc)
		page.stats <- page.stats[page.idx,,drop=F]
	}
	if(!is.na(cur.vol || !is.na(cur.arc)))
	{	# record stats
		file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="_page_stats")
		tlog(5,"Writing page stats \"",file,"\"")
		write.csv(x=page.stats, file=paste0(file,".csv"), row.names=FALSE)
		# record chars
		tab <- cbind(
			1:nrow(page.stats),
			sapply(page.chars[page.idx], function(chars) paste(chars,collapse="\t"))
		)
		colnames(tab) <- c(COL_PAGE_ID, COL_CHARS)
		file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="_page_chars")
		tlog(4,"Writing page chars \"",file,"\"")
		write.table(tab, file=paste0(file,".txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	}
	# whole series
	else
	{	vname <- NA
		page.idx <- 1:length(page.chars)
	}
	
	##################
	# attribute-based stats
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	page.stats.atts <- list()
	if(att.nbr==0)
	{	tlog(4,"No attribute: nothing else to compute")
	}
	else
	{	tlog(4,"Processing attribute-related page stats")
		
		# loop over attributes
		tlog(5,"Loop over attributes")
		for(a in 1:att.nbr)
		{	att <- atts[a]
			tlog(6,"Processing attribute \"",att,"\" (",a,"/",att.nbr,")")
			
			# retrieve unique values
			uniq <- names(table(char.stats[,att]))	#, useNA="always"))
			# compute distribution for each page
			mat <- t(sapply(page.chars[page.idx], function(chars) table(factor(char.stats[match(chars,char.stats[,COL_NAME]), att],levels=uniq))))
			
			# add page id to matrix
			mat <- cbind(1:length(page.idx), mat)
			colnames(mat)[1] <- COL_PAGE_ID
			# add matrix to result list
			page.stats.atts[[att]] <- mat
			
			# record matrix
			file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="_page_stats", att=att)
			tlog(7,"Creating file \"",file,"\"")
			write.csv(x=page.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		}
	}
	
	result <- list(
		page.stats=page.stats, page.stats.atts=page.stats.atts, page.chars=page.chars[page.idx]
	)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the scenes.
#
# scene.stats: table describing all the scene constituting the series.
# scene.chars: list of characters involved in each scene.
# char.stats: table describing all the characters.
#
# returns: a list of tables.
###############################################################################
compute.stats.scene <- function(
		scene.stats, scene.chars, 
		char.stats, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA)
{	object <- "scenes"
	
	
	##################
	# attribute-independent stats
	tlog(4,"Computing attribute-independent scene stats")
	# specific volume
	if(!is.na(cur.vol))
	{	vname <- paste0(cur.vol,"_",volume.stats[cur.vol,COL_VOLUME])
		# keep only the scenes of the current volume
		scene.idx <- which(scene.stats[,COL_VOLUME_ID]==cur.vol)
		scene.stats <- scene.stats[scene.idx,,drop=F]
	}
	# specific arc
	else if(!is.na(cur.arc))
	{	vname <- NA
		# keep only the scenes of the current arc
		scene.idx <- which(scene.stats[,COL_ARC_ID]==cur.arc)
		scene.stats <- scene.stats[scene.idx,,drop=F]
	}
	if(!is.na(cur.vol || !is.na(cur.arc)))
	{	# record stats
		file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="_scene_stats")
		tlog(5,"Writing scene stats \"",file,"\"")
		write.csv(x=scene.stats, file=paste0(file,".csv"), row.names=FALSE)
		# record chars
		tab <- cbind(
			1:nrow(scene.stats),
			sapply(scene.chars[scene.idx], function(chars) paste(chars,collapse="\t"))
		)
		colnames(tab) <- c(COL_SCENE_ID, COL_CHARS)
		file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="_scene_chars")
		tlog(4,"Writing scene chars \"",file,"\"")
		write.table(tab, file=paste0(file,".txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	}
	# whole series
	else
	{	vname <- NA
		scene.idx <- 1:length(scene.chars)
	}
	
	
	##################
	# attribute-based stats
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	scene.stats.atts <- list()
	if(att.nbr==0)
	{	tlog(4,"No attribute: nothing else to compute")
	}
	else
	{	tlog(4,"Processing attribute-related scene stats")
		
		# loop over attributes
		tlog(5,"Loop over attributes")
		for(a in 1:att.nbr)
		{	att <- atts[a]
			tlog(6,"Processing attribute \"",att,"\" (",a,"/",att.nbr,")")
			
			# retrieve unique values
			uniq <- names(table(char.stats[,att]))	#, useNA="always"))
			# compute distribution for each scene
			mat <- t(sapply(scene.chars[scene.idx], function(chars) table(factor(char.stats[match(chars,char.stats[,COL_NAME]), att],levels=uniq))))
			
			# add scene id to matrix
			mat <- cbind(1:length(scene.idx), mat)
			colnames(mat)[1] <- COL_SCENE_ID
			# add matrix to result list
			scene.stats.atts[[att]] <- mat
			
			# record matrix
			file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="_scene_stats", att=att)
			tlog(7,"Creating file \"",file,"\"")
			write.csv(x=scene.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		}
	}
	
	result <- list(
		scene.stats=scene.stats, scene.stats.atts=scene.stats.atts, scene.chars=scene.chars[scene.idx]
	)
	return(result)
}




###############################################################################
# Plots the character statistics.
#
# char.stats: table describing all the characters.
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or id of the volume specifically processed.
# cur.arc: NA (regular mode) or id of the arc specifically processed.
# filtered: whether to consider all characters (FALSE) or only the important ones (TRUE).
###############################################################################
compute.stats.char <- function(
		panel.stats, panel.chars, 
		page.stats, page.chars, 
		scene.stats, scene.chars, 
		char.stats, 
		volume.stats, volume.chars, 
		arc.stats, arc.chars, 
		cur.vol=NA, cur.arc=NA)
{	object <- "characters"
	
	
	##################
	# attribute-independent stats
	tlog(4,"Computing attribute-independent character stats")
	# specific volume
	if(!is.na(cur.vol))
	{	vname <- paste0(cur.vol,"_",volume.stats[cur.vol,COL_VOLUME])
		# keep only the characters of the current volume
		char.idx <- match(volume.chars[[cur.vol]],char.stats[,COL_NAME])
		char.stats <- char.stats[char.idx,,drop=F]
		# update stats
		tt <- table(unlist(panel.chars[which(panel.stats[,COL_VOLUME_ID]==cur.vol)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PANELS] <- tt
		tt <- table(unlist(page.chars[which(page.stats[,COL_VOLUME_ID]==cur.vol)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PAGES] <- tt
		tt <- table(unlist(scene.chars[which(scene.stats[,COL_VOLUME_ID]==cur.vol)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_SCENES] <- tt
		char.stats[,COL_VOLUMES] <- rep(1, length(char.idx))
		char.stats[,COL_ARCS] <- rep(1, length(char.idx))
	}
	# specific arc
	else if(!is.na(cur.arc))
	{	vname <- NA
		# keep only the characters of the current arc
		char.idx <- match(arc.chars[[cur.arc]],char.stats[,COL_NAME])
		char.stats <- char.stats[char.idx,,drop=F]
		# update stats
		tt <- table(unlist(panel.chars[which(panel.stats[,COL_ARC_ID]==cur.arc)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PANELS] <- tt
		tt <- table(unlist(page.chars[which(page.stats[,COL_ARC_ID]==cur.arc)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PAGES] <- tt
		tt <- table(unlist(scene.chars[which(scene.stats[,COL_ARC_ID]==cur.arc)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_SCENES] <- tt
		tt <- table(unlist(volume.chars[which(volume.stats[,COL_ARC_ID]==cur.arc)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_VOLUMES] <- tt
		char.stats[,COL_ARCS] <- rep(1, length(char.idx))
	}
	if(!is.na(cur.vol || !is.na(cur.arc)))
	{	# record stats
		file <- get.path.stats.corpus(object=object, subfold="unfiltered", vol=vname, arc=cur.arc, pref="_char_stats")
		tlog(5,"Writing char stats \"",file,"\"")
		write.csv(x=char.stats, file=paste0(file,".csv"), row.names=FALSE)
	}
	# whole series
	else
	{	vname <- NA
		char.idx <- 1:length(panel.chars)
	}
	
	
	##################
	# attribute-based stats
	tlog(4,"No attribute-based stats to compute for characters")
	#atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	#att.nbr <- length(atts)
	# nothing to do here

	res <- list(
		char.stats=char.stats
	)
	return(res)
}

	
	
	
###############################################################################
# Computes and records some statistics regarding the volumes.
#
# char.stats: table describing all the characters.
# volume.stats: table describing the series volumes.
# volume.chars: list of characters involved in each volume.
#
# returns: a list of tables.
###############################################################################
compute.stats.volume <- function(
		panel.stats, panel.chars, 
		page.stats, page.chars, 
		scene.stats, scene.chars, 
		char.stats, 
		volume.stats, volume.chars)
{	object <- "volumes"
	
	##################
	# init
	volume.nbr <- nrow(volume.stats)
	volume.stats.indiv <- list()
	
	
	##################
	# attribute-independent stats
	tlog(4,"Computing attribute-independent volume stats")
	
	# complete volume stats table
	df <- data.frame(
		numeric(volume.nbr), numeric(volume.nbr), 
		numeric(volume.nbr), numeric(volume.nbr), 
		numeric(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
		numeric(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
		numeric(volume.nbr), numeric(volume.nbr), 
		stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(df) <- c(
		COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
		COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR,
		COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR,
		COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
		COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	volume.stats <- cbind(volume.stats, df)
	
	for(v in 1:volume.nbr)
	{	tlog(5,"Computing volume ",v,"/",volume.nbr)
		
		# panels
		tmp <- compute.stats.panel(panel.stats=panel.stats, panel.chars=panel.chars, char.stats=char.stats, volume.stats=volume.stats, cur.vol=v)
		vol.panel.stats <- tmp$panel.stats
		vol.panel.stats.atts <- tmp$panel.stats.atts
		vol.panel.chars <- tmp$panel.chars
			
		# pages
		tmp <- compute.stats.page(page.stats=page.stats, page.chars=page.chars, char.stats=char.stats, volume.stats=volume.stats, cur.vol=v)
		vol.page.stats <- tmp$page.stats
		vol.page.stats.atts <- tmp$page.stats.atts
		vol.page.chars <- tmp$page.chars
		
		# scenes
		tmp <- compute.stats.scene(scene.stats=scene.stats, scene.chars=scene.chars, char.stats=char.stats, volume.stats=volume.stats, cur.vol=v)
		vol.scene.stats <- tmp$scene.stats
		vol.scene.stats.atts <- tmp$scene.stats.atts
		vol.scene.chars <- tmp$scene.chars
		
		# character
		tmp <- compute.stats.char(panel.stats=panel.stats, panel.chars=panel.chars, page.stats=page.stats, page.chars=page.chars, scene.stats=scene.stats, scene.chars=scene.chars, char.stats=char.stats, volume.stats=volume.stats, volume.chars=volume.chars, arc.stats=arc.stats, arc.chars=arc.chars, cur.vol=v)
		vol.char.stats <- tmp$char.stats
		
		# add to result list
		volume.stats.indiv[[v]] <- list(
			vol.panel.stats=vol.panel.stats, vol.panel.stats.atts=vol.panel.stats.atts, vol.panel.chars=vol.panel.chars,
			vol.page.stats=vol.page.stats, vol.page.stats.atts=vol.page.stats.atts, vol.page.chars=vol.page.chars,
			vol.scene.stats=vol.scene.stats, vol.scene.stats.atts=vol.scene.stats.atts, vol.scene.chars=vol.scene.chars,
			vol.char.stats=vol.char.stats
		)
		
		# update means in volume stats table
		volume.stats[v,COL_PAGES_BY_SCENE]  <- sum(vol.scene.stats[,COL_PAGES]) /nrow(vol.scene.stats)	# mean number of pages by scene
		volume.stats[v,COL_PAGES_BY_CHAR]   <- sum(vol.char.stats [,COL_PAGES]) /nrow(vol.char.stats)	# mean number of pages by character
		volume.stats[v,COL_SCENES_BY_PAGE]  <- sum(vol.page.stats [,COL_SCENES])/nrow(vol.page.stats)	# mean number of scenes by page
		volume.stats[v,COL_SCENES_BY_CHAR]  <- sum(vol.char.stats [,COL_SCENES])/nrow(vol.char.stats)	# mean number of scenes by character
		volume.stats[v,COL_PANELS_BY_PAGE]  <- sum(vol.page.stats [,COL_PANELS])/nrow(vol.page.stats)	# mean number of panels by page
		volume.stats[v,COL_PANELS_BY_SCENE] <- sum(vol.scene.stats[,COL_PANELS])/nrow(vol.scene.stats)	# mean number of panels by scene
		volume.stats[v,COL_PANELS_BY_CHAR]  <- sum(vol.char.stats [,COL_PANELS])/nrow(vol.char.stats)	# mean number of panels by char
		volume.stats[v,COL_CHARS_BY_PAGE]   <- sum(vol.page.stats [,COL_CHARS]) /nrow(vol.page.stats)	# mean number of characters by page
		volume.stats[v,COL_CHARS_BY_SCENE]  <- sum(vol.scene.stats[,COL_CHARS]) /nrow(vol.scene.stats)	# mean number of characters by scene
		volume.stats[v,COL_CHARS_BY_PANEL]  <- sum(vol.panel.stats[,COL_CHARS]) /nrow(vol.panel.stats)	# mean number of characters by panel
		
		# update correlations in volume stats table
		volume.stats[v,COL_CORR_PANELS_CHARS_BY_SCENE] <- cor(vol.scene.stats[,COL_CHARS], vol.scene.stats[,COL_PANELS])	# panels by scene vs. characters by scene
		volume.stats[v,COL_CORR_SCENES_PANELS_BY_CHAR] <- cor(vol.char.stats [,COL_SCENES],vol.char.stats [,COL_PANELS])	# scenes by char vs. panels by char
		
		# record volume stat table
		file <- get.path.stats.corpus(object=object, pref="_volume_stats")
		tlog(4,"Writing volume stats \"",file,"\"")
		write.csv(x=volume.stats, file=paste0(file,".csv"), row.names=FALSE)
	}
	
	
	##################
	# attribute-based stats
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	volume.stats.atts <- list()
	if(att.nbr==0)
	{	tlog(4,"No attribute: nothing else to compute")
	}
	else
	{	tlog(4,"Processing attribute-related volume stats")
		
		# loop over attributes
		tlog(5,"Loop over attributes")
		for(a in 1:att.nbr)
		{	att <- atts[a]
			tlog(6,"Processing attribute \"",att,"\" (",a,"/",att.nbr,")")
			
			# retrieve unique values
			uniq <- names(table(char.stats[,att]))	#, useNA="always"))
			# compute distribution for each volume
			mat <- t(sapply(volume.chars, function(chars) table(factor(char.stats[match(chars,char.stats[,COL_NAME]), att],levels=uniq))))
			
			# add volume id to matrix
			mat <- cbind(1:volume.nbr, mat)
			colnames(mat)[1] <- COL_VOLUME_ID
			# add matrix to result list
			volume.stats.atts[[att]] <- mat
			
			# record matrix
			file <- get.path.stats.corpus(object=object, pref="_volume_stats", att=att)
			tlog(7,"Creating file \"",file,"\"")
			write.csv(x=volume.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		}
	}

	result <- list(
		volume.stats=volume.stats, volume.stats.atts=volume.stats.atts, volume.stats.indiv=volume.stats.indiv, volume.chars=volume.chars
	)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the narrative arcs.
#
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
# returns: an updated list of tables.
###############################################################################
compute.stats.arc <- function(
		panel.stats, panel.chars, 
		page.stats, page.chars, 
		scene.stats, scene.chars, 
		char.stats, 
		volume.stats, volume.stats.atts, volume.chars, 
		arc.stats, arc.chars)
{	object <- "arcs"
	
	##################
	# init
	arc.nbr <- nrow(arc.stats)
	arc.stats.indiv <- list()
	
	
	##################
	# attribute-independent stats
	tlog(4,"Computing attribute-independent arc stats")
	
	# complete arc stats table
	df <- data.frame(
		numeric(arc.nbr), 
		numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
		numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
		numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
		numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
		numeric(arc.nbr), numeric(arc.nbr), 
		stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(df) <- c(
		COL_VOLUMES_BY_CHAR,
		COL_PAGES_BY_VOLUME, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
		COL_SCENES_BY_VOLUME, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR,
		COL_PANELS_BY_VOLUME, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR,
		COL_CHARS_BY_VOLUME, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
		COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	arc.stats <- cbind(arc.stats, df)
	
	for(a in 1:arc.nbr)
	{	tlog(5,"Computing arc ",a,"/",arc.nbr)
		
		# panels
		tmp <- compute.stats.panel(panel.stats=panel.stats, panel.chars=panel.chars, char.stats=char.stats, volume.stats=volume.stats, cur.arc=a)
		arc.panel.stats <- tmp$panel.stats
		arc.panel.stats.atts <- tmp$panel.stats.atts
		arc.panel.chars <- tmp$panel.chars
		
		# pages
		tmp <- compute.stats.page(page.stats=page.stats, page.chars=page.chars, char.stats=char.stats, volume.stats=volume.stats, cur.arc=a)
		arc.page.stats <- tmp$page.stats
		arc.page.stats.atts <- tmp$page.stats.atts
		arc.page.chars <- tmp$page.chars
		
		# scenes
		tmp <- compute.stats.scene(scene.stats=scene.stats, scene.chars=scene.chars, char.stats=char.stats, volume.stats=volume.stats, cur.arc=a)
		arc.scene.stats <- tmp$scene.stats
		arc.scene.stats.atts <- tmp$scene.stats.atts
		arc.scene.chars <- tmp$scene.chars
		
		# character
		tmp <- compute.stats.char(panel.stats=panel.stats, panel.chars=panel.chars, page.stats=page.stats, page.chars=page.chars, scene.stats=scene.stats, scene.chars=scene.chars, char.stats=char.stats, volume.stats=volume.stats, volume.chars=volume.chars, arc.stats=arc.stats, arc.chars=arc.chars, cur.arc=a)
		arc.char.stats <- tmp$char.stats
		
		# volumes
#		tmp <- compute.stats.volume(panel.stats=panel.stats, panel.chars=panel.chars, page.stats=page.stats, page.chars=page.chars, scene.stats=scene.stats, scene.chars=scene.chars, char.stats=char.stats, volume.stats=volume.stats, cur.arc=a)
#		arc.volume.stats <- tmp$volume.stats
#		arc.volume.stats.atts <- tmp$volume.stats.atts
#		arc.volume.chars <- tmp$volume.chars
# we don't need to generate additional files	
		idx <- which(volume.stats[,COL_ARC_ID]==a)
		arc.volume.stats <- volume.stats[idx,,drop=F]
		arc.volume.stats.atts <- lapply(volume.stats.atts, function(mat) mat[idx,,drop=F]) 
		arc.volume.chars <- volume.chars[idx]
	
		# add to result list
		arc.stats.indiv[[a]] <- list(
			arc.panel.stats=arc.panel.stats, arc.panel.stats.atts=arc.panel.stats.atts, arc.panel.chars=arc.panel.chars,
			arc.page.stats=arc.page.stats, arc.page.stats.atts=arc.page.stats.atts, arc.page.chars=arc.page.chars,
			arc.scene.stats=arc.scene.stats, arc.scene.stats.atts=arc.scene.stats.atts, arc.scene.chars=arc.scene.chars,
			arc.char.stats=arc.char.stats,
			arc.volume.stats=arc.volume.stats, arc.volume.stats.atts=arc.volume.stats.atts, arc.volume.chars=arc.volume.chars
		)
		
		# update means in volume stats table
		arc.stats[a,COL_VOLUMES_BY_CHAR]  <- sum(arc.char.stats[,COL_VOLUMES]) /nrow(arc.char.stats)		# mean number of volumes by character
		arc.stats[a,COL_PAGES_BY_VOLUME]  <- sum(arc.volume.stats[,COL_PAGES]) /nrow(arc.volume.stats)		# mean number of pages by volume
		arc.stats[a,COL_PAGES_BY_SCENE]   <- sum(arc.scene.stats[,COL_PAGES])  /nrow(arc.scene.stats)		# mean number of pages by scene
		arc.stats[a,COL_PAGES_BY_CHAR]    <- sum(arc.char.stats [,COL_PAGES])  /nrow(arc.char.stats)		# mean number of pages by character
		arc.stats[a,COL_SCENES_BY_VOLUME] <- sum(arc.volume.stats[,COL_SCENES])/nrow(arc.volume.stats)		# mean number of scenes by volume
		arc.stats[a,COL_SCENES_BY_PAGE]   <- sum(arc.page.stats [,COL_SCENES]) /nrow(arc.page.stats)		# mean number of scenes by page
		arc.stats[a,COL_SCENES_BY_CHAR]   <- sum(arc.char.stats [,COL_SCENES]) /nrow(arc.char.stats)		# mean number of scenes by character
		arc.stats[a,COL_PANELS_BY_VOLUME] <- sum(arc.volume.stats[,COL_PANELS])/nrow(arc.volume.stats)		# mean number of panels by volume
		arc.stats[a,COL_PANELS_BY_PAGE]   <- sum(arc.page.stats [,COL_PANELS]) /nrow(arc.page.stats)		# mean number of panels by page
		arc.stats[a,COL_PANELS_BY_SCENE]  <- sum(arc.scene.stats[,COL_PANELS]) /nrow(arc.scene.stats)		# mean number of panels by scene
		arc.stats[a,COL_PANELS_BY_CHAR]   <- sum(arc.char.stats [,COL_PANELS]) /nrow(arc.char.stats)		# mean number of panels by char
		arc.stats[a,COL_CHARS_BY_VOLUME]  <- sum(arc.volume.stats[,COL_CHARS]) /nrow(arc.volume.stats)		# mean number of characters by volume
		arc.stats[a,COL_CHARS_BY_PAGE]    <- sum(arc.page.stats [,COL_CHARS])  /nrow(arc.page.stats)		# mean number of characters by page
		arc.stats[a,COL_CHARS_BY_SCENE]   <- sum(arc.scene.stats[,COL_CHARS])  /nrow(arc.scene.stats)		# mean number of characters by scene
		arc.stats[a,COL_CHARS_BY_PANEL]   <- sum(arc.panel.stats[,COL_CHARS])  /nrow(arc.panel.stats)		# mean number of characters by panel
		
		# update correlations in arc stats table
		arc.stats[a,COL_CORR_PANELS_CHARS_BY_SCENE] <- cor(arc.scene.stats[,COL_CHARS], arc.scene.stats[,COL_PANELS])	# panels by scene vs. characters by scene
		arc.stats[a,COL_CORR_SCENES_PANELS_BY_CHAR] <- cor(arc.char.stats [,COL_SCENES],arc.char.stats [,COL_PANELS])	# scenes by char vs. panels by char
		
		# record arc stat table
		file <- get.path.stats.corpus(object=object, pref="_arc_stats")
		tlog(4,"Writing arc stats \"",file,"\"")
		write.csv(x=arc.stats, file=paste0(file,".csv"), row.names=FALSE)
	}
	
	
	##################
	# attribute-based stats
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	arc.stats.atts <- list()
	if(att.nbr==0)
	{	tlog(4,"No attribute: nothing else to compute")
	}
	else
	{	tlog(4,"Processing attribute-related arc stats")
		
		# loop over attributes
		tlog(5,"Loop over attributes")
		for(a in 1:att.nbr)
		{	att <- atts[a]
			tlog(6,"Processing attribute \"",att,"\" (",a,"/",att.nbr,")")
			
			# retrieve unique values
			uniq <- names(table(char.stats[,att]))	#, useNA="always"))
			# compute distribution for each arc
			mat <- t(sapply(arc.chars, function(chars) table(factor(char.stats[match(chars,char.stats[,COL_NAME]), att],levels=uniq))))
			
			# add arc id to matrix
			mat <- cbind(1:arc.nbr, mat)
			colnames(mat)[1] <- COL_ARC_ID
			# add matrix to result list
			arc.stats.atts[[att]] <- mat
			
			# record matrix
			file <- get.path.stats.corpus(object=object, pref="_arc_stats", att=att)
			tlog(7,"Creating file \"",file,"\"")
			write.csv(x=arc.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		}
	}
	
	result <- list(
		arc.stats=arc.stats, arc.stats.atts=arc.stats.atts, arc.stats.indiv=arc.stats.indiv, arc.chars=arc.chars
	)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the series.
#
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
# returns: an updated list of tables.
###############################################################################
compute.stats.series <- function(
		panel.stats,  
		page.stats,  
		scene.stats,  
		char.stats, 
		volume.stats,  
		arc.stats)
{	object <- NA
	
	
	##################
	# attribute-independent stats
	tlog(3,"Computing attribute-independent series stats")
	panel.nbr <- nrow(panel.stats)
	page.nbr <- nrow(page.stats)
	scene.nbr <- nrow(scene.stats)
	char.nbr <- nrow(char.stats)
	volume.nbr <- nrow(volume.stats)
	arc.nbr <- nrow(arc.stats)
	
	# init stat table
	series.stats <- data.frame(
		integer(1), numeric(1), 
		integer(1), numeric(1), numeric(1), 
		integer(1), numeric(1), numeric(1), numeric(1), numeric(1), 
		integer(1), numeric(1), numeric(1), numeric(1), numeric(1), 
		integer(1), numeric(1), numeric(1), numeric(1), numeric(1), numeric(1), 
		integer(1), numeric(1), numeric(1), numeric(1), numeric(1), numeric(1), 
		numeric(1), numeric(1),
		stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(series.stats) <- c(
		COL_ARCS, COL_ARCS_BY_CHAR,
		COL_VOLUMES, COL_VOLUMES_BY_ARC, COL_VOLUMES_BY_CHAR,
		COL_PAGES, COL_PAGES_BY_ARC, COL_PAGES_BY_VOLUME, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
		COL_SCENES, COL_SCENES_BY_ARC, COL_SCENES_BY_VOLUME, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR, 
		COL_PANELS, COL_PANELS_BY_ARC, COL_PANELS_BY_VOLUME, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR, 
		COL_CHARS, COL_CHARS_BY_ARC, COL_CHARS_BY_VOLUME, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
		COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	
	# compute stats
	series.stats[1,COL_ARCS] <- arc.nbr
	series.stats[1,COL_ARCS_BY_CHAR] <- sum(char.stats[,COL_ARCS])/char.nbr
	#
	series.stats[1,COL_VOLUMES] <- volume.nbr
	series.stats[1,COL_VOLUMES_BY_ARC] <- sum(arc.stats[,COL_VOLUMES])/arc.nbr
	series.stats[1,COL_VOLUMES_BY_CHAR] <- sum(char.stats[,COL_VOLUMES])/char.nbr
	#
	series.stats[1,COL_PAGES] <- page.nbr
	series.stats[1,COL_PAGES_BY_ARC] <- sum(arc.stats[,COL_PAGES])/arc.nbr
	series.stats[1,COL_PAGES_BY_VOLUME] <- sum(volume.stats[,COL_PAGES])/volume.nbr
	series.stats[1,COL_PAGES_BY_SCENE] <- sum(scene.stats[,COL_PAGES])/scene.nbr
	series.stats[1,COL_PAGES_BY_CHAR] <- sum(char.stats[,COL_PAGES])/char.nbr
	#
	series.stats[1,COL_SCENES] <- scene.nbr
	series.stats[1,COL_SCENES_BY_ARC] <- sum(arc.stats[,COL_SCENES])/arc.nbr
	series.stats[1,COL_SCENES_BY_VOLUME] <- sum(volume.stats[,COL_SCENES])/volume.nbr
	series.stats[1,COL_SCENES_BY_PAGE] <- sum(page.stats[,COL_SCENES])/page.nbr
	series.stats[1,COL_SCENES_BY_CHAR] <- sum(char.stats[,COL_SCENES])/char.nbr
	#
	series.stats[1,COL_PANELS] <- panel.nbr
	series.stats[1,COL_PANELS_BY_ARC] <- sum(arc.stats[,COL_PANELS])/arc.nbr
	series.stats[1,COL_PANELS_BY_VOLUME] <- sum(volume.stats[,COL_PANELS])/volume.nbr
	series.stats[1,COL_PANELS_BY_PAGE] <- sum(page.stats[,COL_PANELS])/page.nbr
	series.stats[1,COL_PANELS_BY_SCENE] <- sum(scene.stats[,COL_PANELS])/scene.nbr
	series.stats[1,COL_PANELS_BY_CHAR] <- sum(char.stats[,COL_PANELS])/char.nbr
	#
	series.stats[1,COL_CHARS] <- char.nbr
	series.stats[1,COL_CHARS_BY_ARC] <- sum(arc.stats[,COL_CHARS])/arc.nbr
	series.stats[1,COL_CHARS_BY_VOLUME] <- sum(volume.stats[,COL_CHARS])/volume.nbr
	series.stats[1,COL_CHARS_BY_PAGE] <- sum(page.stats[,COL_CHARS])/page.nbr
	series.stats[1,COL_CHARS_BY_SCENE] <- sum(scene.stats[,COL_CHARS])/scene.nbr
	series.stats[1,COL_CHARS_BY_PANEL] <- sum(panel.stats[,COL_CHARS])/panel.nbr
	
	# compute correlations
	series.stats[1,COL_CORR_PANELS_CHARS_BY_SCENE] <- cor(scene.stats[,COL_CHARS],scene.stats[,COL_PANELS])
	series.stats[1,COL_CORR_SCENES_PANELS_BY_CHAR] <- cor(char.stats[,COL_SCENES],char.stats[,COL_PANELS])
	
	# record stats
	file <- get.path.stats.corpus(pref="_series_stats")
	tlog(4,"Recording in ",file)
	write.csv(x=series.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	
	
	##################
	# attribute-based stats
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	series.stats.atts <- list()
	if(att.nbr==0)
	{	tlog(4,"No attribute: nothing else to compute")
	}
	else
	{	tlog(4,"Processing attribute-related series stats")
		
		# loop over attributes
		tlog(5,"Loop over attributes")
		for(a in 1:att.nbr)
		{	att <- atts[a]
			tlog(6,"Processing attribute \"",att,"\" (",a,"/",att.nbr,")")
			
			# retrieve unique values
			uniq <- names(table(char.stats[,att]))	#, useNA="always"))
			# compute distribution for each arc
			mat <- t(as.matrix(table(factor(char.stats[,att],levels=uniq))))
			
			# add matrix to result list
			series.stats.atts[[att]] <- mat
			
#			# record matrix
#			file <- get.path.stats.corpus(pref="_series_stats", att=att)
#			tlog(7,"Creating file \"",file,"\"")
#			write.csv(x=series.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
# not needed: information already present in the characters folder
		}
	}
	
	result <- list(
		series.stats=series.stats, series.stats.atts=series.stats.atts
	)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the parsed corpus.
#
# data: previously read raw data.
#
# returns: same data, completed with stats.
###############################################################################
compute.corpus.stats <- function(data)
{	tlog(2,"Computing corpus stats")
	inter.df <- data$inter.df
	# panels
	panel.stats <- data$panel.stats
	panel.chars <- data$panel.chars
	# pages
	page.stats <- data$page.stats
	page.chars <- data$page.chars
	# scenes
	scene.stats <- data$scene.stats
	scene.chars <- data$scene.chars
	# chars
	char.stats <- data$char.stats
	# volumes
	volume.stats <- data$volume.stats
	volume.chars <- data$volume.chars 
	# arcs
	arc.stats <- data$arc.stats
	arc.chars <- data$arc.chars
		
	# complete panel stats
	tmp <- compute.stats.panel(
		panel.stats=panel.stats, panel.chars=panel.chars, 
		char.stats=char.stats,
		cur.vol=NA, cur.arc=NA
	)
	panel.stats.atts <- tmp$panel.stats.atts
	
	# complete page stats
	tmp <- compute.stats.page(
		page.stats=page.stats, page.chars=page.chars,
		char.stats=char.stats,
		cur.vol=NA, cur.arc=NA
	)
	page.stats.atts <- tmp$page.stats.atts
	
	# complete scene stats
	tmp <- compute.stats.scene(
		scene.stats=scene.stats, scene.chars=scene.chars,
		char.stats=char.stats, 
		cur.vol=NA, cur.arc=NA
	)
	scene.stats.atts <- tmp$scene.stats.atts
	
	# complete character stats
	tmp <- compute.stats.char(
		panel.stats=panel.stats, panel.chars=panel.chars, 
		page.stats=page.stats, page.chars=page.chars, 
		scene.stats=scene.stats, scene.chars=scene.chars, 
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.chars=volume.chars, 
		arc.stats=arc.stats, arc.chars=arc.chars, 
		cur.vol=NA, cur.arc=NA
	)
	
	# complete volume stats
	tmp <- compute.stats.volume(
		panel.stats=panel.stats, panel.chars=panel.chars, 
		page.stats=page.stats, page.chars=page.chars, 
		scene.stats=scene.stats, scene.chars=scene.chars, 
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.chars=volume.chars
	)
	volume.stats <- tmp$volume.stats
	volume.stats.atts <- tmp$volume.stats.atts
	volume.stats.indiv <- tmp$volume.stats.indiv
	
	# complete arc stats
	tmp <- compute.stats.arc(
		panel.stats=panel.stats, panel.chars=panel.chars,
		page.stats=page.stats, page.chars=page.chars,
		scene.stats=scene.stats, scene.chars=scene.chars,
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.stats.atts=volume.stats.atts, volume.chars=volume.chars, 
		arc.stats=arc.stats, arc.chars=arc.chars
	)
	arc.stats <- tmp$arc.stats
	arc.stats.atts <- tmp$arc.stats.atts
	arc.stats.indiv <- tmp$arc.stats.indiv
	
	# compute series stats
	tmp <- compute.stats.series(
		panel.stats=panel.stats, 
		page.stats=page.stats, 
		scene.stats=scene.stats, 
		char.stats=char.stats, 
		volume.stats=volume.stats,  
		arc.stats=arc.stats
	)
	series.stats <- tmp$series.stats
	series.stats.atts <- tmp$series.stats.atts
	
	# return all the stats
	result <- list(
		inter.df=inter.df,
		panel.stats=panel.stats, panel.stats.atts=panel.stats.atts, panel.chars=panel.chars,
		page.stats=page.stats, page.stats.atts=page.stats.atts, page.chars=page.chars,
		scene.stats=scene.stats, scene.stats.atts=scene.stats.atts, scene.chars=scene.chars,
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.stats.atts=volume.stats.atts, volume.stats.indiv=volume.stats.indiv, volume.chars=volume.chars,
		arc.stats=arc.stats, arc.stats.atts=arc.stats.atts, arc.stats.indiv=arc.stats.indiv, arc.chars=arc.chars,
		series.stats=series.stats, series.stats.atts=series.stats.atts
	)
	return(result)
}
