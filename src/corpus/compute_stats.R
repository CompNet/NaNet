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
compute.stats.panel <- function(panel.stats, panel.chars, char.stats, volume.stats, cur.vol=NA, cur.arc=NA)
{	object <- "panels"
	
	##################
	# init
	panel.nbr <- nrow(panel.stats)
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	
	
	##################
	# overall stats
	tlog(4,"Computing attribute-blind stats for for panels")
	# specific volume
	if(!is.na(cur.vol))
	{	vname <- volume.stats[cur.vol,COL_VOLUME]
		# keep only the panels of the current volume
		char.idx <- which(panel.stats[,COL_VOLUME_ID]==cur.vol)
		panel.stats <- panel.stats[char.idx,]
	}
	# specific arc
	else if(!is.na(cur.arc))
	{	# keep only the panels of the current arc
		char.idx <- which(panel.stats[,COL_ARC_ID]==cur.arc)
		panel.stats <- panel.stats[char.idx,]
	}
	if(!is.na(cur.vol || !is.na(cur.arc)))
	{	# record stats
		file <- get.path.stat.corpus(object=object, vol=vname, arc=arc, desc="_panel_stats")
		tlog(5,"Writing panel stats \"",file,"\"")
		write.csv(x=panel.stats, file=paste0(file,".csv"), row.names=FALSE)
		# record chars
		tab <- cbind(
			1:nrow(panel.stats),
			sapply(panel.chars, function(chars) paste(chars,collapse="\t"))
		)
		colnames(tab) <- c(COL_PANEL_ID, COL_CHARS)
		file <- get.path.stat.corpus(object=object, vol=vname, arc=arc, desc="_panel_chars")
		tlog(4,"Writing panel chars \"",file,"\"")
		write.table(tab, file=paste0(file,".txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	}
	# whole series
	else
		char.idx <- 1:length(panel.chars)
	
	
	##################
	# attribute-based stats
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
			mat <- t(sapply(panel.chars[char.idx], function(chars) table(factor(char.stats[match(chars,char.stats[,COL_NAME]), att],levels=uniq))))
			
			# add panel id to matrix
			mat <- cbind(1:panel.nbr, mat)
			colnames(mat)[1] <- COL_PANEL_ID
			# add matrix to result list
			panel.stats.atts[[att]] <- mat
			
			# record matrix
			file <- get.path.stat.corpus(object=object, desc="_panel_stats", att=att)
			tlog(7,"Creating file \"",file,"\"")
			write.csv(x=panel.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		}
	}
	
	result <- list(
		panel.stats=panel.stats, panel.stats.atts=panel.stats.atts, panel.chars=panel.chars
	)
	return(result)
}




###############################################################################
# Plots the panel statistics.
#
# panel.stats: table describing all the panels constituting the series.
# panel.stats.atts: panel stats for each attribute. 
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or id of the volume specifically processed.
# cur.arc: NA (regular mode) or id of the arc specifically processed.
###############################################################################
plot.stats.panel <- function(panel.stats, panel.stats.atts, volume.stats, cur.vol=NA, cur.arc=NA)
{	object <- "panels"
	
	# preparation
	if(!is.na(cur.vol))
	{	vname <- volume.stats[cur.vol, COL_VOLUME]
		tlog(3,"Computing panel stats (cur.vol=",cur.vol," vname=",vname,")")
		panel.idx <- which(panel.stats[,COL_VOLUME_ID]==cur.vol)		
	}
	else if(!is.na(cur.arc))
	{	vname <- NA
		tlog(3,"Computing panel stats (cur.arc=",cur.arc," title=",arc.stats[cur.arc,COL_TITLE],")")
		panel.idx <- which(panel.stats[,COL_ARC_ID]==cur.arc)		
	}
	else
	{	vname <- NA
		tlog(3,"Computing panel stats (whole series)")
		panel.idx <- panel.stats[,COL_PANEL_ID]
	}
	
	# panels
	panel.nbr <- length(panel.idx)
	col <- get.palette(1)
	# vertex attributes
	atts <- names(panel.stats.atts)
	att.nbr <- length(atts)
	
	
	##################
	# attribute-blind stats
	
	# distributions of character numbers (overall)
	vals <- table(panel.stats[panel.idx,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_PANELS, "Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_char_nbr")
	tlog(4,"Distribution of character numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- panel.stats[panel.idx,COL_CHARS]
	ml <- "Character number distribution over panels"
	xl <- "Number of characters by panel"
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			# histogram
			h <- hist(
				data,
				breaks=0:max(data),
				col=col,
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
			)
#			# scatterplot
#			x <- h$breaks[2:length(h$breaks)]
#			y <- h$counts
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	# distribution of panel positions
	vals <- c()
	vals["Both"] <- length(which(panel.stats[panel.idx, COL_MATCH_BOTH]))
	vals["Starts page"] <- length(which(panel.stats[panel.idx, COL_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(panel.stats[panel.idx, COL_MATCH_END])) - vals["Both"]
	vals["None"] <- panel.nbr - vals["Both"] - vals["Starts page"] - vals["Ends page"]
	perc <- vals/sum(vals)*100
	df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
	colnames(df) <- c("Position","Frequency","Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_positions")
	tlog(4,"Distribution of panel positions: Producing files \"",file,"\"")
	write.csv(x=df, paste0(file, ".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			barplot(
				height=perc,
				main="Distribution of panel positions (%)",
				col=col
			)
		dev.off()
	}
	
	
	##################
	# attribute-based stats
	if(att.nbr>0)
	{	# distribution of character numbers (by attribute)
		for(att in atts)
		{	data <- panel.stats.atts[[att]][panel.idx,-1]
			pal <- get.palette(ncol(data))
			file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_char_nbr", att=att)
			tlog(4,"Distribution of character numbers for attribute \"",att,"\": producing files \"",file,"\"")
			xl <- "Number of characters by panel"
			yl <- "Frequency"
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					ml <- paste0("Character number distribution over pages (att=",att)
					# scatterplot
#					vals <- matrix(NA,ncol=ncol(data),nrow=max(data))
#					for(d in 1:ncol(data))
#					{	h <- hist(
#							data[,d], 
#							breaks=0:max(data), 
#							plot=FALSE
#						)
#						vals[,d] <- h$counts
#					}
#					x <- h$breaks[2:length(h$breaks)]
#					x <- x[h$counts>0]
#					y <- h$counts[h$counts>0]
#					plot(
#						x,
#						y,
#						xlim=range(h$breaks),
#						ylim=range(vals[vals!=0]),
#						xlab=xl,
#						main=paste0(ml,")"),
#						col=pal[d],
#						log="y"
#					)
#					for(d in 2:ncol(data))
#					{	points(
#							h$breaks[2:length(h$breaks)],
#							h$counts,
#							col=pal[d],
#						)
#					}
					# barplots
					vals <- matrix(NA,ncol=ncol(data),nrow=max(data))
					for(d in 1:ncol(data))
					{	h <- hist(
							data[,d], 
							breaks=0:max(data), 
							plot=FALSE
						)
						vals[,d] <- h$counts
					}
					barplot(
						height=t(vals),
						names.arg=h$breaks[2:length(h$breaks)],
						xlab=xl,
						ylab=yl,
						main=paste0(ml,")"),
						col=pal,
						space=0,
						args.legend = list(x = "topright"),
						legend.text=colnames(data)
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 2:ncol(panel.stats.atts[[att]]))
			{	data <- panel.stats.atts[[att]][panel.idx,d]
				if(any(data!=0))
				{	val <- colnames(panel.stats.atts[[att]])[d]
					file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_char_nbr", att=att, val=val)
					tlog(5,"Distribution of character numbers for value \"",att,"\"=\"",val,"\": producing files \"",file,"\"")
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
							h <- hist(
								data,
								breaks=0:max(data),
								col=pal[d-1],
								xlab=xl,
								main=paste0(ml," val=",val,")"),
								freq=FALSE,
								#plot=FALSE
							)
						dev.off()
					}
				}
			}
		}
	}
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
compute.stats.page <- function(page.stats, page.chars, char.stats, volume.stats, cur.vol=NA, cur.arc=NA)
{	object <- "pages"
	
	
	##################
	# init
	page.nbr <- nrow(page.stats)
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	
	
	##################
	# overall stats
	tlog(4,"Computing attribute-blind stats for for pages")
	# specific volume
	if(!is.na(cur.vol))
	{	vname <- volume.stats[cur.vol,COL_VOLUME]
		
		# keep only the pages of the current volume
		char.idx <- which(page.stats[,COL_VOLUME_ID]==cur.vol)
		page.stats <- page.stats[char.idx,]
	}
	# specific arc
	else if(!is.na(cur.arc))
	{	# keep only the pages of the current arc
		char.idx <- which(page.stats[,COL_ARC_ID]==cur.arc)
		page.stats <- page.stats[char.idx,]
	}
	if(!is.na(cur.vol || !is.na(cur.arc)))
	{	# record stats
		file <- get.path.stat.corpus(object=object, vol=vname, arc=arc, desc="_page_stats")
		tlog(5,"Writing page stats \"",file,"\"")
		write.csv(x=page.stats, file=paste0(file,".csv"), row.names=FALSE)
		# record chars
		tab <- cbind(
			1:nrow(page.stats),
			sapply(page.chars, function(chars) paste(chars,collapse="\t"))
		)
		colnames(tab) <- c(COL_PAGE_ID, COL_CHARS)
		file <- get.path.stat.corpus(object=object, vol=vname, arc=arc, desc="_page_chars")
		tlog(4,"Writing page chars \"",file,"\"")
		write.table(tab, file=paste0(file,".txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	}
	# whole series
	else
		char.idx <- 1:length(page.chars)
	
	
	##################
	# attribute-based stats
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
			mat <- t(sapply(page.chars[char.idx], function(chars) table(factor(char.stats[match(chars,char.stats[,COL_NAME]), att],levels=uniq))))
			
			# add page id to matrix
			mat <- cbind(1:page.nbr, mat)
			colnames(mat)[1] <- COL_PAGE_ID
			# add matrix to result list
			page.stats.atts[[att]] <- mat
			
			# record matrix
			file <- get.path.stat.corpus(object=object, desc="_page_stats", att=att)
			tlog(7,"Creating file \"",f,"\"")
			write.csv(x=page.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		}
	}
	
	result <- list(
		page.stats=page.stats, page.stats.atts=page.stats.atts, page.chars=page.chars
	)
	return(result)
}




###############################################################################
# Plots the page statistics.
#
# page.stats: table describing all the pages constituting the series.
# page.stats.atts: page stats for each attribute. 
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or id of the volume specifically processed.
# cur.arc: NA (regular mode) or id of the arc specifically processed.
###############################################################################
plot.stats.page <- function(page.stats, page.stats.atts, volume.stats, cur.vol=NA, cur.arc=NA)
{	object <- "pages"
	
	# preparation
	if(!is.na(cur.vol))
	{	vname <- volume.stats[cur.vol, COL_VOLUME]
		tlog(3,"Computing page stats (cur.vol=",cur.vol," vname=",vname,")")
		page.idx <- which(page.stats[,COL_VOLUME_ID]==cur.vol)		
	}
	else if(!is.na(cur.arc))
	{	vname <- NA
		tlog(3,"Computing page stats (cur.arc=",cur.arc," title=",arc.stats[cur.arc,COL_TITLE],")")
		page.idx <- which(page.stats[,COL_ARC_ID]==cur.arc)		
	}
	else
	{	vname <- NA
		tlog(3,"Computing page stats (whole series)")
		page.idx <- page.stats[,COL_PAGE_ID]
	}
	
	# pages
	page.nbr <- length(page.idx)
	col <- get.palette(1)
	# vertex attributes
	atts <- names(page.stats.atts)
	att.nbr <- length(atts)
	
	
	##################
	# attribute-blind stats
	
	# distributions of scene numbers
	vals <- table(page.stats[page.idx,COL_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_SCENES, COL_PAGES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_scene_nbr")
	tlog(4,"Distribution of scene numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- page.stats[page.idx,COL_SCENES]
	ml <- "Scene number distribution over pages"
	xl <- "Number of scenes by page"
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			# histogram
			h <- hist(
				data,
				breaks=0:max(data),
				col=col,
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
			)
#			# scatterplot
#			x <- h$breaks[2:length(h$breaks)]
#			y <- h$counts
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	# distributions of panel numbers
	vals <- table(page.stats[page.idx,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_PAGES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_panel_nbr")
	tlog(4,"Distribution of panel numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- page.stats[page.idx,COL_PANELS]
	ml <- "Panel number distribution over pages"
	xl <- "Number of panels by page"
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			# histogram
			h <- hist(
				data,
				breaks=0:max(data),
				col=col,
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
			)
#			# scatterplot
#			x <- h$breaks[2:length(h$breaks)]
#			y <- h$counts
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	# distributions of character numbers (overall)
	vals <- table(page.stats[page.idx,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_PAGES, "Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_char_nbr")
	tlog(4,"Distribution of character numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- page.stats[page.idx,COL_CHARS]
	ml <- "Character number distribution over pages"
	xl <- "Number of characters by page"
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			# histogram
			h <- hist(
				data,
				breaks=0:max(data),
				col=col,
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
			)
#			# scatterplot
#			x <- h$breaks[2:length(h$breaks)]
#			y <- h$counts
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	
	##################
	# attribute-based stats
	if(att.nbr>0)
	{	# distribution of character numbers (by attribute)
		for(att in atts)
		{	data <- page.stats.atts[[att]][page.idx,-1]
			pal <- get.palette(ncol(data))
			file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_char_nbr", att=att)
			tlog(4,"Distribution of character numbers for attribute \"",att,"\": producing files \"",file,"\"")
			ml <- paste0("Character number distribution over pages (att=",att)
			xl <- "Number of characters by page"
			yl <- "Frequency"
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					# barplots
					vals <- matrix(NA,ncol=ncol(data),nrow=max(data))
					for(d in 1:ncol(data))
					{	h <- hist(
							data[,d], 
							breaks=0:max(data), 
							plot=FALSE
						)
						vals[,d] <- h$counts
					}
					barplot(
						height=t(vals),
						names.arg=h$breaks[2:length(h$breaks)],
						xlab=xl,
						ylab=yl,
						main=paste0(ml,")"),
						col=pal,
						space=0,
						args.legend = list(x = "topright"),
						legend.text=colnames(data)
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 2:ncol(page.stats.atts[[att]]))
			{	data <- page.stats.atts[[att]][page.idx,d]
				if(any(data!=0))
				{	val <- colnames(page.stats.atts[[att]])[d]
					file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_char_nbr", att=att, val=colnames(page.stats.atts[[att]])[d])
					tlog(5,"Distribution of character numbers for value \"",att,"\"=\"",val,"\": producing files \"",file,"\"")
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
							h <- hist(
								data,
								breaks=0:max(data),
								col=pal[d-1],
								xlab=xl,
								main=paste0(ml," - val=",val,")"),
								freq=FALSE,
								#plot=FALSE
							)
						dev.off()
					}
				}
			}
		}
	}
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
compute.stats.scene <- function(scene.stats, scene.chars, char.stats, volume.stats, cur.vol=NA, cur.arc=NA)
{	object <- "scenes"
	
	
	##################
	# init
	scene.nbr <- nrow(scene.stats)
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	
	
	##################
	# overall stats
	tlog(4,"Computing attribute-blind stats for for scenes")
	# specific volume
	if(!is.na(cur.vol))
	{	vname <- volume.stats[cur.vol,COL_VOLUME]
		# keep only the scenes of the current volume
		char.idx <- which(scene.stats[,COL_VOLUME_ID]==cur.vol)
		scene.stats <- scene.stats[char.idx,]
	}
	# specific arc
	else if(!is.na(cur.arc))
	{	# keep only the scenes of the current arc
		char.idx <- which(scene.stats[,COL_ARC_ID]==cur.arc)
		scene.stats <- scene.stats[char.idx,]
	}
	if(!is.na(cur.vol || !is.na(cur.arc)))
	{	# record stats
		file <- get.path.stat.corpus(object=object, vol=vname, arc=arc, desc="_scene_stats")
		tlog(5,"Writing scene stats \"",file,"\"")
		write.csv(x=scene.stats, file=paste0(file,".csv"), row.names=FALSE)
		# record chars
		tab <- cbind(
			1:nrow(scene.stats),
			sapply(scene.chars, function(chars) paste(chars,collapse="\t"))
		)
		colnames(tab) <- c(COL_SCENE_ID, COL_CHARS)
		file <- get.path.stat.corpus(object=object, vol=vname, arc=arc, desc="_scene_chars")
		tlog(4,"Writing scene chars \"",file,"\"")
		write.table(tab, file=paste0(file,".txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	}
	# whole series
	else
		char.idx <- 1:length(scene.chars)
	
	
	##################
	# attribute-based stats
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
			mat <- t(sapply(scene.chars[char.idx], function(chars) table(factor(char.stats[match(chars,char.stats[,COL_NAME]), att],levels=uniq))))
			
			# add scene id to matrix
			mat <- cbind(1:scene.nbr, mat)
			colnames(mat)[1] <- COL_SCENE_ID
			# add matrix to result list
			scene.stats.atts[[att]] <- mat
			
			# record matrix
			file <- get.path.stat.corpus(object=object, desc="_scene_stats", att=att)
			tlog(7,"Creating file \"",f,"\"")
			write.csv(x=scene.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		}
	}
	
	result <- list(
		scene.stats=scene.stats, scene.stats.atts=scene.stats.atts, scene.chars=scene.chars
	)
	return(result)
}




###############################################################################
# Plots the scene statistics.
#
# scene.stats: table describing all the scene constituting the series.
# page.stats.atts: scene stats for each attribute. 
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or id of the volume specifically processed.
# cur.arc: NA (regular mode) or id of the arc specifically processed.
###############################################################################
plot.stats.scene <- function(scene.stats, scene.stats.atts, volume.stats, cur.vol=NA, cur.arc=NA)
{	object <- "scenes"
	
	# preparation
	if(!is.na(cur.vol))
	{	vname <- volume.stats[cur.vol, COL_VOLUME]
		tlog(3,"Computing scene stats (cur.vol=",cur.vol," vname=",vname,")")
		scene.idx <- which(scene.stats[,COL_VOLUME_ID]==cur.vol)		
	}
	else if(!is.na(cur.arc))
	{	vname <- NA
		tlog(3,"Computing scene stats (cur.arc=",cur.arc," title=",arc.stats[cur.arc,COL_TITLE],")")
		scene.idx <- which(scene.stats[,COL_ARC_ID]==cur.arc)		
	}
	else
	{	vname <- NA
		tlog(3,"Computing scene stats (whole series)")
		scene.idx <- scene.stats[,COL_SCENE_ID]
	}
	
	# scenes
	scene.nbr <- length(scene.idx)
	col <- get.palette(1)
	# vertex attributes
	atts <- names(scene.stats.atts)
	att.nbr <- length(atts)
	
	
	##################
	# attribute-blind stats
	
	# distributions of panel numbers
	vals <- table(scene.stats[scene.idx,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_SCENES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_panel_nbr")
	tlog(4,"Distribution of panel numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- scene.stats[scene.idx,COL_PANELS]
	ml <- "Panel number distribution over scenes"
	xl <- "Number of panels by scene"
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
#				# histogram
#			h <- hist(
#					data,
#					breaks=0:max(data),
##					col=col,
##					xlab=xl,
##					main=ml,
##					freq=FALSE,
#					plot=FALSE
#			)
#			# scatterplot
#				x <- h$breaks[2:length(h$breaks)]
#			y <- h$density
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			expmax <- floor(log(min(y),10))
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy", yaxt="n") #las=1
#			axis(side=2, at=10^(expmax:0), label=parse(text=paste("10^", expmax:0, sep="")), las=1)
			# complementary cumulative distribution function
			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
	if(is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distributions of character numbers (overall)
	vals <- table(scene.stats[scene.idx,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_SCENES, "Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_char_nbr")
	tlog(4,"Distribution of character numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- scene.stats[scene.idx,COL_CHARS]
	ml <- "Character number distribution over scenes"
	xl <- "Number of characters by scene"
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			# histogram
			h <- hist(
					data,
					breaks=0:max(data),
					col=col,
					xlab=xl,
					main=ml,
					freq=FALSE,
#					plot=FALSE
			)
#			# scatterplot
#			x <- h$breaks[2:length(h$breaks)]
#			y <- h$counts
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	# distributions of page numbers
	vals <- table(scene.stats[scene.idx,COL_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_PAGES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_page_nbr")
	tlog(4,"Distribution of page numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- scene.stats[scene.idx,COL_PAGES]
	ml <- "Page number distribution over scenes"
	xl <- "Number of pages by scene"
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			# histogram
			h <- hist(
				data,
				breaks=0:max(data),
				col=col,
				xlab=xl,
				main=ml,
				freq=FALSE,
#				plot=FALSE
			)
#			# scatterplot
#			x <- h$breaks[2:length(h$breaks)]
#			y <- h$counts
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
	if(is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distribution of scene positions
	vals <- c()
	vals["Both"] <- length(which(scene.stats[scene.idx, COL_MATCH_BOTH]))
	vals["Starts page"] <- length(which(scene.stats[scene.idx, COL_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(scene.stats[scene.idx, COL_MATCH_END])) - vals["Both"]
	vals["None"] <- scene.nbr - vals["Both"] - vals["Starts page"] - vals["Ends page"]
	perc <- vals/sum(vals)*100
	df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
	colnames(df) <- c("Position","Frequency","Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_positions")
	tlog(4,"Distribution of scene positions: Producing files \"",file,"\"")
	write.csv(x=df, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			barplot(
				height=perc,
				main="Distribution of scene positions (%)",
				col=col
			)
		dev.off()
	}
	
	
	##################
	# attribute-based stats
	if(att.nbr>0)
	{	# distribution of character numbers (by attribute)
		for(att in atts)
		{	data <- scene.stats.atts[[att]][scene.idx,-1]
			pal <- get.palette(ncol(data))
			file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_char_nbr", att=att)
			tlog(4,"Distribution of character numbers for attribute \"",att,"\": producing files \"",file,"\"")
			ml <- paste0("Character number distribution over scenes (att=",att)
			xl <- "Number of characters by scene"
			yl <- "Frequency"
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					# barplots
					vals <- matrix(NA,ncol=ncol(data),nrow=max(data))
					for(d in 1:ncol(data))
					{	h <- hist(
							data[,d], 
							breaks=0:max(data), 
							plot=FALSE
						)
						vals[,d] <- h$counts
					}
					barplot(
						height=t(vals),
						names.arg=h$breaks[2:length(h$breaks)],
						xlab=xl,
						ylab=yl,
						main=paste0(ml,")"),
						col=pal,
						space=0,
						args.legend = list(x = "topright"),
						legend.text=colnames(data)
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 2:ncol(scene.stats.atts[[att]]))
			{	data <- scene.stats.atts[[att]][scene.idx,d]
				if(any(data!=0))
				{	val <- colnames(scene.stats.atts[[att]])[d]
					file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_char_nbr", att=att, val=colnames(scene.stats.atts[[att]])[d])
					tlog(5,"Distribution of character numbers for value \"",att,"\"=\"",val,"\": producing files \"",file,"\"")
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
							h <- hist(
								data,
								breaks=0:max(data),
								col=pal[d-1],
								xlab=xl,
								main=paste0(ml," - val=",val,")"),
								freq=FALSE,
								#plot=FALSE
							)
						dev.off()
					}
				}
			}
		}
	}
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
compute.stats.char <- function(panel.stats, panel.chars, page.stats, page.chars, scene.stats, scene.chars, char.stats, volume.stats, volume.chars, arc.stats, arc.chars, cur.vol=NA, cur.arc=NA)
{	object <- "characters"
	
	
	##################
	# init
	char.nbr <- nrow(char.stats)
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	
	
	##################
	# overall stats
	tlog(4,"Computing attribute-blind stats for for character")
	# specific volume
	if(!is.na(cur.vol))
	{	vname <- volume.stats[cur.vol,COL_VOLUME]
		# keep only the characters of the current volume
		char.idx <- match(volume.chars[[cur.vol]],char.stats[,COL_NAME])
		char.stats <- char.stats[char.idx,]
		# update stats
		tt <- table(unlist(panel.chars[which(panel.stats[,COL_VOLUME_ID]==cur.vol)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PANELS] <- tt
		tt <- table(unlist(page.chars[which(page.stats[,COL_VOLUME_ID]==cur.vol)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PAGES] <- tt
		tt <- table(unlist(scene.chars[which(scene.stats[,COL_VOLUME_ID]==cur.vol)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_SCENES] <- tt
		char.stats[,COL_VOLUMES] <- rep(1, length(idx))
		char.stats[,COL_ARCS] <- rep(1, length(idx))
	}
	# specific arc
	else if(!is.na(cur.arc))
	{	# keep only the characters of the current arc
		char.idx <- match(arc.chars[[cur.arc]],char.stats[,COL_NAME])
		char.stats <- char.stats[char.idx,]
		# update stats
		tt <- table(unlist(panel.chars[which(panel.stats[,COL_ARC_ID]==cur.arc)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PANELS] <- tt
		tt <- table(unlist(page.chars[which(page.stats[,COL_ARC_ID]==cur.arc)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_PAGES] <- tt
		tt <- table(unlist(scene.chars[which(scene.stats[,COL_ARC_ID]==cur.arc)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_SCENES] <- tt
		tt <- table(unlist(volume.chars[which(volume.stats[,COL_ARC_ID]==cur.arc)])); char.stats[match(names(tt),char.stats[,COL_NAME]),COL_VOLUMES] <- tt
		char.stats[,COL_ARCS] <- rep(1, length(idx))
	}
	if(!is.na(cur.vol || !is.na(cur.arc)))
	{	# record stats
		file <- get.path.stat.corpus(object=object, subfold="unfiltered", vol=vname, arc=arc, desc="_char_stats")
		tlog(5,"Writing char stats \"",file,"\"")
		write.csv(x=char.stats, file=paste0(file,".csv"), row.names=FALSE)
	}
	# whole series
	else
		char.idx <- 1:length(panel.chars)
	
	
	##################
	# attribute-based stats
	tlog(4,"No attribute-based stats to compute for characters")
	# nothing to do here
	
	res <- list(
		char.stats=char.stats
	)
	return(res)
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
plot.stats.char <- function(char.stats, volume.stats, cur.vol=NA, cur.arc=NA, filtered=FALSE)
{	object <- "characters"
	
	# char filtering
	if(filtered)
	{	filt.txt <- "filtered"
		char.filter <- char.stats[,COL_FILTERED]
		col <- get.palette(values=2)[2]
	}
	else
	{	filt.txt <- "unfiltered"
		char.filter <- rep(TRUE, nrow(char.stats))
		col <- get.palette(values=2)[1]
	}
	
	# preparation
	if(!is.na(cur.vol))
	{	vname <- volume.stats[cur.vol, COL_VOLUME]
		tlog(3,"Computing ",filt.txt," char stats (cur.vol=",cur.vol," vname=",vname,")")
		char.idx <- which(char.filter & char.stats[,COL_VOLUME_ID]==cur.vol)		
	}
	else if(!is.na(cur.arc))
	{	vname <- NA
		tlog(3,"Computing ",filt.txt," char stats (cur.arc=",cur.arc," title=",arc.stats[cur.arc,COL_TITLE],")")
		char.idx <- which(char.filter & char.stats[,COL_ARC_ID]==cur.arc)
	}
	else
	{	vname <- NA
		tlog(3,"Computing ",filt.txt," char stats (whole series)")
		char.idx <- which(char.filter)
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, desc="_char_stats")
	tlog(4,"Recording stats in ",file)
	write.csv(x=char.stats[char.idx,], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	
	# distribution of volumes by character
	if(is.na(cur.vol))
	{	vals <- table(char.stats[char.idx,COL_VOLUMES])
		vals <- data.frame(names(vals), vals, 100*vals/sum(vals), stringsAsFactors=FALSE, check.names=FALSE)
		colnames(vals) <- c(COL_VOLUMES, COL_CHARS,"Proportion")
		tlog(4,"Distribution of volume numbers: producing files \"",file,"\"")
		file <- get.path.stat.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, desc=paste0("chars_distrib_volume_nbr"))
		write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		#
		data <- char.stats[char.idx,COL_VOLUMES]
		if(length(unique(data))>1)
		{	ml <- paste0("Volume number distribution over",if(filtered) " filtered" else ""," characters")
			xl <- paste0("Number of volumes by",if(filtered) " filtered" else ""," character")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
#						# histogram
#						h <- hist(
#							data,
#							breaks=0:max(data),
##							col=col,
##							xlab=xl,
##							main=ml,
##							freq=FALSE,
#							plot=FALSE
#						)
#						# scatterplot
#						x <- h$breaks[2:length(h$breaks)]
#						y <- h$counts
#						idx <- which(y>0)
#						x <- x[idx]
#						y <- y[idx]
#						plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
						# complementary cumulative distribution function
						if(length(unique(data))>1)
							plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
					dev.off()
			}
			# check distribution
			if(length(unique(data))>1 && is.na(cur.arc))
			{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
				write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
			}
		}
	}
	
	# distribution of pages by character
	vals <- table(char.stats[char.idx,COL_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PAGES, COL_CHARS,"Proportion")
	tlog(4,"Distribution of page numbers: producing files \"",file,"\"")
	file <- get.path.stat.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, desc=paste0("chars_distrib_page_nbr"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[char.idx,COL_PAGES]
	ml <- paste0("Page number distribution over",if(filtered) " filtered" else ""," characters")
	xl <- paste0("Number of pages by",if(filtered) " filtered" else ""," character")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
#			# histogram
#			h <- hist(
#					data,
#					breaks=0:max(data),
##					col=col,
##					xlab=xl,
##					main=ml,
##					freq=FALSE,
#					plot=FALSE
#			)
#			# scatterplot
#			x <- h$breaks[2:length(h$breaks)]
#			y <- h$counts
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
	if(length(unique(data))>1 && is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distribution of scenes by character
	vals <- table(char.stats[char.idx,COL_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_SCENES, COL_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, desc=paste0("chars_distrib_scene_nbr"))
	tlog(4,"Distribution of scene numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[char.idx,COL_SCENES]
	ml <- paste0("Scene number distribution over",if(filtered) " filtered" else ""," characters")
	xl <- paste0("Number of scenes by",if(filtered) " filtered" else ""," character")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
#			# histogram
#			h <- hist(
#					data,
#				breaks=0:max(data),
##				col=col,
##				xlab=xl,
##				main=ml,
##				freq=FALSE,
#				plot=FALSE
#			)
#			x <- h$breaks[2:length(h$breaks)]
#			y <- h$counts
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
	if(length(unique(data))>1 && is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distribution of panels by character
	vals <- table(char.stats[char.idx,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, desc=paste0("chars_distrib_panel_nbr"))
	tlog(4,"Distribution of panel numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[char.idx,COL_PANELS]
	ml <- paste0("Panel number distribution over",if(filtered) " filtered" else ""," characters")
	xl <- paste0("Number of panels by",if(filtered) " filtered" else ""," character")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
#			# histogram
#			h <- hist(
#					data,
#					breaks=0:max(data),
##					col=col,
##					xlab=xl,
##					main=ml,
##					freq=FALSE,
#					plot=FALSE
#			)
#			# scatterplot
#			x <- h$breaks[2:length(h$breaks)]
#			y <- h$counts
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
	if(length(unique(data))>1 && is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# behavior of character filtering (trying to identify extras)
	if(!filtered)
	{	thresholds <- seq(0, 10)	#max(char.stats[,COL_FREQ]))
		char.nums <- sapply(thresholds, function(t) c(table(factor(char.stats[char.stats[,COL_FREQ]>=t,COL_NAMED], levels=c("TRUE","FALSE")))))
		# generate barplots
		file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="chars_filtering_by_occurences")
		tlog(4,"Behavior of character: producing files \"",file,"\"")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				cols <- c(col, combine.colors(col, "WHITE", transparency=40))
				barplot(
					height=char.nums,						# data
					names.arg=thresholds,					# bar names
					beside=FALSE,							# grouped bars
					col=cols,								# bar colors
					main=NA,								# no main title
					xlab="Minimal number of occurrences",	# x-axis label
					ylab="Number of characters",			# y-axis label
					las=0,									# x label orientation
				)
				legend(
					x="topright",
					fill=cols,
					legend=c("Named","Unnamed")
				)
			dev.off()
		}
		# record as CSV
		thresholds <- seq(0, 10)	#max(char.stats[,COL_FREQ]))
		char.nums <- sapply(thresholds, function(t) c(table(factor(char.stats[char.stats[,COL_FREQ]>=t,COL_NAMED], levels=c("TRUE","FALSE")))))
		tab <- cbind(thresholds, t(char.nums))
		colnames(tab) <- c("Min occurrences","Named","Unnamed")
		write.csv(x=tab, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
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
compute.stats.volume <- function(panel.stats, panel.chars, page.stats, page.chars, scene.stats, scene.chars, char.stats, volume.stats, volume.chars)
{	object <- "volumes"
	
	##################
	# init
	volume.nbr <- nrow(volume.stats)
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	volume.stats.indiv <- list()
	
	
	##################
	# overall stats
	tlog(4,"Computing attribute-blind volume stats")
	
	# complete volume stats table
	df <- data.frame(
		numeric(vol.nbr), numeric(vol.nbr), 
		numeric(vol.nbr), numeric(vol.nbr), 
		numeric(vol.nbr), numeric(vol.nbr), numeric(vol.nbr), 
		numeric(vol.nbr), numeric(vol.nbr), numeric(vol.nbr), 
		numeric(vol.nbr), numeric(vol.nbr), 
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
			vol.scene.stats=vol.scene.stats, vol.scene.stats.atts=vol.scene.stats.atts, vol.scene.chars=vol.scene.chars
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
	}
	
	# record volume stat table
	file <- get.path.stat.corpus(object="volumes",desc="_volume_stats")
	tlog(4,"Writing volume stats \"",file,"\"")
	write.csv(x=volume.stats, file=paste0(file,".csv"), row.names=FALSE)
	
	
	##################
	# attribute-based stats
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
			file <- get.path.stat.corpus(object=object, desc="_volume_stats", att=att)
			tlog(7,"Creating file \"",f,"\"")
			write.csv(x=volume.stats.atts[[att]], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		}
	}

	result <- list(
		volume.stats=volume.stats, volume.stats.atts=volume.stats.atts, volume.stats.indiv
	)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the volumes.
#
# volume.stats: table describing the series volumes.
# volume.chars: list of characters involved in each volume.
#
# returns: an updated list of tables.
###############################################################################
plot.stats.volume <- function(panel.stats, panel.stats.atts, page.stats, page.stats.atts, scene.stats, scene.stats.atts, char.stats, volume.stats, volume.stats.atts, volume.stats.indiv)
{	object <- "volumes"
	
	# volumes
	volume.nbr <- nrow(volume.stats)
	col <- get.palette(1)
	# vertex attributes
	atts <- names(volume.stats.atts)
	att.nbr <- length(atts)
	
	# plot the stats for each volume
	tlog(4,"Processing each volume separately")
	for(v in 1:volume.nbr)
	{	vname <- volume.stats[v,COL_VOLUME]
		tlog(5,"Processing volume ",vname," (",v,"/",nrow(volume.stats),")")
		
		
		##################
		# attribute-blind stats
		
		# plot volume-specific stats
		plot.stats.panel(panel.stats, panel.stats.atts, volume.stats, cur.vol=v, cur.arc=NA)
		plot.stats.page(page.stats, page.stats.atts, volume.stats, cur.vol=v, cur.arc=NA)
		plot.stats.scene(scene.stats, scene.stats.atts, volume.stats, cur.vol=v, cur.arc=NA)
		plot.stats.char(char.stats, char.stats.atts, volume.stats, cur.vol=v, cur.arc=NA)
		
		# volume-based selection
		idx.sc <- which(scene.stats[,COL_VOLUME_ID]==v)
		idx.char <- which(char.stats[,COL_VOLUME_ID]==v)
		
		# density plot: chars vs. panels (by scene)
		file <- get.path.stat.corpus(vol=vname, desc="comparison_chars-scenes_vs_panels-scenes")
		tlog(6,"Distribution of characters by scene vs. panels by scene: producing files \"",file,"\"")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				xvals <- scene.stats[idx.sc,COL_CHARS]
				yvals <- scene.stats[idx.sc,COL_PANELS]
				xlab <- "Number of characters by scene"
				ylab <- "Number of panels by scene"
				p=ggplot(scene.stats[idx.sc,], aes(x=xvals, y=yvals)) +
					geom_hex(binwidth=1) + 
					coord_fixed() +
					scale_fill_viridis(begin=0.1, limits=c(0,NA),) + 
					theme_bw() +
					theme_classic() +	# base_size=18
					labs(fill="Frequency",x=xlab, y=ylab) +
					theme(legend.position="left") +
					geom_point(aes(x=xvals, y=yvals), alpha=0)
				ggMarginal(p, type="histogram", xparams=list(binwidth=1), yparams=list(binwidth=1), fill=MAIN_COLOR)
				print(p)
			dev.off()
		}
		
		
		##################
		# attribute-based stats
		if(att.nbr>0)
		{	for(a in 1:length(atts))
			{	tlog(6,"Processing attribute ",atts[a]," (",a,"/",length(atts),")")
				
				# attribute distribution over the characters
				vals <- table(char.stats[idx.char,atts[a]])
				perc <- vals/sum(vals)*100
				df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
				colnames(df) <- c(atts[a],"Frequency","Proportion")
				file <- get.path.stat.corpus(vol=vname, desc="attr_distrib", att=atts[a])
				write.csv(x=df, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
				#
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						barplot(
							height=perc,
							main=paste0("Distribution of character attribute ",atts[a]," (%)"),
							col=col
						)
					dev.off()
				}
				
				# others?
			}
		}
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, desc="volumes")
	tlog(4,"Recording in ",file)
	write.csv(x=volume.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=volume.stats.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	# record the volumes for each character (for latter use)
	con <- file(paste0(file,"_chars.txt"), open="wt")
		sapply(volume.chars, function(lst) writeLines(paste(lst,collapse=","), con))
	close(con)
	
	# evolution of the stats by volume
	vol.cols <- c(
			COL_PAGES, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
			COL_SCENES, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR, 
			COL_PANELS, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR, 
			COL_CHARS, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
			COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	vol.titles <- c(
			"total number of pages", "average number of pages by scene", "average number of pages by character", 
			"total number of scenes", "average number of scenes by page", "average number of scenes by character", 
			"total number of panels", "average number of panels by page", "average number of panels by scene", "average number of panels by character", 
			"total number of characters", "average number of characters by page", "average number of characters by scene", "average number of characters by panel",
			"correlation between the numbers of characters and panels by scene", "correlation between the numbers of scenes and panels by character"
	)
	vol.fnames <- c(
			"num_pages", "num_pages_by_scene", "num_pages_by_char",
			"num_scenes", "num_scenes_by_page", "num_scenes_by_char",
			"num_panels", "num_panels_by_page", "num_panels_by_scene", "num_panels_by_char", 
			"num_chars", "num_chars_by_page", "num_chars_by_scene", "num_chars_by_panel",
			"corr_chars_panels_by_scene", "corr_scenes_panels_by_char"
	)
	tlog(4,"Generating the plots")
	for(v in 1:length(vol.cols))
	{	tlog(5,"Processing column \"",vol.cols[v],"\" (",v,"/",length(vol.cols),")")
		
		file <- get.path.stat.corpus(object=object, desc=vol.fnames[v])
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				barplot(
					height=volume.stats[,vol.cols[v]],
					names.arg=volume.stats[,COL_VOLUME],
					main=paste0("Evolution of the ",vol.titles[v]),
					col=col
				)
			dev.off()
		}
	}
	
	# distributions of character numbers
	vals <- table(volume.stats[, COL_CHARS])
	vals <- data.frame(names(vals), vals, 100*vals/sum(vals), stringsAsFactors=FALSE, check.names=FALSE)
	colnames(vals) <- c(COL_CHARS, COL_VOLUMES, "Proportion")
	file <- get.path.stat.corpus(object=object, desc="volumes_distrib_char_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- volume.stats[,COL_CHARS]
	if(length(unique(data))>1)
	{	ml <- "Character number distribution over volumes"
		xl <- "Number of characters by volume"
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
#					# histogram
#					h <- hist(
#						data,
#						breaks=0:max(data),
##						col=col,
##						xlab=xl,
##						main=ml,
##						freq=FALSE,
#						plot=FALSE
#					)
#					# scatterplot
#					x <- h$breaks[2:length(h$breaks)]
#					y <- h$counts
#					idx <- which(y>0)
#					x <- x[idx]
#					y <- y[idx]
#					plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy")
					# complementary cumulative distribution function
					plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
				dev.off()
		}
		# check distribution
		distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}
	
	# TODO plot the distributions obtained for all volumes on the same plot? (using lines instead of points)

	result <- list(
		volume.stats=volume.stats, volume.stats.atts=volume.stats.atts
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
		volume.stats, volume.chars, 
		arc.stats, arc.chars)
{	object <- "arcs"
	# vertex attributes
	atts <- setdiff(colnames(char.stats), c(COL_NAME, COL_NAME_SHORT, COL_FREQ))
	att.nbr <- length(atts)
	
	# compute stats
	tlog(3,"Computing arc stats")
	arc.titles <- unique(volume.stats[,COL_ARC])
	arc.nbr <- length(arc.titles)
	
	#  identify the characters in each arc
	tlog(4,"Identify the characters in each arc")
	arc.chars <- lapply(1:arc.nbr, function(x) c())
	for(a in 1:arc.nbr)
	{	tlog(5,"Processing arc ",a,"/",arc.nbr)
		
		# find the pages contained in the arc
		vols <- which(volume.stats[,COL_ARC]==arc.titles[a])
		idx.pg <- which(page.stats[,COL_VOLUME_ID] %in% vols)
		for(p in idx.pg)
		{	if(length(page.chars[[p]])>0)
				arc.chars[[a]] <- union(arc.chars[[a]], page.chars[[p]])
		}
	}
	
	# init stats table for arcs
	arc.stats <- data.frame(
			character(arc.nbr), integer(arc.nbr),
			integer(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			numeric(arc.nbr), numeric(arc.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(arc.stats) <- c(
			COL_ARC, COL_ARC_ID,
			COL_VOLUMES, COL_VOLUMES_BY_CHAR,
			COL_PAGES, COL_PAGES_BY_VOLUME, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
			COL_SCENES, COL_SCENES_BY_VOLUME, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR, 
			COL_PANELS, COL_PANELS_BY_VOLUME, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR, 
			COL_CHARS, COL_CHARS_BY_VOLUME, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
			COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	
	# init stats tables for attributes
	arc.stats.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.stats[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=arc.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			arc.stats.atts[[att]] <- m
		}
	}
	
	# compute the stats for each arc
	tlog(4,"Processing each arc separately")
	for(a in 1:arc.nbr)
	{	tlog(5,"Processing arc ",a,"/",arc.nbr)
		
		# corresponding volumes
		idx.vol <- which(volume.stats[,COL_ARC]==arc.titles[a])
		vln <- length(idx.vol)
		
		# corresponding pages
		idx.pg <- which(page.stats[,COL_VOLUME_ID] %in% idx.vol)
		pgn <- length(idx.pg)
		
		# corresponding characters
		char.arc <- c()
		for(i in idx.pg)
			char.arc <- union(char.arc, page.chars[[i]])
		idx.char <- match(char.arc, char.stats[,COL_NAME])
		chn <- length(char.arc)
		
		# corresponding scenes
		idx.sc <- which(scene.stats[,COL_VOLUME_ID] %in% idx.vol)
		scn <- length(idx.sc)
		
		# corresponding panels
		idx.pn <- which(panel.stats[,COL_VOLUME_ID] %in% idx.vol)
		pnn <- sum(page.stats[idx.pg,COL_PANELS])
		
		# char stats
		char.arc.vln <- rep(0,chn)
		names(char.arc.vln) <- char.arc
		for(v in idx.vol)
		{	for(cc in volume.chars[[v]])
				char.arc.vln[cc] <- char.arc.vln[cc] + 1
		}
		char.arc.pgn <- rep(0,chn)
		names(char.arc.pgn) <- char.arc
		for(p in idx.pg)
		{	for(cc in page.chars[[p]])
				char.arc.pgn[cc] <- char.arc.pgn[cc] + 1
		}
		char.arc.scn <- rep(0,chn)
		names(char.arc.scn) <- char.arc
		for(s in idx.sc)
		{	for(cc in scene.chars[[s]])
				char.arc.scn[cc] <- char.arc.scn[cc] + 1
		}
		char.arc.pnl <- rep(0,chn)
		names(char.arc.pnl) <- char.arc
		for(p in idx.pn)
		{	for(cc in panel.chars[[p]])
				char.arc.pnl[cc] <- char.arc.pnl[cc] + 1
		}
		
		# compute detailed stats
		arc.volume.stats <- volume.stats[idx.vol,]
		arc.page.stats <- page.stats[idx.pg,]
		arc.char.stats <- char.stats[idx.char,] 
		arc.scene.stats <- scene.stats[idx.sc,] 
		arc.scene.chars <- scene.chars[idx.sc]
		# compute panel stats
		tmp <- compute.stats.panels(
				page.stats=arc.page.stats, 
				scene.stats=arc.scene.stats, scene.chars=arc.scene.chars,
				char.stats=arc.char.stats, 
				volume.stats=arc.volume.stats, 
				cur.vol=NA, cur.arc=a
		)
		arc.panel.stats <- tmp$panel.stats
		arc.panel.stats.atts <- tmp$panel.stats.atts
		arc.panel.chars <- tmp$panel.chars
		# compute page stats
		tmp <- compute.stats.pages(
				panel.stats=arc.panel.stats, panel.chars=arc.panel.chars,
				page.stats=arc.page.stats, 
				scene.stats=arc.scene.stats, scene.chars=arc.scene.chars,
				char.stats=arc.char.stats,
				volume.stats=arc.volume.stats,  
				cur.vol=NA, cur.arc=a
		)
		arc.page.stats <- tmp$page.stats
		arc.page.stats.atts <- tmp$page.stats.atts
		arc.page.chars <- tmp$page.chars
		# compute scene stats
		tmp <- compute.stats.scenes(
				panel.stats=arc.panel.stats, panel.chars=arc.panel.chars,
				page.stats=arc.page.stats, page.chars=arc.page.chars, 
				scene.stats=arc.scene.stats, scene.chars=arc.scene.chars, 
				char.stats=arc.char.stats,
				volume.stats=arc.volume.stats, 
				cur.vol=NA, cur.arc=a
		)
		arc.scene.stats <- tmp$scene.stats
		arc.scene.stats.atts <- tmp$scene.stats.atts
		# compute character stats
		tmp <- compute.stats.chars(
				panel.stats=arc.panel.stats, panel.chars=arc.panel.chars,
				page.stats=arc.page.stats, page.chars=arc.page.chars, 
				scene.stats=arc.scene.stats, scene.chars=arc.scene.chars, 
				char.stats=arc.char.stats,
				volume.stats=arc.volume.stats,  
				cur.vol=NA, cur.arc=a
		)
		arc.char.stats <- tmp$char.stats
		arc.volume.chars <- tmp$volume.chars
		
		# update overall stat table
		arc.stats[a, COL_ARC] <- arc.titles[a]
		arc.stats[a, COL_ARC_ID] <- a
		#
		arc.stats[a, COL_VOLUMES] <- vln
		arc.stats[a, COL_VOLUMES_BY_CHAR] <- sum(char.arc.vln)/chn
		#
		arc.stats[a, COL_PAGES] <- pgn
		arc.stats[a, COL_PAGES_BY_VOLUME] <- sum(volume.stats[idx.vol,COL_PAGES])/vln
		arc.stats[a, COL_PAGES_BY_SCENE] <- sum(scene.stats[idx.sc,COL_PAGES])/scn
		arc.stats[a, COL_PAGES_BY_CHAR] <- sum(char.arc.pgn)/chn
		#
		arc.stats[a, COL_SCENES] <- scn
		arc.stats[a, COL_SCENES_BY_VOLUME] <- sum(volume.stats[idx.vol,COL_SCENES])/vln
		arc.stats[a, COL_SCENES_BY_PAGE] <- sum(page.stats[idx.pg,COL_SCENES])/pgn
		arc.stats[a, COL_SCENES_BY_CHAR] <- sum(char.arc.scn)/chn
		#
		arc.stats[a, COL_PANELS] <- pnn
		arc.stats[a, COL_PANELS_BY_VOLUME] <- sum(volume.stats[idx.vol,COL_PANELS])/vln
		arc.stats[a, COL_PANELS_BY_PAGE] <- sum(page.stats[idx.pg,COL_PANELS])/pgn
		arc.stats[a, COL_PANELS_BY_SCENE] <- sum(scene.stats[idx.sc,COL_PANELS])/scn
		arc.stats[a, COL_PANELS_BY_CHAR] <- sum(char.arc.pnl)/chn
		#
		arc.stats[a, COL_CHARS] <- chn
		arc.stats[a, COL_CHARS_BY_VOLUME] <- sum(volume.stats[idx.vol,COL_CHARS])/vln
		arc.stats[a, COL_CHARS_BY_PAGE] <- sum(page.stats[idx.pg,COL_CHARS])/pgn
		arc.stats[a, COL_CHARS_BY_SCENE] <- sum(scene.stats[idx.sc,COL_CHARS])/scn
		arc.stats[a, COL_CHARS_BY_PANEL] <- sum(panel.stats[idx.pn,COL_CHARS])/pnn
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(arc.chars[[a]], char.stats[,COL_NAME])
			for(att in atts)
			{	m <- arc.stats.atts[[att]]
				tt <- table(char.stats[idx,att])
				m[a,names(tt)] <- tt
				arc.stats.atts[[att]] <- m
			}
		}
		
		# density plot: chars vs. panels (by scene)
		file <- get.path.stat.corpus(arc=a, desc="comparison_chars-scenes_vs_panels-scenes")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				xvals <- scene.stats[idx.sc,COL_CHARS]
				yvals <- scene.stats[idx.sc,COL_PANELS]
				xlab <- "Number of characters by scene"
				ylab <- "Number of panels by scene"
				p=ggplot(scene.stats[idx.sc,], aes(x=xvals, y=yvals)) +
					geom_hex(binwidth=1) + 
					coord_fixed() +
					scale_fill_viridis(begin=0.1, limits=c(0,NA),) + 
					theme_bw() +
					theme_classic() +	# base_size=18
					labs(fill="Frequency",x=xlab, y=ylab) +
					theme(legend.position="left") +
					geom_point(aes(x=xvals, y=yvals), alpha=0)
				ggMarginal(p, type="histogram", xparams=list(binwidth=1), yparams=list(binwidth=1), fill=MAIN_COLOR)
				print(p)
			dev.off()
		}
		# correlation
		val <- cor(scene.stats[idx.sc,COL_CHARS],scene.stats[idx.sc,COL_PANELS])
		arc.stats[a, COL_CORR_PANELS_CHARS_BY_SCENE] <- val
		tlog(6,"Correlation between characters/scene and panels/scene: ", val)
		
		# density: scenes vs. panels (by char)
		val <- cor(char.stats[idx.char,COL_SCENES],char.stats[idx.char,COL_PANELS])
		arc.stats[a, COL_CORR_SCENES_PANELS_BY_CHAR] <- val
		tlog(6,"Correlation between scenes/character and panels/character: ", val)
		
		# attribute stats
		tlog(6,"Computing attribute stats")
		for(at in 1:length(atts))
		{	tlog(5,"Computing attribute ",atts[at]," (",at,"/",length(atts),")")
			
			# attribute distribution over the characters
			vals <- table(char.stats[idx.char,atts[at]])
			perc <- vals/sum(vals)*100
			df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
			colnames(df) <- c(atts[at],"Frequency","Proportion")
			file <- get.path.stat.corpus(arc=a, desc="attr_distrib_arc_", att=atts[at])
			write.csv(x=df, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
			#
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					barplot(
						height=perc,
						main=paste0("Distribution of character attribute ",atts[at]," (%)"),
						col=col
					)
				dev.off()
			}
			
			# others?
		}
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, desc="arcs")
	tlog(4,"Recording in ",file)
	write.csv(x=arc.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=arc.stats.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	
	# evolution of the stats by arc
	arc.cols <- c(
			COL_VOLUMES, COL_VOLUMES_BY_CHAR,
			COL_PAGES, COL_PAGES_BY_VOLUME, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
			COL_SCENES, COL_SCENES_BY_VOLUME, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR, 
			COL_PANELS, COL_PANELS_BY_VOLUME, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR, 
			COL_CHARS, COL_CHARS_BY_VOLUME, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
			COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	arc.ttls <- c(
			"total number of volumes", "average number of volumes by character", 
			"total number of pages", "average number of pages by volume", "average number of pages by scene", "average number of pages by character", 
			"total number of scenes", "average number of scenes by volume", "average number of scenes by page", "average number of scenes by character", 
			"total number of panels", "average number of panels by volume", "average number of panels by page", "average number of panels by scene", "average number of panels by character", 
			"total number of characters", "average number of characters by volume", "average number of characters by page", "average number of characters by scene", "average number of characters by panel",
			"correlation between the numbers of characters and panels by scene", "correlation between the numbers of scenes and panels by character"
	)
	arc.fnames <- c(
			"num_volumes", "num_volumes_by_char",
			"num_pages", "num_pages_by_volume", "num_pages_by_scene", "num_pages_by_char",
			"num_scenes", "num_scenes_by_volume", "num_scenes_by_page", "num_scenes_by_char",
			"num_panels", "num_panels_by_volume", "num_panels_by_page", "num_panels_by_scene", "num_panels_by_char", 
			"num_chars", "num_chars_by_volume", "num_chars_by_page", "num_chars_by_scene", "num_chars_by_panel",
			"corr_chars_panels_by_scene", "corr_scenes_panels_by_char"
	)
	tlog(4,"Generating the plots")
	for(a in 1:length(arc.cols))
	{	tlog(5,"Processing column \"",arc.cols[a],"\" (",a,"/",length(arc.cols),")")
		
		file <- get.path.stat.corpus(object=object, desc=arc.fnames[a])
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				barplot(
					height=arc.stats[,arc.cols[a]],
					names.arg=arc.stats[,COL_ARC],
					main=paste0("Evolution of the ",arc.ttls[a]),
					col=col
				)
			dev.off()
		}
	}
	
	# TODO plot all arc distributions on the same plot?
	
	result <- list(
		arc.stats=arc.stats, arc.stats.atts=arc.stats.atts, arc.chars=arc.chars
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
plot.stats.arc <- function(
		panel.stats, panel.chars, 
		page.stats, page.chars, 
		scene.stats, scene.chars, 
		char.stats, 
		volume.stats, volume.chars, 
		arc.stats, arc.chars)
{	object <- "arcs"
	# vertex attributes
	atts <- setdiff(colnames(char.stats), c(COL_NAME, COL_NAME_SHORT, COL_FREQ))
	att.nbr <- length(atts)
	
	# compute stats
	tlog(3,"Computing arc stats")
	arc.titles <- unique(volume.stats[,COL_ARC])
	arc.nbr <- length(arc.titles)
	
	#  identify the characters in each arc
	tlog(4,"Identify the characters in each arc")
	arc.chars <- lapply(1:arc.nbr, function(x) c())
	for(a in 1:arc.nbr)
	{	tlog(5,"Processing arc ",a,"/",arc.nbr)
		
		# find the pages contained in the arc
		vols <- which(volume.stats[,COL_ARC]==arc.titles[a])
		idx.pg <- which(page.stats[,COL_VOLUME_ID] %in% vols)
		for(p in idx.pg)
		{	if(length(page.chars[[p]])>0)
				arc.chars[[a]] <- union(arc.chars[[a]], page.chars[[p]])
		}
	}
	
	# init stats table for arcs
	arc.stats <- data.frame(
			character(arc.nbr), integer(arc.nbr),
			integer(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			numeric(arc.nbr), numeric(arc.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(arc.stats) <- c(
			COL_ARC, COL_ARC_ID,
			COL_VOLUMES, COL_VOLUMES_BY_CHAR,
			COL_PAGES, COL_PAGES_BY_VOLUME, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
			COL_SCENES, COL_SCENES_BY_VOLUME, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR, 
			COL_PANELS, COL_PANELS_BY_VOLUME, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR, 
			COL_CHARS, COL_CHARS_BY_VOLUME, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
			COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	
	# init stats tables for attributes
	arc.stats.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.stats[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=arc.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			arc.stats.atts[[att]] <- m
		}
	}
	
	# compute the stats for each arc
	tlog(4,"Processing each arc separately")
	for(a in 1:arc.nbr)
	{	tlog(5,"Processing arc ",a,"/",arc.nbr)
		
		# corresponding volumes
		idx.vol <- which(volume.stats[,COL_ARC]==arc.titles[a])
		vln <- length(idx.vol)
		
		# corresponding pages
		idx.pg <- which(page.stats[,COL_VOLUME_ID] %in% idx.vol)
		pgn <- length(idx.pg)
		
		# corresponding characters
		char.arc <- c()
		for(i in idx.pg)
			char.arc <- union(char.arc, page.chars[[i]])
		idx.char <- match(char.arc, char.stats[,COL_NAME])
		chn <- length(char.arc)
		
		# corresponding scenes
		idx.sc <- which(scene.stats[,COL_VOLUME_ID] %in% idx.vol)
		scn <- length(idx.sc)
		
		# corresponding panels
		idx.pn <- which(panel.stats[,COL_VOLUME_ID] %in% idx.vol)
		pnn <- sum(page.stats[idx.pg,COL_PANELS])
		
		# char stats
		char.arc.vln <- rep(0,chn)
		names(char.arc.vln) <- char.arc
		for(v in idx.vol)
		{	for(cc in volume.chars[[v]])
				char.arc.vln[cc] <- char.arc.vln[cc] + 1
		}
		char.arc.pgn <- rep(0,chn)
		names(char.arc.pgn) <- char.arc
		for(p in idx.pg)
		{	for(cc in page.chars[[p]])
				char.arc.pgn[cc] <- char.arc.pgn[cc] + 1
		}
		char.arc.scn <- rep(0,chn)
		names(char.arc.scn) <- char.arc
		for(s in idx.sc)
		{	for(cc in scene.chars[[s]])
				char.arc.scn[cc] <- char.arc.scn[cc] + 1
		}
		char.arc.pnl <- rep(0,chn)
		names(char.arc.pnl) <- char.arc
		for(p in idx.pn)
		{	for(cc in panel.chars[[p]])
				char.arc.pnl[cc] <- char.arc.pnl[cc] + 1
		}
		
		# compute detailed stats
		arc.volume.stats <- volume.stats[idx.vol,]
		arc.page.stats <- page.stats[idx.pg,]
		arc.char.stats <- char.stats[idx.char,] 
		arc.scene.stats <- scene.stats[idx.sc,] 
		arc.scene.chars <- scene.chars[idx.sc]
		# compute panel stats
		tmp <- compute.stats.panels(
				page.stats=arc.page.stats, 
				scene.stats=arc.scene.stats, scene.chars=arc.scene.chars,
				char.stats=arc.char.stats, 
				volume.stats=arc.volume.stats, 
				cur.vol=NA, cur.arc=a
		)
		arc.panel.stats <- tmp$panel.stats
		arc.panel.stats.atts <- tmp$panel.stats.atts
		arc.panel.chars <- tmp$panel.chars
		# compute page stats
		tmp <- compute.stats.pages(
				panel.stats=arc.panel.stats, panel.chars=arc.panel.chars,
				page.stats=arc.page.stats, 
				scene.stats=arc.scene.stats, scene.chars=arc.scene.chars,
				char.stats=arc.char.stats,
				volume.stats=arc.volume.stats,  
				cur.vol=NA, cur.arc=a
		)
		arc.page.stats <- tmp$page.stats
		arc.page.stats.atts <- tmp$page.stats.atts
		arc.page.chars <- tmp$page.chars
		# compute scene stats
		tmp <- compute.stats.scenes(
				panel.stats=arc.panel.stats, panel.chars=arc.panel.chars,
				page.stats=arc.page.stats, page.chars=arc.page.chars, 
				scene.stats=arc.scene.stats, scene.chars=arc.scene.chars, 
				char.stats=arc.char.stats,
				volume.stats=arc.volume.stats, 
				cur.vol=NA, cur.arc=a
		)
		arc.scene.stats <- tmp$scene.stats
		arc.scene.stats.atts <- tmp$scene.stats.atts
		# compute character stats
		tmp <- compute.stats.chars(
				panel.stats=arc.panel.stats, panel.chars=arc.panel.chars,
				page.stats=arc.page.stats, page.chars=arc.page.chars, 
				scene.stats=arc.scene.stats, scene.chars=arc.scene.chars, 
				char.stats=arc.char.stats,
				volume.stats=arc.volume.stats,  
				cur.vol=NA, cur.arc=a
		)
		arc.char.stats <- tmp$char.stats
		arc.volume.chars <- tmp$volume.chars
		
		# update overall stat table
		arc.stats[a, COL_ARC] <- arc.titles[a]
		arc.stats[a, COL_ARC_ID] <- a
		#
		arc.stats[a, COL_VOLUMES] <- vln
		arc.stats[a, COL_VOLUMES_BY_CHAR] <- sum(char.arc.vln)/chn
		#
		arc.stats[a, COL_PAGES] <- pgn
		arc.stats[a, COL_PAGES_BY_VOLUME] <- sum(volume.stats[idx.vol,COL_PAGES])/vln
		arc.stats[a, COL_PAGES_BY_SCENE] <- sum(scene.stats[idx.sc,COL_PAGES])/scn
		arc.stats[a, COL_PAGES_BY_CHAR] <- sum(char.arc.pgn)/chn
		#
		arc.stats[a, COL_SCENES] <- scn
		arc.stats[a, COL_SCENES_BY_VOLUME] <- sum(volume.stats[idx.vol,COL_SCENES])/vln
		arc.stats[a, COL_SCENES_BY_PAGE] <- sum(page.stats[idx.pg,COL_SCENES])/pgn
		arc.stats[a, COL_SCENES_BY_CHAR] <- sum(char.arc.scn)/chn
		#
		arc.stats[a, COL_PANELS] <- pnn
		arc.stats[a, COL_PANELS_BY_VOLUME] <- sum(volume.stats[idx.vol,COL_PANELS])/vln
		arc.stats[a, COL_PANELS_BY_PAGE] <- sum(page.stats[idx.pg,COL_PANELS])/pgn
		arc.stats[a, COL_PANELS_BY_SCENE] <- sum(scene.stats[idx.sc,COL_PANELS])/scn
		arc.stats[a, COL_PANELS_BY_CHAR] <- sum(char.arc.pnl)/chn
		#
		arc.stats[a, COL_CHARS] <- chn
		arc.stats[a, COL_CHARS_BY_VOLUME] <- sum(volume.stats[idx.vol,COL_CHARS])/vln
		arc.stats[a, COL_CHARS_BY_PAGE] <- sum(page.stats[idx.pg,COL_CHARS])/pgn
		arc.stats[a, COL_CHARS_BY_SCENE] <- sum(scene.stats[idx.sc,COL_CHARS])/scn
		arc.stats[a, COL_CHARS_BY_PANEL] <- sum(panel.stats[idx.pn,COL_CHARS])/pnn
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(arc.chars[[a]], char.stats[,COL_NAME])
			for(att in atts)
			{	m <- arc.stats.atts[[att]]
				tt <- table(char.stats[idx,att])
				m[a,names(tt)] <- tt
				arc.stats.atts[[att]] <- m
			}
		}
		
		# density plot: chars vs. panels (by scene)
		file <- get.path.stat.corpus(arc=a, desc="comparison_chars-scenes_vs_panels-scenes")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				xvals <- scene.stats[idx.sc,COL_CHARS]
				yvals <- scene.stats[idx.sc,COL_PANELS]
				xlab <- "Number of characters by scene"
				ylab <- "Number of panels by scene"
				p=ggplot(scene.stats[idx.sc,], aes(x=xvals, y=yvals)) +
					geom_hex(binwidth=1) + 
					coord_fixed() +
					scale_fill_viridis(begin=0.1, limits=c(0,NA),) + 
					theme_bw() +
					theme_classic() +	# base_size=18
					labs(fill="Frequency",x=xlab, y=ylab) +
					theme(legend.position="left") +
					geom_point(aes(x=xvals, y=yvals), alpha=0)
				ggMarginal(p, type="histogram", xparams=list(binwidth=1), yparams=list(binwidth=1), fill=MAIN_COLOR)
				print(p)
			dev.off()
		}
		# correlation
		val <- cor(scene.stats[idx.sc,COL_CHARS],scene.stats[idx.sc,COL_PANELS])
		arc.stats[a, COL_CORR_PANELS_CHARS_BY_SCENE] <- val
		tlog(6,"Correlation between characters/scene and panels/scene: ", val)
		
		# density: scenes vs. panels (by char)
		val <- cor(char.stats[idx.char,COL_SCENES],char.stats[idx.char,COL_PANELS])
		arc.stats[a, COL_CORR_SCENES_PANELS_BY_CHAR] <- val
		tlog(6,"Correlation between scenes/character and panels/character: ", val)
		
		# attribute stats
		tlog(6,"Computing attribute stats")
		for(at in 1:length(atts))
		{	tlog(5,"Computing attribute ",atts[at]," (",at,"/",length(atts),")")
			
			# attribute distribution over the characters
			vals <- table(char.stats[idx.char,atts[at]])
			perc <- vals/sum(vals)*100
			df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
			colnames(df) <- c(atts[at],"Frequency","Proportion")
			file <- get.path.stat.corpus(arc=a, desc="attr_distrib_arc_", att=atts[at])
			write.csv(x=df, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
			#
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					barplot(
						height=perc,
						main=paste0("Distribution of character attribute ",atts[at]," (%)"),
						col=MAIN_COLOR
					)
				dev.off()
			}
			
			# others?
		}
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, desc="arcs")
	tlog(4,"Recording in ",file)
	write.csv(x=arc.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=arc.stats.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	
	# evolution of the stats by arc
	arc.cols <- c(
			COL_VOLUMES, COL_VOLUMES_BY_CHAR,
			COL_PAGES, COL_PAGES_BY_VOLUME, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
			COL_SCENES, COL_SCENES_BY_VOLUME, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR, 
			COL_PANELS, COL_PANELS_BY_VOLUME, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR, 
			COL_CHARS, COL_CHARS_BY_VOLUME, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
			COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	arc.ttls <- c(
			"total number of volumes", "average number of volumes by character", 
			"total number of pages", "average number of pages by volume", "average number of pages by scene", "average number of pages by character", 
			"total number of scenes", "average number of scenes by volume", "average number of scenes by page", "average number of scenes by character", 
			"total number of panels", "average number of panels by volume", "average number of panels by page", "average number of panels by scene", "average number of panels by character", 
			"total number of characters", "average number of characters by volume", "average number of characters by page", "average number of characters by scene", "average number of characters by panel",
			"correlation between the numbers of characters and panels by scene", "correlation between the numbers of scenes and panels by character"
	)
	arc.fnames <- c(
			"num_volumes", "num_volumes_by_char",
			"num_pages", "num_pages_by_volume", "num_pages_by_scene", "num_pages_by_char",
			"num_scenes", "num_scenes_by_volume", "num_scenes_by_page", "num_scenes_by_char",
			"num_panels", "num_panels_by_volume", "num_panels_by_page", "num_panels_by_scene", "num_panels_by_char", 
			"num_chars", "num_chars_by_volume", "num_chars_by_page", "num_chars_by_scene", "num_chars_by_panel",
			"corr_chars_panels_by_scene", "corr_scenes_panels_by_char"
	)
	tlog(4,"Generating the plots")
	for(a in 1:length(arc.cols))
	{	tlog(5,"Processing column \"",arc.cols[a],"\" (",a,"/",length(arc.cols),")")
		
		file <- get.path.stat.corpus(object=object, desc=arc.fnames[a])
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				barplot(
					height=arc.stats[,arc.cols[a]],
					names.arg=arc.stats[,COL_ARC],
					main=paste0("Evolution of the ",arc.ttls[a]),
					col=MAIN_COLOR
				)
			dev.off()
		}
	}
	
	# TODO plot all arc distributions on the same plot?
	
	result <- list(
		arc.stats=arc.stats, arc.stats.atts=arc.stats.atts, arc.chars=arc.chars
	)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the volumes.
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
compute.stats.overall <- function(
		panel.stats, panel.chars, 
		page.stats, page.chars, 
		scene.stats, scene.chars, 
		char.stats, 
		volume.stats, volume.chars, 
		arc.stats, arc.chars)
{	# vertex attributes
	atts <- setdiff(colnames(char.stats), c(COL_NAME, COL_NAME_SHORT, COL_FREQ))
	att.nbr <- length(atts)
	
	# compute stats
	tlog(3,"Computing overall stats")
	panel.nbr <- max(c(scene.stats[,COL_PANEL_START_ID],scene.stats[,COL_PANEL_END_ID]))
	page.nbr <- max(c(scene.stats[,COL_PAGE_START_ID],scene.stats[,COL_PAGE_END_ID]))
	scene.nbr <- nrow(scene.stats)
	char.nbr <- length(sort(unique(unlist(scene.chars))))
	volume.nbr <- nrow(volume.stats)
	
	# init stat table
	overall.stats <- data.frame(
			integer(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), numeric(1), 
			numeric(1), numeric(1),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(overall.stats) <- c(
			COL_VOLUMES, COL_VOLUMES_BY_CHAR,
			COL_PAGES, COL_PAGES_BY_VOLUME, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
			COL_SCENES, COL_SCENES_BY_VOLUME, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR, 
			COL_PANELS, COL_PANELS_BY_VOLUME, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR, 
			COL_CHARS, COL_CHARS_BY_VOLUME, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
			COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	
	# init stats tables for attributes
	overall.stats.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.stats[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=1, ncol=length(uniq))
			colnames(m) <- uniq
			overall.stats.atts[[att]] <- m
		}
	}
	
	# compute stats
	overall.stats[1,COL_VOLUMES] <- volume.nbr
	overall.stats[1,COL_VOLUMES_BY_CHAR] <- sum(char.stats[,COL_VOLUMES])/char.nbr
	#
	overall.stats[1,COL_PAGES] <- page.nbr
	overall.stats[1,COL_PAGES_BY_VOLUME] <- sum(volume.stats[,COL_PAGES])/volume.nbr
	overall.stats[1,COL_PAGES_BY_SCENE] <- sum(scene.stats[,COL_PAGES])/scene.nbr
	overall.stats[1,COL_PAGES_BY_CHAR] <- sum(char.stats[,COL_PAGES])/char.nbr
	#
	overall.stats[1,COL_SCENES] <- scene.nbr
	overall.stats[1,COL_SCENES_BY_VOLUME] <- sum(volume.stats[,COL_SCENES])/volume.nbr
	overall.stats[1,COL_SCENES_BY_PAGE] <- sum(page.stats[,COL_SCENES])/page.nbr
	overall.stats[1,COL_SCENES_BY_CHAR] <- sum(char.stats[,COL_SCENES])/char.nbr
	#
	overall.stats[1,COL_PANELS] <- panel.nbr
	overall.stats[1,COL_PANELS_BY_VOLUME] <- panel.nbr/volume.nbr
	overall.stats[1,COL_PANELS_BY_PAGE] <- panel.nbr/page.nbr
	overall.stats[1,COL_PANELS_BY_SCENE] <- sum(scene.stats[,COL_PANELS])/scene.nbr
	overall.stats[1,COL_PANELS_BY_CHAR] <- sum(char.stats[,COL_PANELS])/char.nbr
	#
	overall.stats[1,COL_CHARS] <- char.nbr
	overall.stats[1,COL_CHARS_BY_VOLUME] <- sum(volume.stats[,COL_CHARS])/volume.nbr
	overall.stats[1,COL_CHARS_BY_PAGE] <- sum(page.stats[,COL_CHARS])/page.nbr
	overall.stats[1,COL_CHARS_BY_SCENE] <- sum(scene.stats[,COL_CHARS])/scene.nbr
	overall.stats[1,COL_CHARS_BY_PANEL] <- sum(panel.stats[,COL_CHARS])/panel.nbr
	
	# record stats
	file <- get.path.stat.corpus(desc="overall")
	tlog(4,"Recording in ",file)
	write.csv(x=overall.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	
	# density plot: chars vs. panels
	file <- get.path.stat.corpus(desc="comparison_chars-scenes_vs_panels-scenes")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			xvals <- scene.stats[,COL_CHARS]
			yvals <- scene.stats[,COL_PANELS]
			xlab <- "Number of characters by scene"
			ylab <- "Number of panels by scene"
			p=ggplot(scene.stats, aes(x=xvals, y=yvals)) +
				geom_hex(binwidth=2) + 
				coord_fixed() +
				scale_fill_viridis(begin=0.1, limits=c(0,NA),) + 
				theme_bw() +
				theme_classic() +	# base_size=18
				labs(fill="Frequency",x=xlab, y=ylab) +
				theme(legend.position="left") +
				geom_point(aes(x=xvals, y=yvals), alpha=0)
			ggMarginal(p, type="histogram", xparams=list(binwidth=2), yparams=list(binwidth=2), fill=MAIN_COLOR)
			print(p)
		dev.off()
	}
	# correlation
	val <- cor(scene.stats[,COL_CHARS],scene.stats[,COL_PANELS])
	overall.stats[1,COL_CORR_PANELS_CHARS_BY_SCENE] <- val
	tlog(4,"Correlation between characters/scene and panels/scene: ", val)
	
	# density plot: scenes vs. panels
	file <- get.path.stat.corpus(desc="comparison_scenes-chars_vs_panels-chars")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			xvals <- char.stats[,COL_SCENES]
			yvals <- char.stats[,COL_PANELS]
			xlab <- "Number of scenes by character"
			ylab <- "Number of panels by character"
			p=ggplot(char.stats, aes(x=xvals, y=yvals)) +
				geom_hex(binwidth=100) + 
				coord_fixed() +
				scale_fill_viridis(begin=0.1, limits=c(0,NA),) + 
				theme_bw() +
				theme_classic() +	# base_size=18
				labs(fill="Frequency",x=xlab, y=ylab) +
				theme(legend.position="left") +
				geom_point(aes(x=xvals, y=yvals), alpha=0)
			ggMarginal(p, type="histogram", xparams=list(binwidth=100), yparams=list(binwidth=100), fill=MAIN_COLOR)
			print(p)
		dev.off()
	}
	# correlation
	val <- cor(char.stats[,COL_SCENES],char.stats[,COL_PANELS])
	overall.stats[1,COL_CORR_SCENES_PANELS_BY_CHAR] <- val
	tlog(4,"Correlation between scenes/character and panels/character: ", val)
	
	# attribute stats
	tlog(3,"Computing attribute stats")
	for(a in 1:length(atts))
	{	tlog(4,"Computing attribute ",atts[a]," (",a,"/",length(atts),")")
		
		# attribute distribution over the characters
		vals <- table(char.stats[,atts[a]])
		perc <- vals/sum(vals)*100
		df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
		colnames(df) <- c(atts[a],"Frequency","Proportion")
		file <- get.path.stat.corpus(desc="attr_distrib", att=atts[a])
		write.csv(x=df, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		#
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				barplot(
					height=perc,
					main=paste0("Distribution of character attribute ",atts[a]," (%)"),
					col=MAIN_COLOR
				)
			dev.off()
		}
		
		# others?
	}
	
	
	result <- list(
		overall.stats=overall.stats, overall.stats.atts=overall.stats.atts
	)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the volumes.
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
plot.stats.overall <- function(
		panel.stats, panel.chars, 
		page.stats, page.chars, 
		scene.stats, scene.chars, 
		char.stats, 
		volume.stats, volume.chars, 
		arc.stats, arc.chars)
{	# vertex attributes
	atts <- setdiff(colnames(char.stats), c(COL_NAME, COL_NAME_SHORT, COL_FREQ))
	att.nbr <- length(atts)
	
	# compute stats
	tlog(3,"Computing overall stats")
	panel.nbr <- max(c(scene.stats[,COL_PANEL_START_ID],scene.stats[,COL_PANEL_END_ID]))
	page.nbr <- max(c(scene.stats[,COL_PAGE_START_ID],scene.stats[,COL_PAGE_END_ID]))
	scene.nbr <- nrow(scene.stats)
	char.nbr <- length(sort(unique(unlist(scene.chars))))
	volume.nbr <- nrow(volume.stats)
	
	# init stat table
	overall.stats <- data.frame(
			integer(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), numeric(1), 
			numeric(1), numeric(1),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(overall.stats) <- c(
			COL_VOLUMES, COL_VOLUMES_BY_CHAR,
			COL_PAGES, COL_PAGES_BY_VOLUME, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
			COL_SCENES, COL_SCENES_BY_VOLUME, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR, 
			COL_PANELS, COL_PANELS_BY_VOLUME, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR, 
			COL_CHARS, COL_CHARS_BY_VOLUME, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
			COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	
	# init stats tables for attributes
	overall.stats.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.stats[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=1, ncol=length(uniq))
			colnames(m) <- uniq
			overall.stats.atts[[att]] <- m
		}
	}
	
	# compute stats
	overall.stats[1,COL_VOLUMES] <- volume.nbr
	overall.stats[1,COL_VOLUMES_BY_CHAR] <- sum(char.stats[,COL_VOLUMES])/char.nbr
	#
	overall.stats[1,COL_PAGES] <- page.nbr
	overall.stats[1,COL_PAGES_BY_VOLUME] <- sum(volume.stats[,COL_PAGES])/volume.nbr
	overall.stats[1,COL_PAGES_BY_SCENE] <- sum(scene.stats[,COL_PAGES])/scene.nbr
	overall.stats[1,COL_PAGES_BY_CHAR] <- sum(char.stats[,COL_PAGES])/char.nbr
	#
	overall.stats[1,COL_SCENES] <- scene.nbr
	overall.stats[1,COL_SCENES_BY_VOLUME] <- sum(volume.stats[,COL_SCENES])/volume.nbr
	overall.stats[1,COL_SCENES_BY_PAGE] <- sum(page.stats[,COL_SCENES])/page.nbr
	overall.stats[1,COL_SCENES_BY_CHAR] <- sum(char.stats[,COL_SCENES])/char.nbr
	#
	overall.stats[1,COL_PANELS] <- panel.nbr
	overall.stats[1,COL_PANELS_BY_VOLUME] <- panel.nbr/volume.nbr
	overall.stats[1,COL_PANELS_BY_PAGE] <- panel.nbr/page.nbr
	overall.stats[1,COL_PANELS_BY_SCENE] <- sum(scene.stats[,COL_PANELS])/scene.nbr
	overall.stats[1,COL_PANELS_BY_CHAR] <- sum(char.stats[,COL_PANELS])/char.nbr
	#
	overall.stats[1,COL_CHARS] <- char.nbr
	overall.stats[1,COL_CHARS_BY_VOLUME] <- sum(volume.stats[,COL_CHARS])/volume.nbr
	overall.stats[1,COL_CHARS_BY_PAGE] <- sum(page.stats[,COL_CHARS])/page.nbr
	overall.stats[1,COL_CHARS_BY_SCENE] <- sum(scene.stats[,COL_CHARS])/scene.nbr
	overall.stats[1,COL_CHARS_BY_PANEL] <- sum(panel.stats[,COL_CHARS])/panel.nbr
	
	# record stats
	file <- get.path.stat.corpus(desc="overall")
	tlog(4,"Recording in ",file)
	write.csv(x=overall.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	
	# density plot: chars vs. panels
	file <- get.path.stat.corpus(desc="comparison_chars-scenes_vs_panels-scenes")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			xvals <- scene.stats[,COL_CHARS]
			yvals <- scene.stats[,COL_PANELS]
			xlab <- "Number of characters by scene"
			ylab <- "Number of panels by scene"
			p=ggplot(scene.stats, aes(x=xvals, y=yvals)) +
				geom_hex(binwidth=2) + 
				coord_fixed() +
				scale_fill_viridis(begin=0.1, limits=c(0,NA),) + 
				theme_bw() +
				theme_classic() +	# base_size=18
				labs(fill="Frequency",x=xlab, y=ylab) +
				theme(legend.position="left") +
				geom_point(aes(x=xvals, y=yvals), alpha=0)
			ggMarginal(p, type="histogram", xparams=list(binwidth=2), yparams=list(binwidth=2), fill=MAIN_COLOR)
			print(p)
		dev.off()
	}
	# correlation
	val <- cor(scene.stats[,COL_CHARS],scene.stats[,COL_PANELS])
	overall.stats[1,COL_CORR_PANELS_CHARS_BY_SCENE] <- val
	tlog(4,"Correlation between characters/scene and panels/scene: ", val)
	
	# density plot: scenes vs. panels
	file <- get.path.stat.corpus(desc="comparison_scenes-chars_vs_panels-chars")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			xvals <- char.stats[,COL_SCENES]
			yvals <- char.stats[,COL_PANELS]
			xlab <- "Number of scenes by character"
			ylab <- "Number of panels by character"
			p=ggplot(char.stats, aes(x=xvals, y=yvals)) +
				geom_hex(binwidth=100) + 
				coord_fixed() +
				scale_fill_viridis(begin=0.1, limits=c(0,NA),) + 
				theme_bw() +
				theme_classic() +	# base_size=18
				labs(fill="Frequency",x=xlab, y=ylab) +
				theme(legend.position="left") +
				geom_point(aes(x=xvals, y=yvals), alpha=0)
			ggMarginal(p, type="histogram", xparams=list(binwidth=100), yparams=list(binwidth=100), fill=MAIN_COLOR)
			print(p)
		dev.off()
	}
	# correlation
	val <- cor(char.stats[,COL_SCENES],char.stats[,COL_PANELS])
	overall.stats[1,COL_CORR_SCENES_PANELS_BY_CHAR] <- val
	tlog(4,"Correlation between scenes/character and panels/character: ", val)
	
	# attribute stats
	tlog(3,"Computing attribute stats")
	for(a in 1:length(atts))
	{	tlog(4,"Computing attribute ",atts[a]," (",a,"/",length(atts),")")
		
		# attribute distribution over the characters
		vals <- table(char.stats[,atts[a]])
		perc <- vals/sum(vals)*100
		df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
		colnames(df) <- c(atts[a],"Frequency","Proportion")
		file <- get.path.stat.corpus(desc="attr_distrib", att=atts[a])
		write.csv(x=df, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		#
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				barplot(
					height=perc,
					main=paste0("Distribution of character attribute ",atts[a]," (%)"),
					col=MAIN_COLOR
				)
			dev.off()
		}
		
		# others?
	}
	
	
	result <- list(
		overall.stats=overall.stats, overall.stats.atts=overall.stats.atts
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
compute.stats <- function(data)
{	tlog(2,"Computing corpus stats")
	inter.df <- data$inter.df
	panel.stats <- data$panel.stats
	panel.chars <- data$panel.chars
	page.stats <- data$page.stats
	page.chars <- data$page.chars
	scene.stats <- data$scene.stats
	scene.chars <- data$scene.chars
	char.stats <- data$char.stats
	volume.stats <- data$volume.stats
	volume.chars <- data$volume.chars 
	arc.stats <- data$arc.stats
	arc.chars <- data$arc.chars
		
	# complete panel stats
	tmp <- compute.stats.panel(
		panel.stats=panel.stats, panel.chars=panel.chars, 
		char.stats=char.stats
	)
	panel.stats.atts <- tmp$panel.stats.atts
	# plot panel stats
	plot.stats.panel(
		panel.stats=panel.stats, panel.stats.atts=panel.stats.atts,
		volume.stats,
		cur.vol=NA, cur.arc=NA
	)
	
	# complete page stats
	tmp <- compute.stats.page(
		page.stats=page.stats, page.chars=page.chars,
		char.stats=char.stats
	)
	page.stats.atts <- tmp$page.stats.atts
	# plot page stats
	plot.stats.page(
		page.stats=page.stats, page.stats.atts=page.stats.atts,
		volume.stats,
		cur.vol=NA, cur.arc=NA
	)
	
	# complete scene stats
	tmp <- compute.stats.scene(
		scene.stats=scene.stats, scene.chars=scene.chars,
		char.stats=char.stats 
	)
	scene.stats.atts <- tmp$scene.stats.atts
	# plot scene stats
	plot.stats.scene(
		scene.stats=scene.stats, scene.stats.atts=scene.stats.atts,
		volume.stats,
		cur.vol=NA, cur.arc=NA
	)
	
	# plot character stats
	plot.stats.char(
		char.stats=char.stats, 
		volume.stats=volume.stats, 
		cur.vol=NA, cur.arc=NA,
		filtered=FALSE
	)
#	# possibly process filtered characters (just for plots) >> TODO do that later (after net extraction)
#	if(COL_FILTERED %in% colnames(char.stats))
#	{	plot.stats.char(
#			char.stats=char.stats, 
#			volume.stats=volume.stats, 
#			cur.vol=NA, cur.arc=NA,
#			filtered=TRUE
#		)
#	}
	
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
	# plot volume stats
	plot.stats.volume(
		volume.stats=volume.stats, volume.chars=volume.chars, volume.stats.indiv=volume.stats.indiv
	)
	
	# complete arc stats
	tmp <- compute.stats.arc(
		panel.stats=panel.stats, panel.chars=panel.chars,
		page.stats=page.stats, page.chars=page.chars,
		scene.stats=scene.stats, scene.chars=scene.chars,
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.chars=volume.chars, 
		arc.stats=arc.stats, arc.chars=arc.chars
	)
	arc.stats <- tmp$arc.stats
	arc.stats.atts <- tmp$arc.stats.atts
	arc.chars <- tmp$arc.chars
	
	# complete overall stats
	tmp <- compute.stats.overall(
		panel.stats=panel.stats, panel.chars=panel.chars,
		page.stats=page.stats, page.chars=page.chars,
		scene.stats=scene.stats, scene.chars=scene.chars,
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.chars=volume.chars, 
		arc.stats=arc.stats, arc.chars=arc.chars
	)
	overall.stats <- tmp$overall.stats
	overall.stats.atts <- tmp$overall.stats.atts
	
	# return all the stats
	result <- list(
		inter.df=inter.df,
		panel.stats=panel.stats, panel.stats.atts=panel.stats.atts, panel.chars=panel.chars,
		page.stats=page.stats, page.stats.atts=page.stats.atts, page.chars=page.chars,
		scene.stats=scene.stats, scene.stats.atts=scene.stats.atts, scene.chars=scene.chars,
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.stats.atts=volume.stats.atts, volume.chars=volume.chars,
		arc.stats=arc.stats, arc.stats.atts=arc.stats.atts, arc.chars=arc.chars,
		overall.stats=overall.stats, overall.stats.atts=overall.stats.atts
	)
	return(result)
}
