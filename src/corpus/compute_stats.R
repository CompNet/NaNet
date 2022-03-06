# This script contains functions used to compute some statistics and generate
# plots related to the corpus.
# 
# Vincent Labatut
# 05/2021
###############################################################################




###############################################################################
# Computes and records some statistics regarding the panels.
#
# page.stats: table describing all the pages constituting the BD series.
# scene.stats: previously computed scene statistics.
# scene.chars: characters present in each scene.
# char.stats: table describing all the characters occurring in the BD series.
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or Id of the volume specifically processed.
# cur.arc: NA (regular mode) or Id of the arc specifically processed.
#
# returns: a list containing the table with overall panel stats (panel.stats), the 
#          panel stats by vertex attribute values (panel.stats.atts), and the list
#          of characters for each panel (panel.chars).
###############################################################################
compute.stats.panel <- function(
		page.stats, 
		scene.stats, scene.chars,
		char.stats, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA)
{	object <- "panels"
	# vertex attributes
	atts <- setdiff(colnames(char.stats), c(COL_NAME, COL_NAME_SHORT, COL_FREQ))
	att.nbr <- length(atts)
	# panel positions
	pages.end.panel.ids <- c(
			page.stats[2:nrow(page.stats),COL_PANEL_START_ID]-1,
			page.stats[nrow(page.stats),COL_PANEL_START_ID]+page.stats[nrow(page.stats),COL_PANELS]
	)
	
	# volume name
	if(!is.na(cur.vol))
		vname <- volume.stats[1, COL_VOLUME]
	else
		vname <- NA
	
	# compute stats
	tlog(3,"Computing panel stats (cur.vol=",cur.vol," vname=",vname,")")
	
	# list of concerned panel ids
	panel.ids <- c()
	for(s in 1:nrow(scene.stats))
	{	panel.ids <- union(panel.ids, 
				seq(scene.stats[s,COL_PANEL_START_ID], scene.stats[s,COL_PANEL_END_ID])
		)
	}
	panel.nbr <- length(panel.ids)
	
	# init stats table for panels
	panel.stats <- data.frame(
			character(panel.nbr), integer(panel.nbr), 
			integer(panel.nbr), integer(panel.nbr), 
			integer(panel.nbr), integer(panel.nbr),
			integer(panel.nbr),
			logical(panel.nbr), logical(panel.nbr), logical(panel.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(panel.stats) <- c(
			COL_VOLUME, COL_VOLUME_ID,
			COL_PAGE, COL_PAGE_ID,
			COL_PANEL, COL_PANEL_ID, 
			COL_CHARS, 
			COL_MATCH_START, COL_MATCH_END, COL_MATCH_BOTH
	)
	# init stats tables for attributes
	panel.stats.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.stats[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=panel.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			panel.stats.atts[[att]] <- m
		}
	}
	
	# compute the stats for each panel
	tlog(4,"Processing each panel separately")
	panel.chars <- lapply(1:panel.nbr, function(x) c())
	for(p in 1:panel.nbr)
	{	tlog(5,"Processing panel id ",panel.ids[p]," (",p,"/",panel.nbr,")")
		
		# find the scenes containing this panel
		ss <- which(scene.stats[,COL_PANEL_START_ID]<=panel.ids[p]
						& scene.stats[,COL_PANEL_END_ID]>=panel.ids[p])
		cur.page <- which(page.stats[,COL_PANEL_START_ID]<=panel.ids[p]
						& pages.end.panel.ids>=panel.ids[p])
		pos <- panel.ids[p] - page.stats[cur.page, COL_PANEL_START_ID] + 1
		match.start <- pos==1
		match.end <- pos==page.stats[cur.page,COL_PANELS]
		for(s in ss)
		{	if(length(scene.chars[[s]])>0)
				panel.chars[[p]] <- union(panel.chars[[p]], scene.chars[[s]])
		}
		
		# update overall stat table
		panel.stats[p, COL_VOLUME] <- scene.stats[ss[1],COL_VOLUME]
		panel.stats[p, COL_VOLUME_ID] <- scene.stats[ss[1],COL_VOLUME_ID]
		panel.stats[p, COL_PAGE] <- page.stats[cur.page,COL_PAGE]
		panel.stats[p, COL_PAGE_ID] <- page.stats[cur.page,COL_PAGE_ID]
		panel.stats[p, COL_PANEL] <- pos
		panel.stats[p, COL_PANEL_ID] <- panel.ids[p]
		panel.stats[p, COL_CHARS] <- length(panel.chars[[p]])
		panel.stats[p, COL_MATCH_START] <- match.start
		panel.stats[p, COL_MATCH_END] <- match.end
		panel.stats[p, COL_MATCH_BOTH] <- match.start && match.end
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(panel.chars[[p]], char.stats[,COL_NAME])
			for(att in atts)
			{	m <- panel.stats.atts[[att]]
				tt <- table(char.stats[idx,att])
				m[p,names(tt)] <- tt
				panel.stats.atts[[att]] <- m
			}
		}
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels")
	tlog(4,"Recording in ",file)
	write.csv(x=panel.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=panel.stats.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	
	# distributions of character numbers (overall)
	vals <- table(panel.stats[,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_PANELS, "Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_char_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- panel.stats[,COL_CHARS]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- "Character number distribution over panels"
			xl <- "Number of characters by panel"
			# histogram
			h <- hist(
				data,
				breaks=0:max(data),
				col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
	
	# distribution of character numbers (by attribute)
	if(att.nbr>0)
	{	for(att in atts)
		{	pal <- get.palette(ncol(panel.stats.atts[[att]]))[1:ncol(panel.stats.atts[[att]])]
			file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_char_nbr", att=att)
			data <- panel.stats.atts[[att]]
			write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					ml <- paste0("Character number distribution over pages (att=",att)
					xl <- "Number of characters by panel"
					yl <- "Frequency"
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
						legend.text=colnames(panel.stats.atts[[att]])
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 1:ncol(panel.stats.atts[[att]]))
			{	data <- panel.stats.atts[[att]][,d]
				file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_char_nbr", att=att, val=colnames(panel.stats.atts[[att]])[d])
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						h <- hist(
							data,
							breaks=0:max(data),
							col=pal[d],
							xlab=xl,
							main=paste0(ml," val=",colnames(panel.stats.atts[[att]])[d],")"),
							freq=FALSE,
							#plot=FALSE
						)
					dev.off()
				}
			}
		}
	}
	
	# distribution of panel positions
	vals <- c()
	vals["Both"] <- length(which(panel.stats[, COL_MATCH_BOTH]))
	vals["Starts page"] <- length(which(panel.stats[, COL_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(panel.stats[, COL_MATCH_END])) - vals["Both"]
	vals["None"] <- nrow(panel.stats) - vals["Both"] - vals["Starts page"] - vals["Ends page"]
	perc <- vals/sum(vals)*100
	df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
	colnames(df) <- c("Position","Frequency","Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_positions")
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
				col=MAIN_COLOR
			)
		dev.off()
	}
	
	result <- list(panel.stats=panel.stats, panel.stats.atts=panel.stats.atts, panel.chars=panel.chars)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the pages.
#
# panel.stats: previously computed panel statistics.
# panel.chars: characters present in each panel.
# page.stats: table describing all the pages constituting the BD series.
# scene.stats: previously computed scene statistics.
# scene.chars: characters present in each scene.
# char.stats: table describing all the characters occurring in the BD series.
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or Id of the volume specifically processed.
# cur.arc: NA (regular mode) or Id of the arc specifically processed.
#
# returns: a list containing the table with overall page stats (page.stats), the 
#          page stats by vertex attribute values (page.stats.atts), and the list
#          of characters for each page (page.chars).
###############################################################################
compute.stats.page <- function(
		panel.stats, panel.chars,
		page.stats, 
		scene.stats, scene.chars, 
		char.stats, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA)
{	object <- "pages"
	# vertex attributes
	atts <- setdiff(colnames(char.stats), c(COL_NAME, COL_NAME_SHORT, COL_FREQ))
	att.nbr <- length(atts)
	
	# volume name
	if(!is.na(cur.vol))
		vname <- volume.stats[1, COL_VOLUME]
	else
		vname <- NA
	
	# compute stats
	page.ids <- sort(unique(panel.stats[,COL_PAGE_ID]))
	page.nbr <- length(page.ids)
	
	# list the characters by page
	tlog(3,"Listing the characters by page")
	page.chars <- lapply(1:page.nbr, function(x) c())
	first.pg.id <- scene.stats[1,COL_PAGE_START_ID]
	for(s in 1:nrow(scene.stats))
	{	tlog(5,"Processing scene id ",s,"/",nrow(scene.stats))
		
		# find the pages containing the scene
		start.page.id <- scene.stats[s,COL_PAGE_START_ID]
		end.page.id <- scene.stats[s,COL_PAGE_END_ID]
		sc.page.ids <- start.page.id:end.page.id
		for(page.id in sc.page.ids)
		{	if(length(scene.chars[[s]])>0)
			{	i <- which(page.stats[,COL_PAGE_ID]==page.id)
				page.chars[[i]] <- union(page.chars[[i]], scene.chars[[s]])
			}
		}
	}
	
	# init stats table for pages
	tmp <- data.frame(
			integer(page.nbr), 
			integer(page.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(tmp) <- c(
			COL_SCENES, 
			COL_CHARS
	)
	page.stats <- cbind(page.stats, tmp)
	# init stats tables for attributes
	page.stats.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.stats[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=page.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			page.stats.atts[[att]] <- m
		}
	}
	
	# compute the stats for each page
	tlog(4,"Processing each page separately")
	for(p in 1:page.nbr)
	{	tlog(5,"Processing page id ",page.ids[p]," (",p,"/",page.nbr,")")
		
		# number of scenes overlapping the page
		scn <- length(which(
						scene.stats[,COL_PAGE_START_ID]<=page.ids[p]
								& scene.stats[,COL_PAGE_END_ID]>=page.ids[p]
				))
		
		# update overall stat table
		page.stats[p, COL_SCENES] <- scn
		page.stats[p, COL_CHARS] <- length(page.chars[[p]])
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(page.chars[[p]], char.stats[,COL_NAME])
			for(att in atts)
			{	m <- page.stats.atts[[att]]
				tt <- table(char.stats[idx,att])
				m[p,names(tt)] <- tt
				page.stats.atts[[att]] <- m
			}
		}
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages")
	tlog(4,"Recording in ",file)
	write.csv(x=page.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=page.stats.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	
	# distributions of scene numbers
	vals <- table(page.stats[,COL_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_SCENES, COL_PAGES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_scene_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- page.stats[,COL_SCENES]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- "Scene number distribution over pages"
			xl <- "Number of scenes by page"
			# histogram
			h <- hist(
					data,
					breaks=0:max(data),
					col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
	
	# distributions of panel numbers
	vals <- table(page.stats[,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_PAGES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_panel_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- page.stats[,COL_PANELS]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- "Panel number distribution over pages"
			xl <- "Number of panels by page"
			# histogram
			h <- hist(
					data,
					breaks=0:max(data),
					col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
	
	# distributions of character numbers (overall)
	vals <- table(page.stats[,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_PAGES, "Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_char_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- page.stats[,COL_CHARS]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- "Character number distribution over pages"
			xl <- "Number of characters by page"
			# histogram
			h <- hist(
					data,
					breaks=0:max(data),
					col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
	
	# distribution of character numbers (by attribute)
	if(att.nbr>0)
	{	for(att in atts)
		{	pal <- get.palette(ncol(page.stats.atts[[att]]))[1:ncol(page.stats.atts[[att]])]
			file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_char_nbr", att=att)
			data <- page.stats.atts[[att]]
			write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					ml <- paste0("Character number distribution over pages (att=",att)
					xl <- "Number of characters by page"
					yl <- "Frequency"
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
						legend.text=colnames(page.stats.atts[[att]])
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 1:ncol(page.stats.atts[[att]]))
			{	data <- page.stats.atts[[att]][,d]
				if(any(data!=0))
				{	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_char_nbr", att=att, val=colnames(page.stats.atts[[att]])[d])
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
							h <- hist(
								data,
								breaks=0:max(data),
								col=pal[d],
								xlab=xl,
								main=paste0(ml," - val=",colnames(page.stats.atts[[att]])[d],")"),
								freq=FALSE,
								#plot=FALSE
							)
						dev.off()
					}
				}
			}
		}
	}
	
	result <- list(page.stats=page.stats, page.stats.atts=page.stats.atts, page.chars=page.chars)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the scenes.
#
# panel.stats: previously computed panel statistics.
# panel.chars: characters present in each panel.
# page.stats: previously computed page statistics.
# page.chars: characters present in each page.
# scene.stats: previously computed scene statistics.
# scene.chars: characters present in each scene.
# char.stats: table describing all the characters occurring in the BD series.
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or Id of the volume specifically processed.
# cur.arc: NA (regular mode) or Id of the arc specifically processed.
#
# returns: a list containing the table with overall scene stats (scene.stats) and the 
#          scene stats by vertex attribute values (scene.stats.atts).
###############################################################################
compute.stats.scene <- function(
		panel.stats, panel.chars,
		page.stats, page.chars, 
		scene.stats, scene.chars, 
		char.stats, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA)
{	scene.stats
	
	
	
	object <- "scenes"
	# vertex attributes
	atts <- setdiff(colnames(char.stats), c(COL_NAME, COL_NAME_SHORT, COL_FREQ))
	att.nbr <- length(atts)
	
	# volume name
	if(!is.na(cur.vol))
		vname <- volume.stats[1, COL_VOLUME]
	else
		vname <- NA
	
	# compute stats
	tlog(3,"Computing scene stats")
	scene.nbr <- nrow(scene.stats)
	
	# init stats tables for attributes
	scene.stats.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.stats[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=scene.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			scene.stats.atts[[att]] <- m
		}
	}
	
	# compute additional stats for each scene
	tlog(4,"Processing each scene separately")
	for(s in 1:scene.nbr)
	{	tlog(5,"Processing scene ",s,"/",scene.nbr)
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(scene.chars[[s]], char.stats[,COL_NAME])
			for(att in atts)
			{	m <- scene.stats.atts[[att]]
				tt <- table(char.stats[idx,att])
				m[s,names(tt)] <- tt
				scene.stats.atts[[att]] <- m
			}
		}
	}
	
	# record scene stats
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes")
	tlog(4,"Recording in ",file)
	write.csv(x=scene.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=scene.stats.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	
	# distributions of panel numbers
	vals <- table(scene.stats[,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_SCENES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_panel_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- scene.stats[,COL_PANELS]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- "Panel number distribution over scenes"
			xl <- "Number of panels by scene"
#				# histogram
#			h <- hist(
#					data,
#					breaks=0:max(data),
##					col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy", yaxt="n") #las=1
#			axis(side=2, at=10^(expmax:0), label=parse(text=paste("10^", expmax:0, sep="")), las=1)
			# complementary cumulative distribution function
			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
#	# check distribution
#	test.disc.distr(data)
	
	# distributions of character numbers (overall)
	vals <- table(scene.stats[,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_SCENES, "Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_char_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- scene.stats[,COL_CHARS]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- "Character number distribution over scenes"
			xl <- "Number of characters by scene"
			# histogram
			h <- hist(
					data,
					breaks=0:max(data),
					col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
#			# complementary cumulative distribution function
#			plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
	
	# distribution of character numbers (by attribute)
	if(att.nbr>0)
	{	for(att in atts)
		{	pal <- get.palette(ncol(scene.stats.atts[[att]]))[1:ncol(scene.stats.atts[[att]])]
			file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_char_nbr", att=att)
			data <- scene.stats.atts[[att]]
			write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					ml <- paste0("Character number distribution over scenes (att=",att)
					xl <- "Number of characters by scene"
					yl <- "Frequency"
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
						legend.text=colnames(scene.stats.atts[[att]])
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 1:ncol(scene.stats.atts[[att]]))
			{	data <- scene.stats.atts[[att]][,d]
				file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_char_nbr", att=att, val=colnames(scene.stats.atts[[att]])[d])
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						h <- hist(
							data,
							breaks=0:max(data),
							col=pal[d],
							xlab=xl,
							main=paste0(ml," - val=",colnames(scene.stats.atts[[att]])[d],")"),
							freq=FALSE,
							#plot=FALSE
						)
					dev.off()
				}
			}
		}
	}
	
	# distributions of page numbers
	vals <- table(scene.stats[,COL_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_PAGES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_page_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- scene.stats[,COL_PAGES]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- "Page number distribution over scenes"
			xl <- "Number of pages by scene"
			# histogram
			h <- hist(
					data,
					breaks=0:max(data),
					col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
#	# check distribution
#	test.disc.distr(data)
	
	# distribution of scene positions
	vals <- c()
	vals["Both"] <- length(which(scene.stats[, COL_MATCH_BOTH]))
	vals["Starts page"] <- length(which(scene.stats[, COL_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(scene.stats[, COL_MATCH_END])) - vals["Both"]
	vals["None"] <- nrow(scene.stats) - vals["Both"] - vals["Starts page"] - vals["Ends page"]
	perc <- vals/sum(vals)*100
	df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
	colnames(df) <- c("Position","Frequency","Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_positions")
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
				col=MAIN_COLOR
			)
		dev.off()
	}
	
	result <- list(scene.stats=scene.stats, scene.stats.atts=scene.stats.atts)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the characters.
#
# panel.stats: previously computed panel statistics.
# panel.chars: characters present in each panel.
# page.stats: previously computed page statistics.
# page.chars: characters present in each page.
# scene.stats: previously computed scene statistics.
# scene.chars: characters present in each scene.
# char.stats: table describing all the characters occurring in the BD series.
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or Id of the volume specifically processed.
# cur.arc: NA (regular mode) or Id of the arc specifically processed.
# filtered: whether to process only the filtered characters.
#
# returns: a list containing the table with overall character stats (char.stats) and
#		   the list of characters for each volume (volume.chars).
###############################################################################
compute.stats.char <- function(
		panel.stats=panel.stats, panel.chars=panel.chars,
		page.stats=page.stats, page.char=page.char, 
		scene.stats=scene.stats, scene.chars=scene.chars, 
		char.stats=char.stats, 
		volume.stats=volume.stats, 
		cur.vol=NA, cur.arc=NA,
		filtered=FALSE)
{	object <- "characters"
	# vertex attributes
	atts <- setdiff(colnames(char.stats), c(COL_NAME, COL_NAME_SHORT, COL_FREQ, COL_FILTERED, COL_NAMED))
	att.nbr <- length(atts)
	
	# volume name
	if(!is.na(cur.vol))
		vname <- volume.stats[1, COL_VOLUME]
	else
		vname <- NA
	
	# compute stats
	tlog(3,"Computing ",if(filtered) "filtered" else "all"," character stats")
	unique.chars <- sort(unique(unlist(scene.chars)))
	if(filtered)
	{	idx.rm <- which(char.stats[,COL_FILTERED])
		idx.kp <- which(!char.stats[,COL_FILTERED])
		unique.chars <- setdiff(unique.chars, char.stats[idx.rm ,COL_NAME])
	}
	char.nbr <- length(unique.chars)
	
	#  identify the characters in each volume
	tlog(4,"Identify the",if(filtered) " filtered" else ""," characters in each volume")
	volume.ids <- volume.stats[,COL_VOLUME_ID]
	volume.nbr <- length(volume.ids)
	volume.chars <- lapply(1:volume.nbr, function(x) c())
	for(v in 1:volume.nbr)
	{	tlog(5,"Processing volume id ",volume.ids[v]," (",v,"/",volume.nbr,")")
		
		# find the pages contained in the volume
		idx.pg <- which(page.stats[,COL_VOLUME_ID]==volume.ids[v])
		for(p in idx.pg)
		{	if(length(page.chars[[p]])>0)
				volume.chars[[v]] <- union(volume.chars[[v]], page.chars[[p]])
		}
	}
	
	# init stats table for characters
	tmp <- data.frame(
			integer(char.nbr),
			integer(char.nbr),
			integer(char.nbr),
			integer(char.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(tmp) <- c(
			COL_VOLUMES, 
			COL_PAGES, 
			COL_SCENES, 
			COL_PANELS
	)
	char.stats <- cbind(char.stats, tmp)
	
	# compute the stats for each character
	tlog(4,"Processing each",if(filtered) " filtered" else ""," character separately")
	for(c in 1:char.nbr)
	{	tlog(5,"Processing",if(filtered) " filtered" else ""," character \"",unique.chars[c],"\" (",c,"/",char.nbr,")")
		
		# update overall stat table
		char.stats[c, COL_VOLUMES] <- sum(sapply(volume.chars, function(volume.char) unique.chars[c] %in% volume.char))
		char.stats[c, COL_PAGES] <- sum(sapply(page.chars, function(page.char) unique.chars[c] %in% page.char))
		char.stats[c, COL_SCENES] <- sum(sapply(scene.chars, function(scene.char) unique.chars[c] %in% scene.char))
		char.stats[c, COL_PANELS] <- sum(sapply(panel.chars, function(panel.char) unique.chars[c] %in% panel.char))
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("characters", if(filtered) "_filtered" else ""))
	tlog(4,"Recording in ",file)
	write.csv(x=char.stats, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	
	# distributions of volume numbers
	vals <- table(char.stats[,COL_VOLUMES])
	vals <- data.frame(names(vals), vals, 100*vals/sum(vals), stringsAsFactors=FALSE, check.names=FALSE)
	colnames(vals) <- c(COL_VOLUMES, COL_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("chars", if(filtered) "_filtered" else "","_distrib_volume_nbr"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[,COL_VOLUMES]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	if(length(unique(data))>1)
	{	for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					ml <- paste0("Volume number distribution over",if(filtered) " filtered" else ""," characters")
					xl <- paste0("Number of volumes by",if(filtered) " filtered" else ""," character")
#					# histogram
#					h <- hist(
#						data,
#						breaks=0:max(data),
##						col=MAIN_COLOR,
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
#					plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
					# complementary cumulative distribution function
					plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
				dev.off()
		}
		# check distribution
#		tmp <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, sims=100, plot.file=paste0(file,"_disttest"))
	}
	
	# distributions of page numbers
	vals <- table(char.stats[,COL_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PAGES, COL_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("chars", if(filtered) "_filtered" else "","_distrib_page_nbr"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[,COL_PAGES]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- paste0("Page number distribution over",if(filtered) " filtered" else ""," characters")
			xl <- paste0("Number of pages by",if(filtered) " filtered" else ""," character")
#			# histogram
#			h <- hist(
#					data,
#					breaks=0:max(data),
##					col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
	# check distribution
#	tmp <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, sims=100, plot.file=paste0(file,"_disttest"))
	
	# distributions of scene numbers
	vals <- table(char.stats[,COL_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_SCENES, COL_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("chars", if(filtered) "_filtered" else "","_distrib_scene_nbr"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[,COL_SCENES]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- paste0("Scene number distribution over",if(filtered) " filtered" else ""," characters")
			xl <- paste0("Number of scenes by",if(filtered) " filtered" else ""," character")
#			# histogram
#			h <- hist(
#					data,
#				breaks=0:max(data),
##				col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
	# check distribution
#	tmp <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, sims=100, plot.file=paste0(file,"_disttest"))
	
	# distributions of panel numbers
	vals <- table(char.stats[,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("chars", if(filtered) "_filtered" else "","_distrib_panel_nbr"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[,COL_PANELS]
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			ml <- paste0("Panel number distribution over",if(filtered) " filtered" else ""," characters")
			xl <- paste0("Number of panels by",if(filtered) " filtered" else ""," character")
#			# histogram
#			h <- hist(
#					data,
#					breaks=0:max(data),
##					col=MAIN_COLOR,
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
#			plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
		dev.off()
	}
	# check distribution
#	tmp <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, sims=100, plot.file=paste0(file,"_disttest"))
	
	# behavior of character filtering (trying to identify extras)
	thresholds <- seq(0, 10)	#max(char.stats[,COL_FREQ]))
	char.nums <- sapply(thresholds, function(t) c(table(factor(char.stats[char.stats[,COL_FREQ]>=t,COL_NAMED], levels=c("TRUE","FALSE")))))
	# generate barplots
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="chars_filtering_by_occurences")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			cols <- get.palette(values=2)
			if(filtered)
				cols <- c(cols[2], combine.colors(cols[2], "WHITE", transparency=40))
			else
				cols <- c(cols[1], combine.colors(cols[1], "WHITE", transparency=40))
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
			text2 <- 
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
	
	result <- list(char.stats=char.stats, volume.chars=volume.chars)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the volumes.
#
# panel.stats: previously computed panel statistics.
# panel.chars: characters present in each panel.
# page.stats: previously computed page statistics.
# page.chars: characters present in each page.
# scene.stats: previously computed scene statistics.
# char.stats: previously computed character statistics.
# volume.stats: table describing the series volumes.
# volume.chars: characters present in each volume.
#
# returns: a list containing the table with overall volume stats (volume.stats), and
#          the volume stats by vertex attribute values (volume.stats.atts).
###############################################################################
compute.stats.volume <- function(
		panel.stats, panel.chars,
		page.stats, page.chars, 
		scene.stats, scene.chars,
		char.stats,
		volume.stats, volume.chars)
{	object <- "volumes"
	# vertex attributes
	atts <- setdiff(colnames(char.stats), c(COL_NAME, COL_NAME_SHORT, COL_FREQ))
	att.nbr <- length(atts)
	
	# compute stats
	tlog(3,"Computing volume stats")
	volume.nbr <- nrow(volume.stats)
	
	# init stats table for volumes
	tmp <- data.frame(
			integer(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
			integer(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
			integer(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
			integer(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
			numeric(volume.nbr), numeric(volume.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(tmp) <- c(
			COL_PAGES, COL_PAGES_BY_SCENE, COL_PAGES_BY_CHAR,
			COL_SCENES, COL_SCENES_BY_PAGE, COL_SCENES_BY_CHAR, 
			COL_PANELS, COL_PANELS_BY_PAGE, COL_PANELS_BY_SCENE, COL_PANELS_BY_CHAR, 
			COL_CHARS, COL_CHARS_BY_PAGE, COL_CHARS_BY_SCENE, COL_CHARS_BY_PANEL,
			COL_CORR_PANELS_CHARS_BY_SCENE, COL_CORR_SCENES_PANELS_BY_CHAR
	)
	volume.stats <- cbind(volume.stats, tmp)
	
	# init stats tables for attributes
	volume.stats.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.stats[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=volume.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			volume.stats.atts[[att]] <- m
		}
	}
	
	# compute the stats for each volume
	tlog(4,"Processing each volume separately")
	for(v in 1:volume.nbr)
	{	vname <- volume.stats[v,COL_VOLUME]
		tlog(5,"Processing volume ",vname," (",v,"/",nrow(volume.stats),")")
		
		# corresponding pages
		idx.pg <- which(page.stats[,COL_VOLUME_ID]==v)
		pgn <- volume.stats[v,COL_PAGE_END] - volume.stats[v,COL_PAGE_START] + 1
		
		# corresponding characters
		char.volume <- c()
		for(i in idx.pg)
			char.volume <- union(char.volume, page.chars[[i]])
		idx.char <- match(char.volume, char.stats[,COL_NAME])
		chn <- length(char.volume)
		
		# corresponding scenes
		idx.sc <- which(scene.stats[,COL_VOLUME_ID]==v)
		scn <- length(idx.sc)
		
		# corresponding panels
		idx.pn <- which(panel.stats[,COL_VOLUME_ID]==v)
		pnn <- sum(page.stats[idx.pg,COL_PANELS])
		
		# char stats
		char.volume.pgn <- rep(0,chn)
		names(char.volume.pgn) <- char.volume
		for(p in idx.pg)
		{	for(cc in page.chars[[p]])
				char.volume.pgn[cc] <- char.volume.pgn[cc] + 1
		}
		char.volume.scn <- rep(0,chn)
		names(char.volume.scn) <- char.volume
		for(s in idx.sc)
		{	for(cc in scene.chars[[s]])
				char.volume.scn[cc] <- char.volume.scn[cc] + 1
		}
		char.volume.pnl <- rep(0,chn)
		names(char.volume.pnl) <- char.volume
		for(p in idx.pn)
		{	for(cc in panel.chars[[p]])
				char.volume.pnl[cc] <- char.volume.pnl[cc] + 1
		}
		
		# compute detailed stats
		vol.volume.stats <- volume.stats[v,]
		vol.page.stats <- page.stats[idx.pg,]
		vol.char.stats <- char.stats[idx.char,] 
		vol.scene.stats <- scene.stats[idx.sc,] 
		vol.scene.chars <- scene.chars[idx.sc]
		# compute panel stats
		tmp <- compute.stats.panels(
				page.stats=vol.page.stats, 
				scene.stats=vol.scene.stats, scene.chars=vol.scene.chars,
				char.stats=vol.char.stats, 
				volume.stats=vol.volume.stats, 
				cur.vol=v, cur.arc=NA
		)
		vol.panel.stats <- tmp$panel.stats
		vol.panel.stats.atts <- tmp$panel.stats.atts
		vol.panel.chars <- tmp$panel.chars
		# compute page stats
		tmp <- compute.stats.pages(
				panel.stats=vol.panel.stats, panel.chars=vol.panel.chars,
				page.stats=vol.page.stats, 
				scene.stats=vol.scene.stats, scene.chars=vol.scene.chars,  
				char.stats=vol.char.stats, 
				volume.stats=vol.volume.stats,  
				cur.vol=v, cur.arc=NA
		)
		vol.page.stats <- tmp$page.stats
		vol.page.stats.atts <- tmp$page.stats.atts
		vol.page.chars <- tmp$page.chars
		# compute scene stats
		tmp <- compute.stats.scenes(
				panel.stats=vol.panel.stats, panel.chars=vol.panel.chars,
				page.stats=vol.page.stats, page.chars=vol.page.chars, 
				scene.stats=vol.scene.stats, scene.chars=vol.scene.chars,
				char.stats=vol.char.stats, 
				volume.stats=vol.volume.stats, 
				cur.vol=v, cur.arc=NA
		)
		vol.scene.stats <- tmp$scene.stats
		vol.scene.stats.atts <- tmp$scene.stats.atts
		# compute character stats
		tmp <- compute.stats.chars(
				panel.stats=vol.panel.stats, panel.chars=vol.panel.chars,
				page.stats=vol.page.stats, page.chars=vol.page.chars, 
				scene.stats=vol.scene.stats, scene.chars=vol.scene.chars, 
				char.stats=vol.char.stats,
				volume.stats=vol.volume.stats, 
				cur.vol=v, cur.arc=NA
		)
		vol.char.stats <- tmp$char.stats
		vol.volume.chars <- tmp$volume.chars
		
		# update overall stat table
		volume.stats[v, COL_PAGES] <- pgn
		volume.stats[v, COL_PAGES_BY_SCENE] <- sum(scene.stats[idx.sc,COL_PAGES])/scn
		volume.stats[v, COL_PAGES_BY_CHAR] <- sum(char.volume.pgn)/chn
		#
		volume.stats[v, COL_SCENES] <- scn
		volume.stats[v, COL_SCENES_BY_PAGE] <- sum(page.stats[idx.pg,COL_SCENES])/pgn
		volume.stats[v, COL_SCENES_BY_CHAR] <- sum(char.volume.scn)/chn
		#
		volume.stats[v, COL_PANELS] <- pnn
		volume.stats[v, COL_PANELS_BY_PAGE] <- pnn/pgn
		volume.stats[v, COL_PANELS_BY_SCENE] <- sum(scene.stats[idx.sc,COL_PANELS])/scn
		volume.stats[v, COL_PANELS_BY_CHAR] <- sum(char.volume.pnl)/chn
		#
		volume.stats[v, COL_CHARS] <- chn
		volume.stats[v, COL_CHARS_BY_PAGE] <- sum(page.stats[idx.pg,COL_CHARS])/pgn
		volume.stats[v, COL_CHARS_BY_SCENE] <- sum(scene.stats[idx.sc,COL_CHARS])/scn
		volume.stats[v, COL_CHARS_BY_PANEL] <- sum(panel.stats[idx.pn,COL_CHARS])/pnn
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(volume.chars[[v]], char.stats[,COL_NAME])
			for(att in atts)
			{	m <- volume.stats.atts[[att]]
				tt <- table(char.stats[idx,att])
				m[v,names(tt)] <- tt
				volume.stats.atts[[att]] <- m
			}
		}
		
		# density plot: chars vs. panels (by scene)
		file <- get.path.stat.corpus(vol=vname, desc="comparison_chars-scenes_vs_panels-scenes")
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
		volume.stats[v, COL_CORR_PANELS_CHARS_BY_SCENE] <- val
		tlog(6,"Correlation between characters/scene and panels/scene: ", val)
		
		# density: scenes vs. panels (by char)
		val <- cor(char.stats[idx.char,COL_SCENES],char.stats[idx.char,COL_PANELS])
		volume.stats[v, COL_CORR_SCENES_PANELS_BY_CHAR] <- val
		tlog(6,"Correlation between scenes/character and panels/character: ", val)
		
		# attribute stats
		tlog(6,"Computing attribute stats")
		for(a in 1:length(atts))
		{	tlog(5,"Computing attribute ",atts[a]," (",a,"/",length(atts),")")
			
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
						col=MAIN_COLOR
					)
				dev.off()
			}
			
			# others?
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
					col=MAIN_COLOR
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
	write.csv(x=data, paste0(file,"_rawvals.csv"), row.names=FALSE)#, col.names=FALSE)
	if(length(unique(data))>1)
	{	for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					ml <- "Character number distribution over volumes"
					xl <- "Number of characters by volume"
#					# histogram
#					h <- hist(
#						data,
#						breaks=0:max(data),
##						col=MAIN_COLOR,
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
#					plot(x, y, col=MAIN_COLOR, xlab=xl, ylab="Density", main=ml, log="xy")
					# complementary cumulative distribution function
					plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE)
				dev.off()
		}
		# check distribution
		distr.stats <- test.disc.distr(data, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}
	
	# TODO plot the distributions obtained for all volumes on the same plot? (using lines instead of points)

	result <- list(volume.stats=volume.stats, volume.stats.atts=volume.stats.atts)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the narrative arcs.
#
# panel.stats: previously computed panel statistics.
# panel.chars: characters present in each panel.
# page.stats: previously computed page statistics.
# page.chars: characters present in each page.
# scene.stats: previously computed scene statistics.
# scene.chars: characters present in each scene.
# char.stats: previously computed character statistics.
# volume.stats: previously computed volume statistics.
# volume.chars: characters present in each volume.
#
# returns: a list containing the table with overall arc stats (arc.stats), and
#          the arc stats by vertex attribute values (arc.stats.atts).
###############################################################################
compute.stats.arc <- function(
		panel.stats=panel.stats, panel.chars=panel.chars,
		page.stats=page.stats, page.chars=page.chars, 
		scene.stats=scene.stats, scene.chars=scene.chars, 
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.chars=volume.chars)
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
	
	result <- list(arc.stats=arc.stats, arc.stats.atts=arc.stats.atts, arc.chars=arc.chars)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the volumes.
#
# panel.stats: previously computed panel statistics.
# panel.chars: characters present in each panel.
# page.stats: previously computed page statistics.
# page.chars: characters present in each page.
# scene.stats: previously computed scene statistics.
# scene.chars: characters present in each scene.
# char.stats: previously computed character statistics.
# volume.stats: previously computed volume statistics.
# volume.chars: characters present in each volume.
# arc.stats: previously computed arc statistics.
# arc.chars: characters present in each narrative arc.
#
# returns: a list containing the table with overall volume stats (stats.chars), and
#          the volume stats by vertex attribute values (stats.chars.atts).
###############################################################################
compute.stats.overall <- function(
		panel.stats=panel.stats, panel.chars=panel.chars,
		page.stats=page.stats, page.chars=page.chars, 
		scene.stats=scene.stats, scene.chars=scene.chars,
		char.stats=char.stats, 
		volume.stats=volume.stats, volume.chars=volume.chars, 
		arc.stats=arc.stats, arc.chars=arc.chars)
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
	
	
	result <- list(overall.stats=overall.stats, overall.stats.atts=overall.stats.atts)
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
	volume.stats <- data$volume.stats # TODO
	page.stats <- data$page.stats
	char.stats <- data$char.stats
	scene.stats <- data$scene.stats
	scene.chars <- data$scene.chars
	
	# compute panel stats
	tmp <- compute.stats.panel(
			page.stats=page.stats, 
			scene.stats=scene.stats, scene.chars=scene.chars,
			char.stats=char.stats, 
			volume.stats=volume.stats, 
			cur.vol=NA, cur.arc=NA
	)
	panel.stats <- tmp$panel.stats
	panel.stats.atts <- tmp$panel.stats.atts
	panel.chars <- tmp$panel.chars
	
	# compute page stats
	tmp <- compute.stats.page(
			panel.stats=panel.stats, panel.chars=panel.chars,
			page.stats=page.stats, 
			scene.stats=scene.stats, scene.chars=scene.chars, 
			char.stats=char.stats, 
			volume.stats=volume.stats, 
			cur.vol=NA, cur.arc=NA
	)
	page.stats <- tmp$page.stats
	page.stats.atts <- tmp$page.stats.atts
	page.chars <- tmp$page.chars
	
	# compute scene stats
	tmp <- compute.stats.scene(
			panel.stats=panel.stats, panel.chars=panel.chars,
			page.stats=page.stats, page.chars=page.chars, 
			scene.stats=scene.stats, scene.chars=scene.chars, 
			char.stats=char.stats, 
			volume.stats=volume.stats, 
			cur.vol=NA, cur.arc=NA
	)
	scene.stats <- tmp$scene.stats
	scene.stats.atts <- tmp$scene.stats.atts
	
	# compute character stats
	tmp <- compute.stats.char(
			panel.stats=panel.stats, panel.chars=panel.chars,
			page.stats=page.stats, page.char=page.char, 
			scene.stats=scene.stats, scene.chars=scene.chars, 
			char.stats=char.stats, 
			volume.stats=volume.stats, 
			cur.vol=NA, cur.arc=NA,
			filtered=FALSE
	)
	char.stats <- tmp$char.stats
	volume.chars <- tmp$volume.chars
	# possibly process filtered characters (just for plots)
	if(COL_FILTERED %in% colnames(char.stats))
	{	tmp <- compute.stats.char(
				panel.stats=panel.stats, panel.chars=panel.chars, 
				page.stats=page.stats, page.chars=page.chars,  
				scene.stats=scene.stats, scene.chars=scene.chars, 
				char.stats=char.stats, 
				volume.stats=volume.stats, 
				cur.vol=NA, cur.arc=NA,
				filtered=TRUE
		)
	}
	
	# compute volume stats
	tmp <- compute.stats.volume(
			panel.stats=panel.stats, panel.chars=panel.chars,
			page.stats=page.stats, page.chars=page.chars, 
			scene.stats=scene.stats, scene.chars=scene.chars,
			char.stats=char.stats,
			volume.stats=volume.stats, volume.chars=volume.chars
	)
	volume.stats <- tmp$volume.stats
	volume.stats.atts <- tmp$volume.stats.atts
	
	# compute arc stats
	tmp <- compute.stats.arc(
			panel.stats=panel.stats, panel.chars=panel.chars,
			page.stats=page.stats, page.chars=page.chars, 
			scene.stats=scene.stats, scene.chars=scene.chars, 
			char.stats=char.stats, 
			volume.stats=volume.stats, volume.chars=volume.chars
	)
	arc.stats <- tmp$arc.stats
	arc.stats.atts <- tmp$arc.stats.atts
	arc.chars <- tmp$arc.chars
	
	# compute overall stats
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
	
	# record all tables
	# TODO
	
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
