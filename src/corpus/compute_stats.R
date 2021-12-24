# This script contains functions used to compute some statistics and generate
# plots related to the corpus.
# 
# Vincent Labatut
# 05/2021
###############################################################################




###############################################################################
# Computes and records some statistics regarding the panels.
#
# volume.info: table describing the series volumes.
# page.info: table describing all the pages constituting the BD series.
# char.info: table describing all the characters occurring in the BD series.
# stats.scenes: previously computed scene statistics.
# char.scenes: characters present in each scene.
# cur.vol: NA (regular mode) or Id of the volume specifically processed.
# cur.arc: NA (regular mode) or Id of the arc specifically processed.
#
# returns: a list containing the table with overall panel stats (stats.panels), the 
#          panel stats by vertex attribute values (stats.panels.atts), and the list
#          of characters for each panel.
###############################################################################
compute.stats.panels <- function(
		volume.info, page.info, char.info, 
		stats.scenes, 
		char.scenes,
		cur.vol=NA, cur.arc=NA)
{	object <- "panels"
	# vertex attributes
	atts <- setdiff(colnames(char.info), c(COL_CHAR_NAME, COL_CHAR_SHORT_NAME, COL_CHAR_FREQ))
	att.nbr <- length(atts)
	# panel positions
	pages.end.panel.ids <- c(
			page.info[2:nrow(page.info),COL_PAGES_START_PANEL_ID]-1,
			page.info[nrow(page.info),COL_PAGES_START_PANEL_ID]+page.info[nrow(page.info),COL_PAGES_PANELS]
	)
	
	# volume name
	if(!is.na(cur.vol))
		vname <- volume.info[1, COL_VOLS_VOLUME]
	else
		vname <- NA
	
	# compute stats
	tlog(3,"Computing panel stats (cur.vol=",cur.vol," vname=",vname,")")
	
	# list of concerned panel ids
	panel.ids <- c()
	for(s in 1:nrow(stats.scenes))
	{	panel.ids <- union(panel.ids, 
				seq(stats.scenes[s,COL_STATS_START_PANEL_ID], stats.scenes[s,COL_STATS_END_PANEL_ID])
		)
	}
	panel.nbr <- length(panel.ids)
	
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
	# init stats tables for attributes
	stats.panels.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.info[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=panel.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			stats.panels.atts[[att]] <- m
		}
	}
	
	# compute the stats for each panel
	tlog(4,"Processing each panel separately")
	char.panels <- lapply(1:panel.nbr, function(x) c())
	for(p in 1:panel.nbr)
	{	tlog(5,"Processing panel id ",panel.ids[p]," (",p,"/",panel.nbr,")")
		
		# find the scenes containing this panel
		ss <- which(stats.scenes[,COL_STATS_START_PANEL_ID]<=panel.ids[p]
						& stats.scenes[,COL_STATS_END_PANEL_ID]>=panel.ids[p])
		cur.page <- which(page.info[,COL_PAGES_START_PANEL_ID]<=panel.ids[p]
						& pages.end.panel.ids>=panel.ids[p])
		pos <- panel.ids[p] - page.info[cur.page, COL_PAGES_START_PANEL_ID] + 1
		match.start <- pos==1
		match.end <- pos==page.info[cur.page,COL_PAGES_PANELS]
		for(s in ss)
		{	if(length(char.scenes[[s]])>0)
				char.panels[[p]] <- union(char.panels[[p]], char.scenes[[s]])
		}
		
		# update overall stat table
		stats.panels[p, COL_STATS_VOLUME] <- stats.scenes[ss[1],COL_STATS_VOLUME]
		stats.panels[p, COL_STATS_VOLUME_ID] <- stats.scenes[ss[1],COL_STATS_VOLUME_ID]
		stats.panels[p, COL_STATS_PAGE] <- page.info[cur.page,COL_PAGES_PAGE]
		stats.panels[p, COL_STATS_PAGE_ID] <- page.info[cur.page,COL_PAGES_PAGE_ID]
		stats.panels[p, COL_STATS_PANEL] <- pos
		stats.panels[p, COL_STATS_PANEL_ID] <- panel.ids[p]
		stats.panels[p, COL_STATS_CHARS] <- length(char.panels[[p]])
		stats.panels[p, COL_STATS_MATCH_START] <- match.start
		stats.panels[p, COL_STATS_MATCH_END] <- match.end
		stats.panels[p, COL_STATS_MATCH_BOTH] <- match.start && match.end
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(char.panels[[p]], char.info[,COL_CHAR_NAME])
			for(att in atts)
			{	m <- stats.panels.atts[[att]]
				tt <- table(char.info[idx,att])
				m[p,names(tt)] <- tt
				stats.panels.atts[[att]] <- m
			}
		}
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels")
	tlog(4,"Recording in ",file)
	write.csv(x=stats.panels, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=stats.panels.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	
	# distributions of character numbers (overall)
	vals <- table(stats.panels[,COL_STATS_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_CHARS, COL_STATS_PANELS, "Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_char_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.panels[,COL_STATS_CHARS]
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
#			plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
	
	# distribution of character numbers (by attribute)
	if(att.nbr>0)
	{	for(att in atts)
		{	pal <- get.palette(ncol(stats.panels.atts[[att]]))[1:ncol(stats.panels.atts[[att]])]
			file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_char_nbr", att=att)
			data <- stats.panels.atts[[att]]
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
						legend.text=colnames(stats.panels.atts[[att]])
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 1:ncol(stats.panels.atts[[att]]))
			{	data <- stats.panels.atts[[att]][,d]
				file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="panels_distrib_char_nbr", att=att, val=colnames(stats.panels.atts[[att]])[d])
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
							main=paste0(ml," val=",colnames(stats.panels.atts[[att]])[d],")"),
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
	vals["Both"] <- length(which(stats.panels[, COL_STATS_MATCH_BOTH]))
	vals["Starts page"] <- length(which(stats.panels[, COL_STATS_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(stats.panels[, COL_STATS_MATCH_END])) - vals["Both"]
	vals["None"] <- nrow(stats.panels) - vals["Both"] - vals["Starts page"] - vals["Ends page"]
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
	
	result <- list(stats.panels=stats.panels, stats.panels.atts=stats.panels.atts, char.panels=char.panels)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the pages.
#
# volume.info: table describing the series volumes.
# page.info: table describing all the pages constituting the BD series.
# char.info: table describing all the characters occurring in the BD series.
# stats.scenes: previously computed scene statistics.
# stats.panels: previously computed panel statistics.
# char.scenes: characters present in each scene.
# char.panels: characters present in each panel.
# cur.vol: NA (regular mode) or Id of the volume specifically processed.
# cur.arc: NA (regular mode) or Id of the arc specifically processed.
#
# returns: a list containing the table with overall page stats (stats.panels), the 
#          page stats by vertex attribute values (stats.panels.atts), and the list
#          of characters for each page.
###############################################################################
compute.stats.pages <- function(
		volume.info, page.info, char.info, 
		stats.scenes, stats.panels, 
		char.scenes, char.panels,
		cur.vol=NA, cur.arc=NA)
{	object <- "pages"
	# vertex attributes
	atts <- setdiff(colnames(char.info), c(COL_CHAR_NAME, COL_CHAR_SHORT_NAME, COL_CHAR_FREQ))
	att.nbr <- length(atts)
	
	# volume name
	if(!is.na(cur.vol))
		vname <- volume.info[1, COL_VOLS_VOLUME]
	else
		vname <- NA
	
	# compute stats
	page.ids <- sort(unique(stats.panels[,COL_STATS_PAGE_ID]))
	page.nbr <- length(page.ids)
	
	# list the characters by page
	tlog(3,"Listing the characters by page")
	char.pages <- lapply(1:page.nbr, function(x) c())
	first.pg.id <- stats.scenes[1,COL_STATS_START_PAGE_ID]
	for(s in 1:nrow(stats.scenes))
	{	tlog(5,"Processing scene id ",s,"/",nrow(stats.scenes))
		
		# find the pages containing the scene
		start.page.id <- stats.scenes[s,COL_STATS_START_PAGE_ID]
		end.page.id <- stats.scenes[s,COL_STATS_END_PAGE_ID]
		sc.page.ids <- start.page.id:end.page.id
		for(page.id in sc.page.ids)
		{	if(length(char.scenes[[s]])>0)
			{	i <- which(page.info[,COL_PAGES_PAGE_ID]==page.id)
				char.pages[[i]] <- union(char.pages[[i]], char.scenes[[s]])
			}
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
	# init stats tables for attributes
	stats.pages.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.info[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=page.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			stats.pages.atts[[att]] <- m
		}
	}
	
	# compute the stats for each page
	tlog(4,"Processing each page separately")
	for(p in 1:page.nbr)
	{	tlog(5,"Processing page id ",page.ids[p]," (",p,"/",page.nbr,")")
		
		# number of scenes overlapping the page
		scn <- length(which(
						stats.scenes[,COL_STATS_START_PAGE_ID]<=page.ids[p]
								& stats.scenes[,COL_STATS_END_PAGE_ID]>=page.ids[p]
				))
		
		# update overall stat table
		stats.pages[p, COL_STATS_VOLUME] <- page.info[p,COL_PAGES_VOLUME]
		stats.pages[p, COL_STATS_VOLUME_ID] <- page.info[p,COL_PAGES_VOLUME_ID]
		stats.pages[p, COL_STATS_PAGE] <- page.info[p,COL_PAGES_PAGE]
		stats.pages[p, COL_STATS_PAGE_ID] <- page.ids[p]
		stats.pages[p, COL_STATS_SCENES] <- scn
		stats.pages[p, COL_STATS_PANELS] <- page.info[p,COL_PAGES_PANELS]
		stats.pages[p, COL_STATS_CHARS] <- length(char.pages[[p]])
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(char.pages[[p]], char.info[,COL_CHAR_NAME])
			for(att in atts)
			{	m <- stats.pages.atts[[att]]
				tt <- table(char.info[idx,att])
				m[p,names(tt)] <- tt
				stats.pages.atts[[att]] <- m
			}
		}
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages")
	tlog(4,"Recording in ",file)
	write.csv(x=stats.pages, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=stats.pages.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	
	# distributions of scene numbers
	vals <- table(stats.pages[,COL_STATS_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_SCENES, COL_STATS_PAGES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_scene_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.pages[,COL_STATS_SCENES]
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
#			plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
	
	# distributions of panel numbers
	vals <- table(stats.pages[,COL_STATS_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PANELS, COL_STATS_PAGES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_panel_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.pages[,COL_STATS_PANELS]
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
#			plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
	
	# distributions of character numbers (overall)
	vals <- table(stats.pages[,COL_STATS_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_CHARS, COL_STATS_PAGES, "Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_char_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.pages[,COL_STATS_CHARS]
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
#			plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
	
	# distribution of character numbers (by attribute)
	if(att.nbr>0)
	{	for(att in atts)
		{	pal <- get.palette(ncol(stats.pages.atts[[att]]))[1:ncol(stats.pages.atts[[att]])]
			file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_char_nbr", att=att)
			data <- stats.pages.atts[[att]]
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
						legend.text=colnames(stats.pages.atts[[att]])
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 1:ncol(stats.pages.atts[[att]]))
			{	data <- stats.pages.atts[[att]][,d]
				if(any(data!=0))
				{	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="pages_distrib_char_nbr", att=att, val=colnames(stats.pages.atts[[att]])[d])
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
								main=paste0(ml," - val=",colnames(stats.pages.atts[[att]])[d],")"),
								freq=FALSE,
								#plot=FALSE
							)
						dev.off()
					}
				}
			}
		}
	}
	
	result <- list(stats.pages=stats.pages, stats.pages.atts=stats.pages.atts, char.pages=char.pages)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the scenes.
#
# volume.info: table describing the series volumes.
# page.info: table describing all the pages constituting the BD series.
# char.info: table describing all the characters occurring in the BD series.
# stats.pages: previously computed page statistics.
# stats.scenes: previously computed scene statistics.
# stats.panels: previously computed panel statistics.
# char.page: characters present in each page.
# char.scenes: characters present in each scene.
# char.panels: characters present in each panel.
# cur.vol: NA (regular mode) or Id of the volume specifically processed.
# cur.arc: NA (regular mode) or Id of the arc specifically processed.
#
# returns: a list containing the table with overall scene stats (stats.scenes) and the 
#          scene stats by vertex attribute values (stats.scenes.atts).
###############################################################################
compute.stats.scenes <- function(
		volume.info, page.info, char.info, 
		stats.pages, stats.scenes, stats.panels, 
		char.pages, char.scenes, char.panels,
		cur.vol=NA, cur.arc=NA)
{	object <- "scenes"
	# vertex attributes
	atts <- setdiff(colnames(char.info), c(COL_CHAR_NAME, COL_CHAR_SHORT_NAME, COL_CHAR_FREQ))
	att.nbr <- length(atts)
	
	# volume name
	if(!is.na(cur.vol))
		vname <- volume.info[1, COL_VOLS_VOLUME]
	else
		vname <- NA
	
	# compute stats
	tlog(3,"Computing scene stats")
	scene.nbr <- nrow(stats.scenes)
	
	# init stats tables for attributes
	stats.scenes.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.info[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=scene.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			stats.scenes.atts[[att]] <- m
		}
	}
	
	# compute additional stats for each scene
	tlog(4,"Processing each scene separately")
	for(s in 1:scene.nbr)
	{	tlog(5,"Processing scene ",s,"/",scene.nbr)
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(char.scenes[[s]], char.info[,COL_CHAR_NAME])
			for(att in atts)
			{	m <- stats.scenes.atts[[att]]
				tt <- table(char.info[idx,att])
				m[s,names(tt)] <- tt
				stats.scenes.atts[[att]] <- m
			}
		}
	}
	
	# record scene stats
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes")
	tlog(4,"Recording in ",file)
	write.csv(x=stats.scenes, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=stats.scenes.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	
	# distributions of panel numbers
	vals <- table(stats.scenes[,COL_STATS_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PANELS, COL_STATS_SCENES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_panel_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.scenes[,COL_STATS_PANELS]
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
			plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
#	# check distribution
#	test.disc.distr(data)
	
	# distributions of character numbers (overall)
	vals <- table(stats.scenes[,COL_STATS_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_CHARS, COL_STATS_SCENES, "Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_char_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.scenes[,COL_STATS_CHARS]
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
#			plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
	
	# distribution of character numbers (by attribute)
	if(att.nbr>0)
	{	for(att in atts)
		{	pal <- get.palette(ncol(stats.scenes.atts[[att]]))[1:ncol(stats.scenes.atts[[att]])]
			file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_char_nbr", att=att)
			data <- stats.scenes.atts[[att]]
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
						legend.text=colnames(stats.scenes.atts[[att]])
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 1:ncol(stats.scenes.atts[[att]]))
			{	data <- stats.scenes.atts[[att]][,d]
				file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_char_nbr", att=att, val=colnames(stats.scenes.atts[[att]])[d])
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
							main=paste0(ml," - val=",colnames(stats.scenes.atts[[att]])[d],")"),
							freq=FALSE,
							#plot=FALSE
						)
					dev.off()
				}
			}
		}
	}
	
	# distributions of page numbers
	vals <- table(stats.scenes[,COL_STATS_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PANELS, COL_STATS_PAGES,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="scenes_distrib_page_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.scenes[,COL_STATS_PAGES]
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
				plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
#	# check distribution
#	test.disc.distr(data)
	
	# distribution of scene positions
	vals <- c()
	vals["Both"] <- length(which(stats.scenes[, COL_STATS_MATCH_BOTH]))
	vals["Starts page"] <- length(which(stats.scenes[, COL_STATS_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(stats.scenes[, COL_STATS_MATCH_END])) - vals["Both"]
	vals["None"] <- nrow(stats.scenes) - vals["Both"] - vals["Starts page"] - vals["Ends page"]
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
	
	result <- list(stats.scenes=stats.scenes, stats.scenes.atts=stats.scenes.atts)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the characters.
#
# volume.info: table describing the series volumes.
# page.info: table describing all the pages constituting the BD series.
# char.info: table describing all the characters occurring in the BD series.
# stats.pages: previously computed page statistics.
# stats.scenes: previously computed scene statistics.
# stats.panels: previously computed panel statistics.
# char.pages: characters present in each page.
# char.scenes: characters present in each scene.
# char.panels: characters present in each panel.
# cur.vol: NA (regular mode) or Id of the volume specifically processed.
# cur.arc: NA (regular mode) or Id of the arc specifically processed.
# filtered: whether to process only the filtered characters.
#
# returns: a list containing the table with overall character stats (stats.chars) and
#		   the list of characters for each volume.
###############################################################################
compute.stats.chars <- function(
		volume.info, page.info, char.info, 
		stats.pages, stats.scenes, stats.panels,
		char.pages, char.scenes, char.panels,
		cur.vol=NA, cur.arc=NA,
		filtered=FALSE)
{	object <- "characters"
	# vertex attributes
	atts <- setdiff(colnames(char.info), c(COL_CHAR_NAME, COL_CHAR_SHORT_NAME, COL_CHAR_FREQ, COL_CHAR_FILTERED, COL_CHAR_NAMED))
	att.nbr <- length(atts)
	
	# volume name
	if(!is.na(cur.vol))
		vname <- volume.info[1, COL_VOLS_VOLUME]
	else
		vname <- NA
	
	# compute stats
	tlog(3,"Computing ",if(filtered) "filtered" else "all"," character stats")
	unique.chars <- sort(unique(unlist(char.scenes)))
	if(filtered)
	{	idx.rm <- which(char.info[,COL_CHAR_FILTERED])
		idx.kp <- which(!char.info[,COL_CHAR_FILTERED])
		unique.chars <- setdiff(unique.chars, char.info[idx.rm ,COL_CHAR_NAME])
	}
	char.nbr <- length(unique.chars)
	
	#  identify the characters in each volume
	tlog(4,"Identify the",if(filtered) " filtered" else ""," characters in each volume")
	volume.ids <- volume.info[,COL_VOLS_VOLUME_ID]
	volume.nbr <- length(volume.ids)
	char.volumes <- lapply(1:volume.nbr, function(x) c())
	for(v in 1:volume.nbr)
	{	tlog(5,"Processing volume id ",volume.ids[v]," (",v,"/",volume.nbr,")")
		
		# find the pages contained in the volume
		idx.pg <- which(page.info[,COL_PAGES_VOLUME_ID]==volume.ids[v])
		for(p in idx.pg)
		{	if(length(char.pages[[p]])>0)
				char.volumes[[v]] <- union(char.volumes[[v]], char.pages[[p]])
		}
	}
	
	# init stats table for characters
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
	tlog(4,"Processing each",if(filtered) " filtered" else ""," character separately")
	for(c in 1:char.nbr)
	{	tlog(5,"Processing",if(filtered) " filtered" else ""," character \"",unique.chars[c],"\" (",c,"/",char.nbr,")")
		
		# update overall stat table
		stats.chars[c, COL_STATS_CHAR] <- unique.chars[c]
		stats.chars[c, COL_STATS_VOLUMES] <- sum(sapply(char.volumes, function(char.volume) unique.chars[c] %in% char.volume))
		stats.chars[c, COL_STATS_PAGES] <- sum(sapply(char.pages, function(char.page) unique.chars[c] %in% char.page))
		stats.chars[c, COL_STATS_SCENES] <- sum(sapply(char.scenes, function(char.scene) unique.chars[c] %in% char.scene))
		stats.chars[c, COL_STATS_PANELS] <- sum(sapply(char.panels, function(char.panel) unique.chars[c] %in% char.panel))
	}
	
	# record stats
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("characters", if(filtered) "_filtered" else ""))
	tlog(4,"Recording in ",file)
	write.csv(x=stats.chars, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	
	# distributions of volume numbers
	vals <- table(stats.chars[,COL_STATS_VOLUMES])
	vals <- data.frame(names(vals), vals, 100*vals/sum(vals), stringsAsFactors=FALSE, check.names=FALSE)
	colnames(vals) <- c(COL_STATS_VOLUMES, COL_STATS_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("chars", if(filtered) "_filtered" else "","_distrib_volume_nbr"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.chars[,COL_STATS_VOLUMES]
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
					plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
				dev.off()
		}
		# check distribution
#		tmp <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, sims=100, plot.file=paste0(file,"_disttest"))
	}
	
	# distributions of page numbers
	vals <- table(stats.chars[,COL_STATS_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PAGES, COL_STATS_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("chars", if(filtered) "_filtered" else "","_distrib_page_nbr"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.chars[,COL_STATS_PAGES]
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
				plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
	# check distribution
#	tmp <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, sims=100, plot.file=paste0(file,"_disttest"))
	
	# distributions of scene numbers
	vals <- table(stats.chars[,COL_STATS_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_SCENES, COL_STATS_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("chars", if(filtered) "_filtered" else "","_distrib_scene_nbr"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.chars[,COL_STATS_SCENES]
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
				plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
	# check distribution
#	tmp <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, sims=100, plot.file=paste0(file,"_disttest"))
	
	# distributions of panel numbers
	vals <- table(stats.chars[,COL_STATS_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_STATS_PANELS, COL_STATS_CHARS,"Proportion")
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc=paste0("chars", if(filtered) "_filtered" else "","_distrib_panel_nbr"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.chars[,COL_STATS_PANELS]
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
				plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
		dev.off()
	}
	# check distribution
#	tmp <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, sims=100, plot.file=paste0(file,"_disttest"))
	
	# behavior of character filtering (trying to identify extras)
	thresholds <- seq(0, 10)	#max(char.info[,COL_CHAR_FREQ]))
	char.nums <- sapply(thresholds, function(t) c(table(factor(char.info[char.info[,COL_CHAR_FREQ]>=t,COL_CHAR_NAMED], levels=c("TRUE","FALSE")))))
	# generate barplots
	file <- get.path.stat.corpus(object=object, vol=vname, arc=cur.arc, desc="chars_filtering_by_occurences")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			cols <- get.palette(values=2)
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
	thresholds <- seq(0, 10)	#max(char.info[,COL_CHAR_FREQ]))
	char.nums <- sapply(thresholds, function(t) c(table(factor(char.info[char.info[,COL_CHAR_FREQ]>=t,COL_CHAR_NAMED], levels=c("TRUE","FALSE")))))
	tab <- cbind(thresholds, t(char.nums))
	colnames(tab) <- c("Min occurrences","Named","Unnamed")
	write.csv(x=tab, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	
	result <- list(stats.chars=stats.chars, char.volumes=char.volumes)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the volumes.
#
# volume.info: table describing the series volumes.
# page.info: table describing all the pages constituting the BD series.
# char.info: table describing all the characters occurring in the BD series.
# stats.pages: previously computed page statistics.
# stats.scenes: previously computed scene statistics.
# stats.panels: previously computed panel statistics.
# stats.chars: previously computed character statistics.
# char.volumes: characters present in each volume.
# char.pages: characters present in each page.
# char.scenes: characters present in each scene.
# char.panels: characters present in each panel.
#
# returns: a list containing the table with overall volume stats (stats.chars), and
#          the volume stats by vertex attribute values (stats.chars.atts).
###############################################################################
compute.stats.volumes <- function(
		volume.info, page.info, char.info, 
		stats.pages, stats.scenes, stats.panels, stats.chars,
		char.volumes, char.pages, char.scenes, char.panels)
{	object <- "volumes"
	# vertex attributes
	atts <- setdiff(colnames(char.info), c(COL_CHAR_NAME, COL_CHAR_SHORT_NAME, COL_CHAR_FREQ))
	att.nbr <- length(atts)
	
	# compute stats
	tlog(3,"Computing volume stats")
	volume.nbr <- nrow(volume.info)
	
	# init stats table for volumes
	stats.volumes <- data.frame(
			character(volume.nbr), integer(volume.nbr),
			integer(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
			integer(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
			integer(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
			integer(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), numeric(volume.nbr), 
			numeric(volume.nbr), numeric(volume.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(stats.volumes) <- c(
			COL_STATS_VOLUME, COL_STATS_VOLUME_ID,
			COL_STATS_PAGES, COL_STATS_PAGES_BY_SCENE, COL_STATS_PAGES_BY_CHAR,
			COL_STATS_SCENES, COL_STATS_SCENES_BY_PAGE, COL_STATS_SCENES_BY_CHAR, 
			COL_STATS_PANELS, COL_STATS_PANELS_BY_PAGE, COL_STATS_PANELS_BY_SCENE, COL_STATS_PANELS_BY_CHAR, 
			COL_STATS_CHARS, COL_STATS_CHARS_BY_PAGE, COL_STATS_CHARS_BY_SCENE, COL_STATS_CHARS_BY_PANEL,
			COL_STATS_CORR_PANELS_CHARS_BY_SCENE, COL_STATS_CORR_SCENES_PANELS_BY_CHAR
	)
	
	# init stats tables for attributes
	stats.volumes.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.info[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=volume.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			stats.volumes.atts[[att]] <- m
		}
	}
	
	# compute the stats for each volume
	tlog(4,"Processing each volume separately")
	for(v in 1:volume.nbr)
	{	vname <- volume.info[v,COL_VOLS_VOLUME]
		tlog(5,"Processing volume ",vname," (",v,"/",nrow(volume.info),")")
		
		# corresponding pages
		idx.pg <- which(page.info[,COL_PAGES_VOLUME_ID]==v)
		pgn <- volume.info[v,COL_VOLS_PAGE_END] - volume.info[v,COL_VOLS_PAGE_START] + 1
		
		# corresponding characters
		char.volume <- c()
		for(i in idx.pg)
			char.volume <- union(char.volume, char.pages[[i]])
		idx.char <- match(char.volume, char.info[,COL_CHAR_NAME])
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
		
		# compute detailed stats
		vol.volume.info <- volume.info[v,]
		vol.page.info <- page.info[idx.pg,]
		vol.char.info <- char.info[idx.char,] 
		vol.stats.scenes <- stats.scenes[idx.sc,] 
		vol.char.scenes <- char.scenes[idx.sc]
		# compute panel stats
		tmp <- compute.stats.panels(
				vol.volume.info, vol.page.info, vol.char.info, 
				vol.stats.scenes, 
				vol.char.scenes,
				cur.vol=v, cur.arc=NA
		)
		vol.stats.panels <- tmp$stats.panels
		vol.stats.panels.atts <- tmp$stats.panels.atts
		vol.char.panels <- tmp$char.panels
		# compute page stats
		tmp <- compute.stats.pages(
				vol.volume.info, vol.page.info, vol.char.info, 
				vol.stats.scenes, vol.stats.panels, 
				vol.char.scenes, vol.char.panels,
				cur.vol=v, cur.arc=NA
		)
		vol.stats.pages <- tmp$stats.pages
		vol.stats.pages.atts <- tmp$stats.pages.atts
		vol.char.pages <- tmp$char.pages
		# compute scene stats
		tmp <- compute.stats.scenes(
				vol.volume.info, vol.page.info, vol.char.info, 
				vol.stats.pages, vol.stats.scenes, vol.stats.panels, 
				vol.char.pages, vol.char.scenes, vol.char.panels,
				cur.vol=v, cur.arc=NA
		)
		vol.stats.scenes <- tmp$stats.scenes
		vol.stats.scenes.atts <- tmp$stats.scenes.atts
		# compute character stats
		tmp <- compute.stats.chars(
				vol.volume.info, vol.page.info, vol.char.info, 
				vol.stats.pages, vol.stats.scenes, vol.stats.panels, 
				vol.char.pages, vol.char.scenes, vol.char.panels,
				cur.vol=v, cur.arc=NA
		)
		vol.stats.chars <- tmp$stats.chars
		vol.char.volumes <- tmp$char.volumes
		
		# update overall stat table
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
		stats.volumes[v, COL_STATS_PANELS_BY_PAGE] <- pnn/pgn
		stats.volumes[v, COL_STATS_PANELS_BY_SCENE] <- sum(stats.scenes[idx.sc,COL_STATS_PANELS])/scn
		stats.volumes[v, COL_STATS_PANELS_BY_CHAR] <- sum(char.volume.pnl)/chn
		#
		stats.volumes[v, COL_STATS_CHARS] <- chn
		stats.volumes[v, COL_STATS_CHARS_BY_PAGE] <- sum(stats.pages[idx.pg,COL_STATS_CHARS])/pgn
		stats.volumes[v, COL_STATS_CHARS_BY_SCENE] <- sum(stats.scenes[idx.sc,COL_STATS_CHARS])/scn
		stats.volumes[v, COL_STATS_CHARS_BY_PANEL] <- sum(stats.panels[idx.pn,COL_STATS_CHARS])/pnn
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(char.volumes[[v]], char.info[,COL_CHAR_NAME])
			for(att in atts)
			{	m <- stats.volumes.atts[[att]]
				tt <- table(char.info[idx,att])
				m[v,names(tt)] <- tt
				stats.volumes.atts[[att]] <- m
			}
		}
		
		# density plot: chars vs. panels (by scene)
		file <- get.path.stat.corpus(vol=vname, desc="comparison_chars-scenes_vs_panels-scenes")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				xvals <- stats.scenes[idx.sc,COL_STATS_CHARS]
				yvals <- stats.scenes[idx.sc,COL_STATS_PANELS]
				xlab <- "Number of characters by scene"
				ylab <- "Number of panels by scene"
				p=ggplot(stats.scenes[idx.sc,], aes(x=xvals, y=yvals)) +
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
		val <- cor(stats.scenes[idx.sc,COL_STATS_CHARS],stats.scenes[idx.sc,COL_STATS_PANELS])
		stats.volumes[v, COL_STATS_CORR_PANELS_CHARS_BY_SCENE] <- val
		tlog(6,"Correlation between characters/scene and panels/scene: ", val)
		
		# density: scenes vs. panels (by char)
		val <- cor(stats.chars[idx.char,COL_STATS_SCENES],stats.chars[idx.char,COL_STATS_PANELS])
		stats.volumes[v, COL_STATS_CORR_SCENES_PANELS_BY_CHAR] <- val
		tlog(6,"Correlation between scenes/character and panels/character: ", val)
		
		# attribute stats
		tlog(6,"Computing attribute stats")
		for(a in 1:length(atts))
		{	tlog(5,"Computing attribute ",atts[a]," (",a,"/",length(atts),")")
			
			# attribute distribution over the characters
			vals <- table(char.info[idx.char,atts[a]])
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
	write.csv(x=stats.volumes, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=stats.volumes.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	# record the volumes for each character (for latter use)
	con <- file(paste0(file,"_chars.txt"), open="wt")
	sapply(char.volumes, function(lst) writeLines(paste(lst,collapse=","), con))
	close(con)
	
	# evolution of the stats by volume
	vol.cols <- c(
			COL_STATS_PAGES, COL_STATS_PAGES_BY_SCENE, COL_STATS_PAGES_BY_CHAR,
			COL_STATS_SCENES, COL_STATS_SCENES_BY_PAGE, COL_STATS_SCENES_BY_CHAR, 
			COL_STATS_PANELS, COL_STATS_PANELS_BY_PAGE, COL_STATS_PANELS_BY_SCENE, COL_STATS_PANELS_BY_CHAR, 
			COL_STATS_CHARS, COL_STATS_CHARS_BY_PAGE, COL_STATS_CHARS_BY_SCENE, COL_STATS_CHARS_BY_PANEL,
			COL_STATS_CORR_PANELS_CHARS_BY_SCENE, COL_STATS_CORR_SCENES_PANELS_BY_CHAR
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
					height=stats.volumes[,vol.cols[v]],
					names.arg=stats.volumes[,COL_STATS_VOLUME],
					main=paste0("Evolution of the ",vol.titles[v]),
					col=MAIN_COLOR
				)
			dev.off()
		}
	}
	
	# distributions of character numbers
	vals <- table(stats.volumes[, COL_STATS_CHARS])
	vals <- data.frame(names(vals), vals, 100*vals/sum(vals), stringsAsFactors=FALSE, check.names=FALSE)
	colnames(vals) <- c(COL_STATS_CHARS, COL_STATS_VOLUMES, "Proportion")
	file <- get.path.stat.corpus(object=object, desc="volumes_distrib_char_nbr")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- stats.volumes[,COL_STATS_CHARS]
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
					plot.ccdf(data=data, main=ml, xlab=xl, log=TRUE)
				dev.off()
		}
		# check distribution
#		distr.stats <- test.disc.distr(data, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
#		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}
	
	# TODO plot the distributions obtained for all volumes on the same plot? (using lines)

	result <- list(stats.volumes=stats.volumes, stats.volumes.atts=stats.volumes.atts)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the narrative arcs.
#
# volume.info: table describing the series volumes.
# page.info: table describing all the pages constituting the BD series.
# char.info: table describing all the characters occurring in the BD series.
# stats.volumes: previously computed volume statistics.
# stats.pages: previously computed page statistics.
# stats.scenes: previously computed scene statistics.
# stats.panels: previously computed panel statistics.
# stats.chars: previously computed character statistics.
# char.volumes: characters present in each volume.
# char.pages: characters present in each page.
# char.scenes: characters present in each scene.
# char.panels: characters present in each panel.
#
# returns: a list containing the table with overall arc stats (stats.arcs), and
#          the arc stats by vertex attribute values (stats.chars.arcs).
###############################################################################
compute.stats.arcs <- function(
		volume.info, page.info, char.info, 
		stats.volumes, stats.pages, stats.scenes, stats.panels, stats.chars,
		char.volumes, char.pages, char.scenes, char.panels)
{	object <- "arcs"
	# vertex attributes
	atts <- setdiff(colnames(char.info), c(COL_CHAR_NAME, COL_CHAR_SHORT_NAME, COL_CHAR_FREQ))
	att.nbr <- length(atts)
	
	# compute stats
	tlog(3,"Computing arc stats")
	arc.titles <- unique(volume.info[,COL_VOLS_ARC])
	arc.nbr <- length(arc.titles)
	
	#  identify the characters in each arc
	tlog(4,"Identify the characters in each arc")
	char.arcs <- lapply(1:arc.nbr, function(x) c())
	for(a in 1:arc.nbr)
	{	tlog(5,"Processing arc ",a,"/",arc.nbr)
		
		# find the pages contained in the arc
		vols <- which(volume.info[,COL_VOLS_ARC]==arc.titles[a])
		idx.pg <- which(page.info[,COL_PAGES_VOLUME_ID] %in% vols)
		for(p in idx.pg)
		{	if(length(char.pages[[p]])>0)
				char.arcs[[a]] <- union(char.arcs[[a]], char.pages[[p]])
		}
	}
	
	# init stats table for arcs
	stats.arcs <- data.frame(
			character(arc.nbr), integer(arc.nbr),
			integer(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			integer(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), numeric(arc.nbr), 
			numeric(arc.nbr), numeric(arc.nbr),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(stats.arcs) <- c(
			COL_STATS_ARC, COL_STATS_ARC_ID,
			COL_STATS_VOLUMES, COL_STATS_VOLUMES_BY_CHAR,
			COL_STATS_PAGES, COL_STATS_PAGES_BY_VOLUME, COL_STATS_PAGES_BY_SCENE, COL_STATS_PAGES_BY_CHAR,
			COL_STATS_SCENES, COL_STATS_SCENES_BY_VOLUME, COL_STATS_SCENES_BY_PAGE, COL_STATS_SCENES_BY_CHAR, 
			COL_STATS_PANELS, COL_STATS_PANELS_BY_VOLUME, COL_STATS_PANELS_BY_PAGE, COL_STATS_PANELS_BY_SCENE, COL_STATS_PANELS_BY_CHAR, 
			COL_STATS_CHARS, COL_STATS_CHARS_BY_VOLUME, COL_STATS_CHARS_BY_PAGE, COL_STATS_CHARS_BY_SCENE, COL_STATS_CHARS_BY_PANEL,
			COL_STATS_CORR_PANELS_CHARS_BY_SCENE, COL_STATS_CORR_SCENES_PANELS_BY_CHAR
	)
	
	# init stats tables for attributes
	stats.arcs.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.info[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=arc.nbr, ncol=length(uniq))
			colnames(m) <- uniq
			stats.arcs.atts[[att]] <- m
		}
	}
	
	# compute the stats for each arc
	tlog(4,"Processing each arc separately")
	for(a in 1:arc.nbr)
	{	tlog(5,"Processing arc ",a,"/",arc.nbr)
		
		# corresponding volumes
		idx.vol <- which(volume.info[,COL_VOLS_ARC]==arc.titles[a])
		vln <- length(idx.vol)
		
		# corresponding pages
		idx.pg <- which(page.info[,COL_PAGES_VOLUME_ID] %in% idx.vol)
		pgn <- length(idx.pg)
		
		# corresponding characters
		char.arc <- c()
		for(i in idx.pg)
			char.arc <- union(char.arc, char.pages[[i]])
		idx.char <- match(char.arc, char.info[,COL_CHAR_NAME])
		chn <- length(char.arc)
		
		# corresponding scenes
		idx.sc <- which(stats.scenes[,COL_STATS_VOLUME_ID] %in% idx.vol)
		scn <- length(idx.sc)
		
		# corresponding panels
		idx.pn <- which(stats.panels[,COL_STATS_VOLUME_ID] %in% idx.vol)
		pnn <- sum(page.info[idx.pg,COL_PAGES_PANELS])
		
		# char stats
		char.arc.vln <- rep(0,chn)
		names(char.arc.vln) <- char.arc
		for(v in idx.vol)
		{	for(cc in char.volumes[[v]])
				char.arc.vln[cc] <- char.arc.vln[cc] + 1
		}
		char.arc.pgn <- rep(0,chn)
		names(char.arc.pgn) <- char.arc
		for(p in idx.pg)
		{	for(cc in char.pages[[p]])
				char.arc.pgn[cc] <- char.arc.pgn[cc] + 1
		}
		char.arc.scn <- rep(0,chn)
		names(char.arc.scn) <- char.arc
		for(s in idx.sc)
		{	for(cc in char.scenes[[s]])
				char.arc.scn[cc] <- char.arc.scn[cc] + 1
		}
		char.arc.pnl <- rep(0,chn)
		names(char.arc.pnl) <- char.arc
		for(p in idx.pn)
		{	for(cc in char.panels[[p]])
				char.arc.pnl[cc] <- char.arc.pnl[cc] + 1
		}
		
		# compute detailed stats
		arc.volume.info <- volume.info[idx.vol,]
		arc.page.info <- page.info[idx.pg,]
		arc.char.info <- char.info[idx.char,] 
		arc.stats.scenes <- stats.scenes[idx.sc,] 
		arc.char.scenes <- char.scenes[idx.sc]
		# compute panel stats
		tmp <- compute.stats.panels(
				arc.volume.info, arc.page.info, arc.char.info, 
				arc.stats.scenes, 
				arc.char.scenes,
				cur.vol=NA, cur.arc=a
		)
		arc.stats.panels <- tmp$stats.panels
		arc.stats.panels.atts <- tmp$stats.panels.atts
		arc.char.panels <- tmp$char.panels
		# compute page stats
		tmp <- compute.stats.pages(
				arc.volume.info, arc.page.info, arc.char.info, 
				arc.stats.scenes, arc.stats.panels, 
				arc.char.scenes, arc.char.panels,
				cur.vol=NA, cur.arc=a
		)
		arc.stats.pages <- tmp$stats.pages
		arc.stats.pages.atts <- tmp$stats.pages.atts
		arc.char.pages <- tmp$char.pages
		# compute scene stats
		tmp <- compute.stats.scenes(
				arc.volume.info, arc.page.info, arc.char.info, 
				arc.stats.pages, arc.stats.scenes, arc.stats.panels, 
				arc.char.pages, arc.char.scenes, arc.char.panels,
				cur.vol=NA, cur.arc=a
		)
		arc.stats.scenes <- tmp$stats.scenes
		arc.stats.scenes.atts <- tmp$stats.scenes.atts
		# compute character stats
		tmp <- compute.stats.chars(
				arc.volume.info, arc.page.info, arc.char.info, 
				arc.stats.pages, arc.stats.scenes, arc.stats.panels, 
				arc.char.pages, arc.char.scenes, arc.char.panels,
				cur.vol=NA, cur.arc=a
		)
		arc.stats.chars <- tmp$stats.chars
		arc.char.volumes <- tmp$char.volumes
		
		# update overall stat table
		stats.arcs[a, COL_STATS_ARC] <- arc.titles[a]
		stats.arcs[a, COL_STATS_ARC_ID] <- a
		#
		stats.arcs[a, COL_STATS_VOLUMES] <- vln
		stats.arcs[a, COL_STATS_VOLUMES_BY_CHAR] <- sum(char.arc.vln)/chn
		#
		stats.arcs[a, COL_STATS_PAGES] <- pgn
		stats.arcs[a, COL_STATS_PAGES_BY_VOLUME] <- sum(stats.volumes[idx.vol,COL_STATS_PAGES])/vln
		stats.arcs[a, COL_STATS_PAGES_BY_SCENE] <- sum(stats.scenes[idx.sc,COL_STATS_PAGES])/scn
		stats.arcs[a, COL_STATS_PAGES_BY_CHAR] <- sum(char.arc.pgn)/chn
		#
		stats.arcs[a, COL_STATS_SCENES] <- scn
		stats.arcs[a, COL_STATS_SCENES_BY_VOLUME] <- sum(stats.volumes[idx.vol,COL_STATS_SCENES])/vln
		stats.arcs[a, COL_STATS_SCENES_BY_PAGE] <- sum(stats.pages[idx.pg,COL_STATS_SCENES])/pgn
		stats.arcs[a, COL_STATS_SCENES_BY_CHAR] <- sum(char.arc.scn)/chn
		#
		stats.arcs[a, COL_STATS_PANELS] <- pnn
		stats.arcs[a, COL_STATS_PANELS_BY_VOLUME] <- sum(stats.volumes[idx.vol,COL_STATS_PANELS])/vln
		stats.arcs[a, COL_STATS_PANELS_BY_PAGE] <- sum(stats.pages[idx.pg,COL_STATS_PANELS])/pgn
		stats.arcs[a, COL_STATS_PANELS_BY_SCENE] <- sum(stats.scenes[idx.sc,COL_STATS_PANELS])/scn
		stats.arcs[a, COL_STATS_PANELS_BY_CHAR] <- sum(char.arc.pnl)/chn
		#
		stats.arcs[a, COL_STATS_CHARS] <- chn
		stats.arcs[a, COL_STATS_CHARS_BY_VOLUME] <- sum(stats.volumes[idx.vol,COL_STATS_CHARS])/vln
		stats.arcs[a, COL_STATS_CHARS_BY_PAGE] <- sum(stats.pages[idx.pg,COL_STATS_CHARS])/pgn
		stats.arcs[a, COL_STATS_CHARS_BY_SCENE] <- sum(stats.scenes[idx.sc,COL_STATS_CHARS])/scn
		stats.arcs[a, COL_STATS_CHARS_BY_PANEL] <- sum(stats.panels[idx.pn,COL_STATS_CHARS])/pnn
		
		# update attribute stat table
		if(att.nbr>0)
		{	idx <- match(char.arcs[[a]], char.info[,COL_CHAR_NAME])
			for(att in atts)
			{	m <- stats.arcs.atts[[att]]
				tt <- table(char.info[idx,att])
				m[a,names(tt)] <- tt
				stats.arcs.atts[[att]] <- m
			}
		}
		
		# density plot: chars vs. panels (by scene)
		file <- get.path.stat.corpus(arc=a, desc="comparison_chars-scenes_vs_panels-scenes")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				xvals <- stats.scenes[idx.sc,COL_STATS_CHARS]
				yvals <- stats.scenes[idx.sc,COL_STATS_PANELS]
				xlab <- "Number of characters by scene"
				ylab <- "Number of panels by scene"
				p=ggplot(stats.scenes[idx.sc,], aes(x=xvals, y=yvals)) +
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
		val <- cor(stats.scenes[idx.sc,COL_STATS_CHARS],stats.scenes[idx.sc,COL_STATS_PANELS])
		stats.arcs[a, COL_STATS_CORR_PANELS_CHARS_BY_SCENE] <- val
		tlog(6,"Correlation between characters/scene and panels/scene: ", val)
		
		# density: scenes vs. panels (by char)
		val <- cor(stats.chars[idx.char,COL_STATS_SCENES],stats.chars[idx.char,COL_STATS_PANELS])
		stats.arcs[a, COL_STATS_CORR_SCENES_PANELS_BY_CHAR] <- val
		tlog(6,"Correlation between scenes/character and panels/character: ", val)
		
		# attribute stats
		tlog(6,"Computing attribute stats")
		for(at in 1:length(atts))
		{	tlog(5,"Computing attribute ",atts[at]," (",at,"/",length(atts),")")
			
			# attribute distribution over the characters
			vals <- table(char.info[idx.char,atts[at]])
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
	write.csv(x=stats.arcs, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	if(att.nbr>0)
	{	for(att in atts)
			write.csv(x=stats.arcs.atts[[att]], file=paste0(file,"_att=",att,".csv"), row.names=FALSE)#, col.names=TRUE)
	}
	
	# evolution of the stats by arc
	arc.cols <- c(
			COL_STATS_VOLUMES, COL_STATS_VOLUMES_BY_CHAR,
			COL_STATS_PAGES, COL_STATS_PAGES_BY_VOLUME, COL_STATS_PAGES_BY_SCENE, COL_STATS_PAGES_BY_CHAR,
			COL_STATS_SCENES, COL_STATS_SCENES_BY_VOLUME, COL_STATS_SCENES_BY_PAGE, COL_STATS_SCENES_BY_CHAR, 
			COL_STATS_PANELS, COL_STATS_PANELS_BY_VOLUME, COL_STATS_PANELS_BY_PAGE, COL_STATS_PANELS_BY_SCENE, COL_STATS_PANELS_BY_CHAR, 
			COL_STATS_CHARS, COL_STATS_CHARS_BY_VOLUME, COL_STATS_CHARS_BY_PAGE, COL_STATS_CHARS_BY_SCENE, COL_STATS_CHARS_BY_PANEL,
			COL_STATS_CORR_PANELS_CHARS_BY_SCENE, COL_STATS_CORR_SCENES_PANELS_BY_CHAR
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
					height=stats.arcs[,arc.cols[a]],
					names.arg=stats.arcs[,COL_STATS_ARC],
					main=paste0("Evolution of the ",arc.ttls[a]),
					col=MAIN_COLOR
				)
			dev.off()
		}
	}
	
	# TODO plot all arc distributions on the same plot?
	
	result <- list(stats.arcs=stats.arcs, stats.arcs.atts=stats.arcs.atts, char.arcs=char.arcs)
	return(result)
}




###############################################################################
# Computes and records some statistics regarding the volumes.
#
# volume.info: table describing the series volumes.
# page.info: table describing all the pages constituting the BD series.
# char.info: table describing all the characters occurring in the BD series.
# stats.arcs: previously computed arc statistics.
# stats.volumes: previously computed volume statistics.
# stats.pages: previously computed page statistics.
# stats.scenes: previously computed scene statistics.
# stats.panels: previously computed panel statistics.
# stats.chars: previously computed character statistics.
# char.arcs: characters present in each narrative arc.
# char.volumes: characters present in each volume.
# char.pages: characters present in each page.
# char.scenes: characters present in each scene.
# char.panels: characters present in each panel.
#
# returns: a list containing the table with overall volume stats (stats.chars), and
#          the volume stats by vertex attribute values (stats.chars.atts).
###############################################################################
compute.stats.overall <- function(
		volume.info, page.info, char.info, 
		stats.arcs, stats.volumes, stats.pages, stats.scenes, stats.panels, stats.chars,
		char.arcs, char.volumes, char.pages, char.scenes, char.panels)
{	# vertex attributes
	atts <- setdiff(colnames(char.info), c(COL_CHAR_NAME, COL_CHAR_SHORT_NAME, COL_CHAR_FREQ))
	att.nbr <- length(atts)
	
	# compute stats
	tlog(3,"Computing overall stats")
	panel.nbr <- max(c(stats.scenes[,COL_STATS_START_PANEL_ID],stats.scenes[,COL_STATS_END_PANEL_ID]))
	page.nbr <- max(c(stats.scenes[,COL_STATS_START_PAGE_ID],stats.scenes[,COL_STATS_END_PAGE_ID]))
	scene.nbr <- nrow(stats.scenes)
	char.nbr <- length(sort(unique(unlist(char.scenes))))
	volume.nbr <- nrow(volume.info)
	
	# init stat table
	stats.overall <- data.frame(
			integer(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), numeric(1), 
			integer(1), numeric(1), numeric(1), numeric(1), numeric(1), 
			numeric(1), numeric(1),
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(stats.overall) <- c(
			COL_STATS_VOLUMES, COL_STATS_VOLUMES_BY_CHAR,
			COL_STATS_PAGES, COL_STATS_PAGES_BY_VOLUME, COL_STATS_PAGES_BY_SCENE, COL_STATS_PAGES_BY_CHAR,
			COL_STATS_SCENES, COL_STATS_SCENES_BY_VOLUME, COL_STATS_SCENES_BY_PAGE, COL_STATS_SCENES_BY_CHAR, 
			COL_STATS_PANELS, COL_STATS_PANELS_BY_VOLUME, COL_STATS_PANELS_BY_PAGE, COL_STATS_PANELS_BY_SCENE, COL_STATS_PANELS_BY_CHAR, 
			COL_STATS_CHARS, COL_STATS_CHARS_BY_VOLUME, COL_STATS_CHARS_BY_PAGE, COL_STATS_CHARS_BY_SCENE, COL_STATS_CHARS_BY_PANEL,
			COL_STATS_CORR_PANELS_CHARS_BY_SCENE, COL_STATS_CORR_SCENES_PANELS_BY_CHAR
	)
	
	# init stats tables for attributes
	stats.overall.atts <- list()
	if(att.nbr>0)
	{	for(att in atts)
		{	vals <- char.info[,att]
			uniq <- names(table(vals))	#, useNA="always"))
			m <- matrix(0, nrow=1, ncol=length(uniq))
			colnames(m) <- uniq
			stats.overall.atts[[att]] <- m
		}
	}
	
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
	file <- get.path.stat.corpus(desc="overall")
	tlog(4,"Recording in ",file)
	write.csv(x=stats.overall, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	
	# density plot: chars vs. panels
	file <- get.path.stat.corpus(desc="comparison_chars-scenes_vs_panels-scenes")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			xvals <- stats.scenes[,COL_STATS_CHARS]
			yvals <- stats.scenes[,COL_STATS_PANELS]
			xlab <- "Number of characters by scene"
			ylab <- "Number of panels by scene"
			p=ggplot(stats.scenes, aes(x=xvals, y=yvals)) +
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
	val <- cor(stats.scenes[,COL_STATS_CHARS],stats.scenes[,COL_STATS_PANELS])
	stats.overall[1,COL_STATS_CORR_PANELS_CHARS_BY_SCENE] <- val
	tlog(4,"Correlation between characters/scene and panels/scene: ", val)
	
	# density plot: scenes vs. panels
	file <- get.path.stat.corpus(desc="comparison_scenes-chars_vs_panels-chars")
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			xvals <- stats.chars[,COL_STATS_SCENES]
			yvals <- stats.chars[,COL_STATS_PANELS]
			xlab <- "Number of scenes by character"
			ylab <- "Number of panels by character"
			p=ggplot(stats.chars, aes(x=xvals, y=yvals)) +
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
	val <- cor(stats.chars[,COL_STATS_SCENES],stats.chars[,COL_STATS_PANELS])
	stats.overall[1,COL_STATS_CORR_SCENES_PANELS_BY_CHAR] <- val
	tlog(4,"Correlation between scenes/character and panels/character: ", val)
	
	# attribute stats
	tlog(3,"Computing attribute stats")
	for(a in 1:length(atts))
	{	tlog(4,"Computing attribute ",atts[a]," (",a,"/",length(atts),")")
		
		# attribute distribution over the characters
		vals <- table(char.info[,atts[a]])
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
	
	
	result <- list(stats.overall=stats.overall, stats.overall.atts=stats.overall.atts)
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
	volume.info <- data$volume.info
	page.info <- data$page.info
	char.info <- data$char.info
	stats.scenes <- data$stats.scenes
	char.scenes <- data$char.scenes
	
	# compute panel stats
	tmp <- compute.stats.panels(
			volume.info, page.info, char.info, 
			stats.scenes, 
			char.scenes,
			cur.vol=NA, cur.arc=NA
	)
	stats.panels <- tmp$stats.panels
	stats.panels.atts <- tmp$stats.panels.atts
	char.panels <- tmp$char.panels
	
	# compute page stats
	tmp <- compute.stats.pages(
			volume.info, page.info, char.info, 
			stats.scenes, stats.panels, 
			char.scenes, char.panels,
			cur.vol=NA, cur.arc=NA
	)
	stats.pages <- tmp$stats.pages
	stats.pages.atts <- tmp$stats.pages.atts
	char.pages <- tmp$char.pages
	
	# compute scene stats
	tmp <- compute.stats.scenes(
			volume.info, page.info, char.info, 
			stats.pages, stats.scenes, stats.panels, 
			char.pages, char.scenes, char.panels,
			cur.vol=NA, cur.arc=NA
	)
	stats.scenes <- tmp$stats.scenes
	stats.scenes.atts <- tmp$stats.scenes.atts
	
	# compute character stats
	tmp <- compute.stats.chars(
			volume.info, page.info, char.info, 
			stats.pages, stats.scenes, stats.panels, 
			char.pages, char.scenes, char.panels,
			cur.vol=NA, cur.arc=NA,
			filtered=FALSE
	)
	stats.chars <- tmp$stats.chars
	char.volumes <- tmp$char.volumes
	# possibly process filtered characters
	if(COL_CHAR_FILTERED %in% colnames(char.info))
		tmp <- compute.stats.chars(
				volume.info, page.info, char.info, 
				stats.pages, stats.scenes, stats.panels, 
				char.pages, char.scenes, char.panels,
				cur.vol=NA, cur.arc=NA,
				filtered=TRUE
		)
	
	# compute volume stats
	tmp <- compute.stats.volumes(
			volume.info, page.info, char.info, 
			stats.pages, stats.scenes, stats.panels, stats.chars,
			char.volumes, char.pages, char.scenes, char.panels
	)
	stats.volumes <- tmp$stats.volumes
	stats.volumes.atts <- tmp$stats.volumes.atts
	
	# compute arc stats
	tmp <- compute.stats.arcs(
			volume.info, page.info, char.info, 
			stats.volumes, stats.pages, stats.scenes, stats.panels, stats.chars,
			char.volumes, char.pages, char.scenes, char.panels
	)
	stats.arcs <- tmp$stats.arcs
	stats.arcs.atts <- tmp$stats.arcs.atts
	char.arcs <- tmp$char.arcs
	
	# compute overall stats
	tmp <- compute.stats.overall(
			volume.info, page.info, char.info, 
			stats.arcs, stats.volumes, stats.pages, stats.scenes, stats.panels, stats.chars,
			char.arcs, char.volumes, char.pages, char.scenes, char.panels
	)
	stats.overall <- tmp$stats.overall
	stats.overall.atts <- tmp$stats.overall.atts
	
	# return all the stats
	result <- list(
		inter.df=inter.df,
		stats.panels=stats.panels, stats.panels.atts=stats.panels.atts, char.panels=char.panels,
		page.info=page.info, stats.pages=stats.pages, stats.pages.atts=stats.pages.atts, char.pages=char.pages,
		stats.scenes=stats.scenes, stats.scenes.atts=stats.scenes.atts, char.scenes=char.scenes,
		char.info=char.info, stats.chars=stats.chars, 
		volume.info=volume.info, stats.volumes=stats.volumes, stats.volumes.atts=stats.volumes.atts, char.volumes=char.volumes,
		stats.arcs=stats.arcs, stats.arcs.atts=stats.arcs.atts, char.arcs=char.arcs,
		stats.overall=stats.overall, stats.overall.atts=stats.overall.atts
	)
	return(result)
}
