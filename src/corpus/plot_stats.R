# This script contains functions used to compute some statistics and generate
# plots related to the corpus.
# 
# Vincent Labatut
# 05/2021
###############################################################################
DO_STAT_TESTS <- TRUE		# much slower




###############################################################################
# Plots the panel statistics.
#
# panel.stats: table describing all the panels constituting the series.
# panel.stats.atts: panel stats for each attribute. 
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or id of the volume specifically processed.
# cur.arc: NA (regular mode) or id of the arc specifically processed.
###############################################################################
plot.stats.panel <- function(
		panel.stats, panel.stats.atts, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA)
{	object <- "panels"
	
	# preparation
	if(!is.na(cur.vol))
	{	vname <- paste0(cur.vol,"_",volume.stats[cur.vol,COL_VOLUME])
		tlog(3,"Computing panel stats (cur.vol=",cur.vol," vname=",vname,")")
	}
	else if(!is.na(cur.arc))
	{	vname <- NA
		tlog(3,"Computing panel stats (cur.arc=",cur.arc,")")
	}
	else
	{	vname <- NA
		tlog(3,"Computing panel stats (whole series)")
	}
	
	# panels
	panel.nbr <- nrow(panel.stats)
	col <- ATT_COLORS_FILT["Discard"]
	# vertex attributes
	atts <- names(panel.stats.atts)
	att.nbr <- length(atts)
	
	
	##################
	# attribute-independent stats
	
	# distributions of character numbers
	vals <- table(panel.stats[,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_PANELS, "Proportion")
	file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_chars-by-panel")
	tlog(4,"Distribution of character numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- panel.stats[,COL_CHARS]
	ml <- "Character number distribution over panels"
	xl <- "Number of characters by panel"
	plot.disc.distribution(
		vals=data, 
		xlab=xl, main=ml, 
		breaks=0:max(data), 
		log=TRUE, cols=col, 
		file=file,
		histo=TRUE, ccdf=FALSE, test=FALSE
	)
	
	# distribution of panel positions
	vals <- c()
	vals["Both"] <- length(which(panel.stats[, COL_MATCH_BOTH]))
	vals["Starts page"] <- length(which(panel.stats[, COL_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(panel.stats[, COL_MATCH_END])) - vals["Both"]
	vals["None"] <- panel.nbr - vals["Both"] - vals["Starts page"] - vals["Ends page"]
	perc <- vals/sum(vals)*100
	df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
	colnames(df) <- c("Position","Frequency","Proportion")
	file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_positions")
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
		{	data <- panel.stats.atts[[att]][,-1,drop=FALSE]
			pal <- ATT_COLORS[[att]]
			if(length(pal)==0) 
			{	pal <- get.palette(ncol(data))
				names(pal) <- colnames(data)
			}
			else
				pal <- pal[colnames(data)]
			file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_chars-by-panel", att=att)
			tlog(4,"Distribution of character numbers for attribute \"",att,"\": producing files \"",file,"\"")
			xl <- "Number of characters by panel"
			yl <- "Frequency"
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					ml <- paste0("Character number distribution over panels (att=",att)
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
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=TRUE, 							# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal,
						#space=0,
						args.legend=list(x="topright"),
						legend.text=colnames(data)
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 2:ncol(panel.stats.atts[[att]]))
			{	data <- panel.stats.atts[[att]][,d]
				if(any(data!=0))
				{	val <- colnames(panel.stats.atts[[att]])[d]
					file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_chars-by-panel", att=att, val=val)
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
# Plots the page statistics.
#
# page.stats: table describing all the pages constituting the series.
# page.stats.atts: page stats for each attribute. 
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or id of the volume specifically processed.
# cur.arc: NA (regular mode) or id of the arc specifically processed.
###############################################################################
plot.stats.page <- function(
		page.stats, page.stats.atts, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA)
{	object <- "pages"
	
	# preparation
	if(!is.na(cur.vol))
	{	vname <- paste0(cur.vol,"_",volume.stats[cur.vol,COL_VOLUME])
		tlog(3,"Computing page stats (cur.vol=",cur.vol," vname=",vname,")")
	}
	else if(!is.na(cur.arc))
	{	vname <- NA
		tlog(3,"Computing page stats (cur.arc=",cur.arc,")")
	}
	else
	{	vname <- NA
		tlog(3,"Computing page stats (whole series)")
	}
	
	# pages
	page.nbr <- nrow(page.stats)
	col <- ATT_COLORS_FILT["Discard"]
	# vertex attributes
	atts <- names(page.stats.atts)
	att.nbr <- length(atts)
	
	
	##################
	# attribute-independent stats
	
	# distributions of scene numbers
	vals <- table(page.stats[,COL_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_SCENES, COL_PAGES,"Proportion")
	file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_scenes-by-page")
	tlog(4,"Distribution of scene numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- page.stats[,COL_SCENES]
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	# distributions of panel numbers
	vals <- table(page.stats[,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_PAGES,"Proportion")
	file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_panels-by-page")
	tlog(4,"Distribution of panel numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- page.stats[,COL_PANELS]
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	# distributions of character numbers
	vals <- table(page.stats[,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_PAGES, "Proportion")
	file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_chars-by-page")
	tlog(4,"Distribution of character numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- page.stats[,COL_CHARS]
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	
	##################
	# attribute-based stats
	if(att.nbr>0)
	{	# distribution of character numbers (by attribute)
		for(att in atts)
		{	data <- page.stats.atts[[att]][,-1,drop=FALSE]
			pal <- ATT_COLORS[[att]]
			if(length(pal)==0) 
			{	pal <- get.palette(ncol(data))
				names(pal) <- colnames(data)
			}
			else
				pal <- pal[colnames(data)]
			file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_chars-by-page", att=att)
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
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=TRUE,								# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal,
						#space=0,
						args.legend=list(x="topright"),
						legend.text=colnames(data)
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 2:ncol(page.stats.atts[[att]]))
			{	data <- page.stats.atts[[att]][,d]
				if(any(data!=0))
				{	val <- colnames(page.stats.atts[[att]])[d]
					file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_chars-by-page", att=att, val=colnames(page.stats.atts[[att]])[d])
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
# Plots the scene statistics.
#
# scene.stats: table describing all the scene constituting the series.
# page.stats.atts: scene stats for each attribute. 
# volume.stats: table describing the series volumes.
# cur.vol: NA (regular mode) or id of the volume specifically processed.
# cur.arc: NA (regular mode) or id of the arc specifically processed.
###############################################################################
plot.stats.scene <- function(
		scene.stats, scene.stats.atts, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA)
{	object <- "scenes"
	
	# preparation
	if(!is.na(cur.vol))
	{	vname <- paste0(cur.vol,"_",volume.stats[cur.vol,COL_VOLUME])
		tlog(3,"Computing scene stats (cur.vol=",cur.vol," vname=",vname,")")
	}
	else if(!is.na(cur.arc))
	{	vname <- NA
		tlog(3,"Computing scene stats (cur.arc=",cur.arc,")")
	}
	else
	{	vname <- NA
		tlog(3,"Computing scene stats (whole series)")
	}
	
	# scenes
	scene.nbr <- nrow(scene.stats)
	col <- ATT_COLORS_FILT["Discard"]
	# vertex attributes
	atts <- names(scene.stats.atts)
	att.nbr <- length(atts)
	
	
	##################
	# attribute-independent stats
	
	# distributions of panel numbers
	vals <- table(scene.stats[,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_SCENES,"Proportion")
	file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_panels-by-scene")
	tlog(4,"Distribution of panel numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- scene.stats[,COL_PANELS]
	ml <- "Panel number distribution over scenes"
	xl <- "Number of panels by scene"
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
#				x <- h$breaks[2:length(h$breaks)]
#			y <- h$density
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			expmax <- floor(log(min(y),10))
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy", yaxt="n") #las=1
#			axis(side=2, at=10^(expmax:0), label=parse(text=paste("10^", expmax:0, sep="")), las=1)
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
	if(DO_STAT_TESTS && length(unique(data))>1 && is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distributions of character numbers
	vals <- table(scene.stats[,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_SCENES, "Proportion")
	file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_chars-by-scene")
	tlog(4,"Distribution of character numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- scene.stats[,COL_CHARS]
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	# distributions of page numbers
	vals <- table(scene.stats[,COL_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_SCENES,"Proportion")
	file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_pages-by-scene")
	tlog(4,"Distribution of page numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- scene.stats[,COL_PAGES]
	ml <- "Page number distribution over scenes"
	xl <- "Number of pages by scene"
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			# histogram
#			h <- hist(
#				data,
#				breaks=0:max(data),
#				col=col,
#				xlab=xl,
#				main=ml,
#				freq=FALSE,
##				plot=FALSE
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
	if(DO_STAT_TESTS && length(unique(data))>1 && is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distribution of scene positions
	vals <- c()
	vals["Both"] <- length(which(scene.stats[, COL_MATCH_BOTH]))
	vals["Starts page"] <- length(which(scene.stats[, COL_MATCH_START])) - vals["Both"]
	vals["Ends page"] <- length(which(scene.stats[, COL_MATCH_END])) - vals["Both"]
	vals["None"] <- scene.nbr - vals["Both"] - vals["Starts page"] - vals["Ends page"]
	perc <- vals/sum(vals)*100
	df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
	colnames(df) <- c("Position","Frequency","Proportion")
	file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_positions")
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
		{	data <- scene.stats.atts[[att]][,-1,drop=FALSE]
			pal <- ATT_COLORS[[att]]
			if(length(pal)==0) 
			{	pal <- get.palette(ncol(data))
				names(pal) <- colnames(data)
			}
			else
				pal <- pal[colnames(data)]
			file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_chars-by-scene", att=att)
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
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=TRUE,								# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal,
						#space=0,
						args.legend=list(x="topright"),
						legend.text=colnames(data)
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 2:ncol(scene.stats.atts[[att]]))
			{	data <- scene.stats.atts[[att]][,d]
				if(any(data!=0))
				{	val <- colnames(scene.stats.atts[[att]])[d]
					file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="distrib_chars-by-scene", att=att, val=colnames(scene.stats.atts[[att]])[d])
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
plot.stats.char <- function(
		char.stats, 
		volume.stats, 
		cur.vol=NA, cur.arc=NA, 
		filtered=FALSE)
{	object <- "characters"
	
	# char filtering
	if(filtered)
	{	filt.txt <- "filtered"
		char.idx <- which(char.stats[,COL_FILTER]=="Keep")
		col <- ATT_COLORS_FILT["Keep"]
	}
	else
	{	filt.txt <- "unfiltered"
		char.idx <- 1:nrow(char.stats)
		col <- ATT_COLORS_FILT["Discard"]
	}
	
	# preparation
	if(!is.na(cur.vol))
	{	vname <- paste0(cur.vol,"_",volume.stats[cur.vol,COL_VOLUME])
		tlog(3,"Computing ",filt.txt," char stats (cur.vol=",cur.vol," vname=",vname,")")
	}
	else if(!is.na(cur.arc))
	{	vname <- NA
		tlog(3,"Computing ",filt.txt," char stats (cur.arc=",cur.arc,")")
	}
	else
	{	vname <- NA
		tlog(3,"Computing ",filt.txt," char stats (whole series)")
	}
	
	# record stats
	file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref="_char_stats")
	tlog(4,"Recording stats in ",file)
	write.csv(x=char.stats[char.idx,], file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	
	
	##################
	# attribute-independent stats

	# distribution of arcs by character
	if(is.na(cur.arc) && is.na(cur.vol))
	{	vals <- table(char.stats[char.idx,COL_ARCS])
		vals <- data.frame(names(vals), vals, 100*vals/sum(vals), stringsAsFactors=FALSE, check.names=FALSE)
		colnames(vals) <- c(COL_ARCS, COL_CHARS,"Proportion")
		tlog(4,"Distribution of arc numbers: producing files \"",file,"\"")
		file <- get.path.stats.corpus(object=object, subfold=filt.txt, pref=paste0("distrib_arcs-by-char"))
		write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		#
		data <- char.stats[char.idx,COL_ARCS]
		if(length(unique(data))>1)
		{	ml <- paste0("Arc number distribution over ",filt.txt," characters")
			xl <- paste0("Number of arcs by ",filt.txt," character")
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
			if(DO_STAT_TESTS)
			{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
				write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
			}
		}
	}
	
	# distribution of volumes by character
	if(is.na(cur.vol))
	{	vals <- table(char.stats[char.idx,COL_VOLUMES])
		vals <- data.frame(names(vals), vals, 100*vals/sum(vals), stringsAsFactors=FALSE, check.names=FALSE)
		colnames(vals) <- c(COL_VOLUMES, COL_CHARS,"Proportion")
		tlog(4,"Distribution of volume numbers: producing files \"",file,"\"")
		file <- get.path.stats.corpus(object=object, subfold=filt.txt, arc=cur.arc, pref=paste0("distrib_volumes-by-char"))
		write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
		#
		data <- char.stats[char.idx,COL_VOLUMES]
		if(length(unique(data))>1)
		{	ml <- paste0("Volume number distribution over ",filt.txt," characters")
			xl <- paste0("Number of volumes by ",filt.txt," character")
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
			if(DO_STAT_TESTS && is.na(cur.arc))
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
	file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref=paste0("distrib_pages-by-char"))
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[char.idx,COL_PAGES]
	ml <- paste0("Page number distribution over ",filt.txt," characters")
	xl <- paste0("Number of pages by ",filt.txt," character")
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
	if(DO_STAT_TESTS && length(unique(data))>1 && is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distribution of scenes by character
	vals <- table(char.stats[char.idx,COL_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_SCENES, COL_CHARS,"Proportion")
	file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref=paste0("distrib_senes-by-char"))
	tlog(4,"Distribution of scene numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[char.idx,COL_SCENES]
	ml <- paste0("Scene number distribution over ",filt.txt," characters")
	xl <- paste0("Number of scenes by ",filt.txt," character")
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
	if(DO_STAT_TESTS && length(unique(data))>1 && is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distribution of panels by character
	vals <- table(char.stats[char.idx,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_CHARS,"Proportion")
	file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref=paste0("distrib_panels-by-char"))
	tlog(4,"Distribution of panel numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- char.stats[char.idx,COL_PANELS]
	ml <- paste0("Panel number distribution over ",filt.txt," characters")
	xl <- paste0("Number of panels by ",filt.txt," character")
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
	if(DO_STAT_TESTS && length(unique(data))>1 && is.na(cur.vol) && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# behavior of character filtering (trying to identify extras)
	if(!filtered)
	{	thresholds <- seq(0, 10)	#max(char.stats[,COL_FREQ]))
		char.nums <- sapply(thresholds, function(t) c(table(factor(char.stats[char.stats[,COL_FREQ]>=t,COL_NAMED], levels=c("TRUE","FALSE")))))
		# generate barplots
		file <- get.path.stats.corpus(object=object, vol=vname, arc=cur.arc, pref="filtering_chars_vs_threshold")
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
					beside=FALSE,							# stacked (FALSE) vs. grouped (TRUE) bars
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
	
	
	##################
	# attribute-based stats
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	if(att.nbr>0)
	{	for(a in 1:length(atts))
		{	tlog(4,"Processing attribute ",atts[a]," (",a,"/",length(atts),")")
			
			# attribute distribution over the characters
			vals <- c(table(char.stats[char.idx,atts[a]]))
			pal <- ATT_COLORS[[atts[a]]]
			if(length(pal)==0) 
			{	pal <- get.palette(length(vals))
				names(pal) <- names(vals)
			}
			else
				pal <- pal[names(vals)]
			#
			perc <- vals/sum(vals)*100
			df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
			rownames(df) <- NULL
			colnames(df) <- c(atts[a],"Frequency","Proportion")
			file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref="attr_distrib", att=atts[a])
			tlog(5,"Producing files \"",file,"\"")
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
					col=pal
				)
				dev.off()
			}
			
			# distribution of arcs by character attribute
			if(is.na(cur.arc) && is.na(cur.vol))
			{	vals <- table(char.stats[char.idx,COL_ARCS],char.stats[char.idx,atts[a]])
				vals <- cbind(as.integer(rownames(vals)), as.data.frame.matrix(vals, stringsAsFactors=FALSE, check.names=FALSE))
				rownames(vals) <- NULL
				colnames(vals)[1] <- COL_ARCS
				# all values at once
				file <- get.path.stats.corpus(object=object, subfold=filt.txt, pref=paste0("distrib_arcs-by-char"), att=atts[a])
				tlog(5,"Distribution of arc numbers by attribute: producing files \"",file,"\"")
				write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
				#
				data <-  matrix(NA, nrow=ncol(vals)-1, ncol=max(vals[,1]))
				rownames(data) <- colnames(vals)[2:ncol(vals)]
				data[,vals[,1]] <- t(vals[,-1,drop=FALSE])
				#
				ml <- paste0("Arc number distribution over ",filt.txt," characters")
				xl <- paste0("Number of arcs by ",filt.txt," character")
				yl <- "Frequency"
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						# barplots
						barplot(
							height=data,
							names.arg=1:ncol(data),
							xlab=xl, ylab=yl, main=paste0(ml,")"),
							beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
							col=pal,
							space=0,
							args.legend=list(x="topright"),
							legend.text=rownames(data)
						)
					dev.off()
				}
				# each value separately
				for(d in 1:nrow(data))
				{	val <- rownames(data)[d]
					file <- get.path.stats.corpus(object=object, subfold=filt.txt, pref=paste0("distrib_arcs-by-char"), att=atts[a], val=val)
					tlog(5,"Distribution of arc numbers by attribute value: producing files \"",file,"\"")
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						# barplots
						barplot(
							height=data[d,],
							names.arg=1:ncol(data),
							xlab=xl, ylab=yl, main=paste0(ml,")"),
							beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
							col=pal[d],
							space=0
						)
						dev.off()
					}
				}
			}

			# distribution of volumes by character attribute
			if(is.na(cur.vol))
			{	vals <- table(char.stats[char.idx,COL_VOLUMES],char.stats[char.idx,atts[a]])
				vals <- cbind(as.integer(rownames(vals)), as.data.frame.matrix(vals, stringsAsFactors=FALSE, check.names=FALSE))
				rownames(vals) <- NULL
				colnames(vals)[1] <- COL_VOLUMES
				# all values at once
				file <- get.path.stats.corpus(object=object, subfold=filt.txt, arc=cur.arc, pref=paste0("distrib_volumes-by-char"), att=atts[a])
				tlog(5,"Distribution of volume numbers by attribute: producing files \"",file,"\"")
				write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
				#
				data <-  matrix(NA, nrow=ncol(vals)-1, ncol=max(vals[,1]))
				rownames(data) <- colnames(vals)[2:ncol(vals)]
				data[,vals[,1]] <- t(vals[,-1,drop=FALSE])
				#
				ml <- paste0("Volume number distribution over ",filt.txt," characters")
				xl <- paste0("Number of volumes by ",filt.txt," character")
				yl <- "Frequency"
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						# barplots
						barplot(
							height=data,
							names.arg=1:ncol(data),
							xlab=xl, ylab=yl, main=paste0(ml,")"),
							beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
							col=pal,
							space=0,
							args.legend=list(x="topright"),
							legend.text=rownames(data)
						)
					dev.off()
				}
				# each value separately
				for(d in 1:nrow(data))
				{	val <- rownames(data)[d]
					file <- get.path.stats.corpus(object=object, subfold=filt.txt, arc=cur.arc, pref=paste0("distrib_volumes-by-char"), att=atts[a], val=val)
					tlog(5,"Distribution of volume numbers by attribute value: producing files \"",file,"\"")
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						# barplots
						barplot(
							height=data[d,],
							names.arg=1:ncol(data),
							xlab=xl, ylab=yl, main=paste0(ml,")"),
							beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
							col=pal[d],
							space=0
						)
						dev.off()
					}
				}
			}

			# distribution of scenes by character attribute
			vals <- table(char.stats[char.idx,COL_SCENES],char.stats[char.idx,atts[a]])
			vals <- cbind(as.integer(rownames(vals)), as.data.frame.matrix(vals, stringsAsFactors=FALSE, check.names=FALSE))
			rownames(vals) <- NULL
			colnames(vals)[1] <- COL_SCENES
			# all values at once
			file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref=paste0("distrib_scenes-by-char"), att=atts[a])
			tlog(5,"Distribution of scene numbers by attribute: producing files \"",file,"\"")
			write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
			#
			data <-  matrix(NA, nrow=ncol(vals)-1, ncol=max(vals[,1]))
			rownames(data) <- colnames(vals)[2:ncol(vals)]
			data[,vals[,1]] <- t(vals[,-1,drop=FALSE])
			#
			ml <- paste0("Scene number distribution over ",filt.txt," characters")
			xl <- paste0("Number of scenes by ",filt.txt," character")
			yl <- "Frequency"
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					# barplots
					barplot(
						height=data,
						names.arg=1:ncol(data),
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal,
						space=0,
						args.legend=list(x="topright"),
						legend.text=rownames(data)
					)
				dev.off()
			}
			# each value separately
			for(d in 1:nrow(data))
			{	val <- rownames(data)[d]
				file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref=paste0("distrib_scenes-by-char"), att=atts[a], val=val)
				tlog(5,"Distribution of scene numbers by attribute value: producing files \"",file,"\"")
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					# barplots
					barplot(
						height=data[d,],
						names.arg=1:ncol(data),
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal[d],
						space=0
					)
					dev.off()
				}
			}
			
			# distribution of pages by character attribute
			vals <- table(char.stats[char.idx,COL_PAGES],char.stats[char.idx,atts[a]])
			vals <- cbind(as.integer(rownames(vals)), as.data.frame.matrix(vals, stringsAsFactors=FALSE, check.names=FALSE))
			rownames(vals) <- NULL
			colnames(vals)[1] <- COL_PAGES
			# all values at once
			file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref=paste0("distrib_pages-by-char"), att=atts[a])
			tlog(5,"Distribution of page numbers by attribute: producing files \"",file,"\"")
			write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
			#
			data <-  matrix(NA, nrow=ncol(vals)-1, ncol=max(vals[,1]))
			rownames(data) <- colnames(vals)[2:ncol(vals)]
			data[,vals[,1]] <- t(vals[,-1,drop=FALSE])
			#
			ml <- paste0("Page number distribution over ",filt.txt," characters")
			xl <- paste0("Number of pages by ",filt.txt," character")
			yl <- "Frequency"
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					# barplots
					barplot(
						height=data,
						names.arg=1:ncol(data),
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal,
						space=0,
						args.legend=list(x="topright"),
						legend.text=rownames(data)
					)
				dev.off()
			}
			# each value separately
			for(d in 1:nrow(data))
			{	val <- rownames(data)[d]
				file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref=paste0("distrib_pages-by-char"), att=atts[a], val=val)
				tlog(5,"Distribution of page numbers by attribute value: producing files \"",file,"\"")
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					# barplots
					barplot(
						height=data[d,],
						names.arg=1:ncol(data),
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal[d],
						space=0
					)
					dev.off()
				}
			}
			
			# distribution of panels by character attribute
			vals <- table(char.stats[char.idx,COL_PANELS],char.stats[char.idx,atts[a]])
			vals <- cbind(as.integer(rownames(vals)), as.data.frame.matrix(vals, stringsAsFactors=FALSE, check.names=FALSE))
			rownames(vals) <- NULL
			colnames(vals)[1] <- COL_PANELS
			# all values at once
			file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref=paste0("distrib_panels-by-char"), att=atts[a])
			tlog(5,"Distribution of panel numbers by attribute: producing files \"",file,"\"")
			write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
			#
			data <-  matrix(NA, nrow=ncol(vals)-1, ncol=max(vals[,1]))
			rownames(data) <- colnames(vals)[2:ncol(vals)]
			data[,vals[,1]] <- t(vals[,-1,drop=FALSE])
			#
			ml <- paste0("Panel number distribution over ",filt.txt," characters")
			xl <- paste0("Number of panels by ",filt.txt," character")
			yl <- "Frequency"
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					# barplots
					barplot(
						height=data,
						names.arg=1:ncol(data),
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal,
						space=0,
						args.legend=list(x="topright"),
						legend.text=rownames(data)
					)
				dev.off()
			}
			# each value separately
			for(d in 1:nrow(data))
			{	val <- rownames(data)[d]
				file <- get.path.stats.corpus(object=object, subfold=filt.txt, vol=vname, arc=cur.arc, pref=paste0("distrib_panels-by-char"), att=atts[a], val=val)
				tlog(5,"Distribution of panel numbers by attribute value: producing files \"",file,"\"")
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					# barplots
					barplot(
						height=data[d,],
						names.arg=1:ncol(data),
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=FALSE, 							# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal[d],
						space=0
					)
					dev.off()
				}
			}
		}
	}
}




###############################################################################
# Computes and records some statistics regarding the volumes.
#
# volume.stats: table describing the series volumes.
# volume.chars: list of characters involved in each volume.
#
# returns: an updated list of tables.
###############################################################################
plot.stats.volume <- function(
		volume.stats, volume.stats.atts, volume.stats.indiv=NA, 
		cur.arc=NA)
{	object <- "volumes"
	
	# preparation
	if(!is.na(cur.arc))
	{	tlog(3,"Computing volume stats (cur.arc=",cur.arc,")")
	}
	else
	{	tlog(3,"Computing volume stats (whole series)")
	}
	
	# volumes
	volume.nbr <- nrow(volume.stats)
	col <- ATT_COLORS_FILT["Discard"]
	# vertex attributes
	atts <- names(volume.stats.atts)
	att.nbr <- length(atts)
	
	
	##################
	# attribute-independent stats
	tlog(4,"Computing attribute-independent volume stats")
	
	# plot the stats for each volume
	if(is.na(cur.arc))
	{	tlog(4,"Processing each volume separately")
		for(v in 1:volume.nbr)
		{	vname <- paste0(v,"_",volume.stats[v,COL_VOLUME])
			tlog(5,"Processing volume ",vname," (",v,"/",nrow(volume.stats),")")
			
			vol.panel.stats <- volume.stats.indiv[[v]]$vol.panel.stats
			vol.panel.stats.atts <- volume.stats.indiv[[v]]$vol.panel.stats.atts
			vol.page.stats <- volume.stats.indiv[[v]]$vol.page.stats
			vol.page.stats.atts <- volume.stats.indiv[[v]]$vol.page.stats.atts
			vol.scene.stats <- volume.stats.indiv[[v]]$vol.scene.stats
			vol.scene.stats.atts <- volume.stats.indiv[[v]]$vol.scene.stats.atts
			vol.char.stats <- volume.stats.indiv[[v]]$vol.char.stats
			
			# plot volume-specific stats
			plot.stats.panel(panel.stats=vol.panel.stats, panel.stats.atts=vol.panel.stats.atts, volume.stats=volume.stats, cur.vol=v)
			plot.stats.page(page.stats=vol.page.stats, page.stats.atts=vol.page.stats.atts, volume.stats=volume.stats, cur.vol=v)
			plot.stats.scene(scene.stats=vol.scene.stats, scene.stats.atts=vol.scene.stats.atts, volume.stats=volume.stats, cur.vol=v)
			plot.stats.char(char.stats=vol.char.stats, volume.stats=volume.stats, cur.vol=v)
			
			# density plot: chars vs. panels (by scene)
			file <- get.path.stats.corpus(vol=vname, pref="distrib_chars-by-scene_vs_panels-by-scene")
			tlog(6,"Distribution of characters by scene vs. panels by scene: producing files \"",file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					xvals <- vol.scene.stats[,COL_CHARS]
					yvals <- vol.scene.stats[,COL_PANELS]
					xlab <- "Number of characters by scene"
					ylab <- "Number of panels by scene"
					p=ggplot(vol.scene.stats, aes(x=xvals, y=yvals)) +
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
		}
	}
	
	# distributions of panel numbers
	vals <- table(volume.stats[,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_VOLUMES,"Proportion")
	file <- get.path.stats.corpus(object=object, arc=cur.arc, pref="distrib_panels-by-volume")
	tlog(4,"Distribution of panel numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- volume.stats[,COL_PANELS]
	ml <- "Panel number distribution over volumes"
	xl <- "Number of panels by volume"
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
#				x <- h$breaks[2:length(h$breaks)]
#			y <- h$density
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			expmax <- floor(log(min(y),10))
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy", yaxt="n") #las=1
#			axis(side=2, at=10^(expmax:0), label=parse(text=paste("10^", expmax:0, sep="")), las=1)
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
	if(DO_STAT_TESTS && length(unique(data))>1 && is.na(cur.arc))
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distributions of character numbers
	vals <- table(volume.stats[,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_VOLUMES, "Proportion")
	file <- get.path.stats.corpus(object=object, arc=cur.arc, pref="distrib_chars-by-volume")
	tlog(4,"Distribution of character numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- volume.stats[,COL_CHARS]
	ml <- "Character number distribution over volumes"
	xl <- "Number of characters by volume"
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	# distributions of page numbers
	vals <- table(volume.stats[,COL_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PAGES, COL_VOLUMES,"Proportion")
	file <- get.path.stats.corpus(object=object, arc=cur.arc, pref="distrib_pages-by-volume")
	tlog(4,"Distribution of page numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- volume.stats[,COL_PAGES]
	ml <- "Page number distribution over volumes"
	xl <- "Number of pages by volume"
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
#	# check distribution
#	if(DO_STAT_TESTS && length(unique(data))>1 && is.na(cur.arc))
#	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
#		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
#	}
	
	# distributions of scene numbers
	vals <- table(volume.stats[,COL_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_SCENES, COL_VOLUMES,"Proportion")
	file <- get.path.stats.corpus(object=object, arc=cur.arc, pref="distrib_scenes-by-volume")
	tlog(4,"Distribution of scene numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- volume.stats[,COL_SCENES]
	ml <- "Scene number distribution over volumes"
	xl <- "Number of scenes by volume"
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
#	if(DO_STAT_TESTS && length(unique(data))>1 && is.na(cur.arc))
#	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
#		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
#	}
	
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
		"correlation between the numbers of characters and panels, by scene", "correlation between the numbers of scenes and panels, by character"
	)
	vol.fnames <- c(
		"pages", "pages-by-scene", "pages-by-char",
		"scenes", "scenes-by-page", "scenes-by-char",
		"panels", "panels-by-page", "panels-by-scene", "panels-by-char", 
		"chars", "chars-by-page", "chars-by-scene", "chars-by-panel",
		"corr_chars-by-scene_vs_panels-by-scene", "corr_scenes-by-char_vs_panels-by-char"
	)
	tlog(4,"Generating the evolution plots")
	for(v in 1:length(vol.cols))
	{	tlog(5,"Processing column \"",vol.cols[v],"\" (",v,"/",length(vol.cols),")")
		
		file <- get.path.stats.corpus(object=object, subfold="evolution", arc=cur.arc, pref=vol.fnames[v])
		tlog(4,"Producing files \"",file,"\"")
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
	
	
	##################
	# attribute-based stats
	if(att.nbr>0)
	{	# distribution of character numbers (by attribute)
		for(att in atts)
		{	tlog(4,"Computing attribute \"",att,"\"")
			data <- volume.stats.atts[[att]][,-1,drop=FALSE]
			pal <- ATT_COLORS[[att]]
			if(length(pal)==0) 
			{	pal <- get.palette(ncol(data))
				names(pal) <- colnames(data)
			}
			else
				pal <- pal[colnames(data)]
			
#			# plot for each volume
#			if(is.na(cur.arc))
#			{	for(v in 1:volume.nbr)
#				{	vname <- paste0(v,"_",volume.stats[v,COL_VOLUME])
#					# attribute distribution over the characters
#					vals <- data[v,]
#					perc <- vals/sum(vals)*100
#					df <- data.frame(names(vals), vals, perc, stringsAsFactors=FALSE, check.names=FALSE)
#					colnames(df) <- c(att,"Frequency","Proportion")
#					file <- get.path.stats.corpus(pref="attr_distrib", vol=vname, att=att)
#					tlog(4,"Producing files \"",file,"\"")
#					write.csv(x=df, file=paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
#					#
#					for(fformat in PLOT_FORMAT)
#					{	if(fformat==PLOT_FORMAT_PDF)
#							pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
#						else if(fformat==PLOT_FORMAT_PNG)
#							png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
#						barplot(
#							height=perc,
#							main=paste0("Distribution of character attribute ",att," (%)"),
#							col=pal
#						)
#						dev.off()
#					}
#					
#					# others?
#				}
#			}
#vTODO already plotted in the characters subfolder
			
			# plot for the attribute
			file <- get.path.stats.corpus(object=object, arc=cur.arc, pref="distrib_chars-by-volume", att=att)
			tlog(4,"Distribution of character numbers for attribute \"",att,"\": producing files \"",file,"\"")
			ml <- paste0("Character number distribution over volumes (att=",att)
			xl <- "Number of characters by volume"
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
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=TRUE,								# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal,
						#space=0,
						args.legend=list(x="topright"),
						legend.text=colnames(data)
					)
				dev.off()
			}
			# separate plot for each value
			for(d in 2:ncol(volume.stats.atts[[att]]))
			{	data <- volume.stats.atts[[att]][,d]
				if(any(data!=0))
				{	val <- colnames(volume.stats.atts[[att]])[d]
					file <- get.path.stats.corpus(object=object, arc=cur.arc, pref="distrib_chars-by-volume", att=att, val=colnames(volume.stats.atts[[att]])[d])
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
	
	# TODO plot the distributions obtained for all volumes on the same plot? (using lines instead of points)
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
		volume.stats, 
		arc.stats, arc.stats.atts, arc.stats.indiv)
{	object <- "arcs"
	
	# preparation
	tlog(3,"Computing volume stats (whole series)")
	arc.idx <- arc.stats[,COL_ARC_ID]
	
	# arcs
	arc.nbr <- nrow(arc.stats)
	col <- ATT_COLORS_FILT["Discard"]
	# vertex attributes
	atts <- names(arc.stats.atts)
	att.nbr <- length(atts)
	
	
	##################
	# attribute-independent stats
	tlog(4,"Computing attribute-independent arc stats")
	
	# plot the stats for each arc
	tlog(4,"Processing each arc separately")
	for(a in 1:arc.nbr)
	{	tlog(5,"Processing arc ",a,"/",nrow(arc.stats))
		
		arc.panel.stats <- arc.stats.indiv[[a]]$arc.panel.stats
		arc.panel.stats.atts <- arc.stats.indiv[[a]]$arc.panel.stats.atts
		arc.page.stats <- arc.stats.indiv[[a]]$arc.page.stats
		arc.page.stats.atts <- arc.stats.indiv[[a]]$arc.page.stats.atts
		arc.scene.stats <- arc.stats.indiv[[a]]$arc.scene.stats
		arc.scene.stats.atts <- arc.stats.indiv[[a]]$arc.scene.stats.atts
		arc.char.stats <- arc.stats.indiv[[a]]$arc.char.stats
		arc.volume.stats <- arc.stats.indiv[[a]]$arc.volume.stats
		arc.volume.stats.atts <- arc.stats.indiv[[a]]$arc.volume.stats.atts
		
		# plot arc-specific stats
		plot.stats.panel(panel.stats=arc.panel.stats, panel.stats.atts=arc.panel.stats.atts, volume.stats=volume.stats, cur.arc=a)
		plot.stats.page(page.stats=arc.page.stats, page.stats.atts=arc.page.stats.atts, volume.stats=volume.stats, cur.arc=a)
		plot.stats.scene(scene.stats=arc.scene.stats, scene.stats.atts=arc.scene.stats.atts, volume.stats=volume.stats, cur.arc=a)
		plot.stats.char(char.stats=arc.char.stats, volume.stats=volume.stats, cur.arc=a)
		plot.stats.volume(volume.stats=arc.volume.stats, volume.stats.atts=arc.volume.stats.atts, cur.arc=a)
		
		# density plot: chars vs. panels (by scene)
		file <- get.path.stats.corpus(arc=a, pref="distrib_chars-by-scene_vs_panels-by-scene")
		tlog(6,"Distribution of characters by scene vs. panels by scene: producing files \"",file,"\"")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			xvals <- arc.scene.stats[,COL_CHARS]
			yvals <- arc.scene.stats[,COL_PANELS]
			xlab <- "Number of characters by scene"
			ylab <- "Number of panels by scene"
			p=ggplot(arc.scene.stats, aes(x=xvals, y=yvals)) +
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
	}
	
	# distributions of panel numbers
	vals <- table(arc.stats[arc.idx,COL_PANELS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PANELS, COL_ARCS,"Proportion")
	file <- get.path.stats.corpus(object=object, pref="distrib_panel-by-arc")
	tlog(4,"Distribution of panel numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- arc.stats[arc.idx,COL_PANELS]
	ml <- "Panel number distribution over arcs"
	xl <- "Number of panels by arc"
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
#				x <- h$breaks[2:length(h$breaks)]
#			y <- h$density
#			idx <- which(y>0)
#			x <- x[idx]
#			y <- y[idx]
#			expmax <- floor(log(min(y),10))
#			plot(x, y, col=col, xlab=xl, ylab="Density", main=ml, log="xy", yaxt="n") #las=1
#			axis(side=2, at=10^(expmax:0), label=parse(text=paste("10^", expmax:0, sep="")), las=1)
			# complementary cumulative distribution function
			if(length(unique(data))>1)
				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
	if(DO_STAT_TESTS && length(unique(data))>1)
	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
	}

	# distributions of character numbers
	vals <- table(arc.stats[arc.idx,COL_CHARS])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_CHARS, COL_ARCS, "Proportion")
	file <- get.path.stats.corpus(object=object, pref="distrib_chars-by-arc")
	tlog(4,"Distribution of character numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- arc.stats[arc.idx,COL_CHARS]
	ml <- "Character number distribution over arcs"
	xl <- "Number of characters by arc"
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	
	# distributions of page numbers
	vals <- table(arc.stats[arc.idx,COL_PAGES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_PAGES, COL_ARCS,"Proportion")
	file <- get.path.stats.corpus(object=object, pref="distrib_pages-by-arc")
	tlog(4,"Distribution of page numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- arc.stats[arc.idx,COL_PAGES]
	ml <- "Page number distribution over arcs"
	xl <- "Number of pages by arc"
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
#	# check distribution
#	if(DO_STAT_TESTS && length(unique(data))>1 && is.na(cur.arc))
#	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
#		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
#	}
	
	# distributions of scene numbers
	vals <- table(arc.stats[arc.idx,COL_SCENES])
	vals <- cbind(as.integer(names(vals)), vals, 100*vals/sum(vals))
	colnames(vals) <- c(COL_SCENES, COL_ARCS,"Proportion")
	file <- get.path.stats.corpus(object=object, pref="distrib_scenes-by-arc")
	tlog(4,"Distribution of scene numbers: producing files \"",file,"\"")
	write.csv(x=vals, paste0(file,".csv"), row.names=FALSE)#, col.names=TRUE)
	#
	data <- arc.stats[arc.idx,COL_SCENES]
	ml <- "Scene number distribution over arcs"
	xl <- "Number of scenes by arc"
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
#			if(length(unique(data))>1)
#				plot.ccdf(data=data, main=ml, xlab=xl, ylab="default", log=TRUE, cols=col)
		dev.off()
	}
	# check distribution
#	if(DO_STAT_TESTS && length(unique(data))>1)
#	{	distr.stats <- test.disc.distr(data=data, xlab=xl, return_stats=TRUE, plot.file=paste0(file,"_distrtest"))
#		write.table(distr.stats, file=paste0(file,"_distrtest.csv"), sep=",", row.names=FALSE, col.names=TRUE)
#	}
	
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
		"correlation between the numbers of characters and panels, by scene", "correlation between the numbers of scenes and panels, by character"
	)
	arc.fnames <- c(
		"volumes", "volumes-by-char",
		"pages", "pages-by-volume", "pages-by-scene", "pages-by-char",
		"scenes", "scenes-by-volume", "scenes-by-page", "scenes-by-char",
		"panels", "panels-by-volume", "panels-by-page", "panels-by-scene", "panels-by-char", 
		"chars", "chars-by-volume", "chars-by-page", "chars-by-scene", "chars-by-panel",
		"corr_chars-by-scene_vs_panels-by-scene", "corr_scenes-by-char_vs_panels-by-char"
	)
	tlog(4,"Generating the plots")
	for(a in 1:length(arc.cols))
	{	tlog(5,"Processing column \"",arc.cols[a],"\" (",a,"/",length(arc.cols),")")
		
		file <- get.path.stats.corpus(object=object, subfold="evolution", pref=arc.fnames[a])
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				barplot(
					height=arc.stats[,arc.cols[a]],
					names.arg=arc.stats[,COL_ARC_ID],
					main=paste0("Evolution of the ",arc.ttls[a]),
					col=col
				)
			dev.off()
		}
	}
	
	
	##################
	# attribute-based stats
	if(att.nbr>0)
	{	# distribution of character numbers (by attribute)
		for(att in atts)
		{	tlog(4,"Computing attribute \"",att,"\"")
			data <- arc.stats.atts[[att]][arc.idx,-1,drop=FALSE]
			pal <- ATT_COLORS[[att]]
			if(length(pal)==0) 
			{	pal <- get.palette(ncol(data))
				names(pal) <- colnames(data)
			}
			else
				pal <- pal[colnames(data)]
			
			# plot for the attribute
			file <- get.path.stats.corpus(object=object, pref="distrib_chars-by-arc", att=att)
			tlog(4,"Distribution of character numbers for attribute \"",att,"\": producing files \"",file,"\"")
			ml <- paste0("Character number distribution over arcs (att=",att)
			xl <- "Number of characters by arc"
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
						xlab=xl, ylab=yl, main=paste0(ml,")"),
						beside=TRUE,								# stacked (FALSE) vs. grouped (TRUE) bars
						col=pal,
						#space=0,
						args.legend=list(x="topright"),
						legend.text=colnames(data)
					)
				dev.off()
			}
			# separate plot for each attribute value
			for(d in 2:ncol(arc.stats.atts[[att]]))
			{	data <- arc.stats.atts[[att]][arc.idx,d]
				if(any(data!=0))
				{	val <- colnames(arc.stats.atts[[att]])[d]
					file <- get.path.stats.corpus(object=object, pref="distrib_chars-by-arc", att=att, val=colnames(arc.stats.atts[[att]])[d])
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
	
	# TODO plot all arc distributions on the same plot?
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
plot.stats.series <- function(
		scene.stats, 
		char.stats)
{	object <- NA
	
	# preparation
	tlog(3,"Plotting series stats")
	# vertex attributes
	atts <- setdiff(colnames(char.stats), COLS_ATT_IGNORE)
	att.nbr <- length(atts)
	
	
	##################
	# attribute-independent stats
	tlog(4,"Plotting attribute-independent series stats")
	
	# density plot: chars/scene vs. panels/scene
	file <- get.path.stats.corpus(pref="distrib_panels-by-scene_vs_chars-by-scene")
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
	
	# density plot: scenes/char vs. panels/char
	file <- get.path.stats.corpus(pref="distrib_panels-by-char_vs_scenes-by-char")
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
	
	# attribute stats
	tlog(3,"Nothing to plot for attribute stats")
	# nothing to do here
}



	
###############################################################################
# Computes and records some statistics regarding the parsed corpus.
#
# data: previously read raw data.
#
# returns: same data, completed with stats.
###############################################################################
plot.corpus.stats <- function(data)
{	tlog(2,"Computing corpus stats")

	# panels
	panel.stats <- data$panel.stats
	panel.stats.atts <- data$panel.stats.atts
	panel.chars <- data$panel.chars
	# pages
	page.stats <- data$page.stats
	page.stats.atts <- data$page.stats.atts
	page.chars <- data$page.chars
	# scenes
	scene.stats <- data$scene.stats
	scene.stats.atts <- data$scene.stats.atts
	scene.chars <- data$scene.chars
	# chars
	char.stats <- data$char.stats
	# volumes
	volume.stats <- data$volume.stats
	volume.stats.atts <- data$volume.stats.atts
	volume.stats.indiv <- data$volume.stats.indiv
	volume.chars <- data$volume.chars 
	# arcs
	arc.stats <- data$arc.stats
	arc.stats.atts <- data$arc.stats.atts
	arc.stats.indiv <- data$arc.stats.indiv
	arc.chars <- data$arc.chars
	# series
	series.stats <- data$series.stats
	
	# plot panel stats
	plot.stats.panel(
		panel.stats=panel.stats, panel.stats.atts=panel.stats.atts,
		volume.stats,
		cur.vol=NA, cur.arc=NA
	)
	
	# plot page stats
	plot.stats.page(
		page.stats=page.stats, page.stats.atts=page.stats.atts,
		volume.stats,
		cur.vol=NA, cur.arc=NA
	)
	
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
	
	# plot volume stats
	plot.stats.volume(
		volume.stats=volume.stats, volume.stats.atts=volume.stats.atts, volume.stats.indiv=volume.stats.indiv,
		cur.arc=NA
	)
	
	# plot arc stats
	plot.stats.arc(
		volume.stats=volume.stats, 
		arc.stats=arc.stats, arc.stats.atts=arc.stats.atts, arc.stats.indiv=arc.stats.indiv
	)
	
	# plot series stats
	plot.stats.series(
		scene.stats=scene.stats,
		char.stats=char.stats
	)
}
