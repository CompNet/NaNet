#############################################################################################
# Functions used to produce custom plots.
# 
# 09/2019 Vincent Labatut
#
# source("src/common/plot/plot_stats.R")
#############################################################################################
source("src/common/colors.R")




#############################################################
# Custom histogram.
#
# vals: raw values.
# name: name of the values (used for the x-axis label).
# file: (optional) file name, to record the histogram plot.
#############################################################
custom.hist <- function(vals, name, file)
{	vals <- vals[!is.na(vals)]
	if(length(vals)>0)
	{	for(fformat in PLOT_FORMAT)
		{	if(hasArg(file))
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(paste0(file,fformat), width=25, height=25)
				else if(fformat==PLOT_FORMAT_PNG)
					png(paste0(file,fformat), width=1024, height=1024)
			}
#			par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
			par(mar=c(5.1, 4.1, 4.1, 2.1))
			hist(
					vals,			# data
					col="#ffd6d6",	# bar color
					main=NA,		# no main title
					prob=TRUE,		# frenquency density
					breaks=20,		# number of bars
					xlab=name,		# x-axis label
					ylab="Densite"	# y-axis label
			)
			lines(
					density(vals), 	# density estimate
					lwd=2, 			# line thickness
					col="RED"		# line color
			)
			stripchart(
					vals, 			# data
					at=0.02, 		# central position of points (y)
					pch=21, 		# point shape
					col="BLACK", 	# point color
					method="jitter",# noise to avoid overlaps
					jitter=0.02, 	# noise magnitude
					add=TRUE		# add to current plot
			)
			if(hasArg(file))
				dev.off()
		}
	}
}




#############################################################
# Custom barplot.
#
# vals: raw values.
# text: name of the bars.
# xlab: label of the x-axis.
# ylab: label of the y-axis.
# file: (optional) file name, to record the histogram plot.
# ...: additional parameters, fetched to the barplot function.
#############################################################
custom.barplot <- function(vals, text, xlab, ylab, file, ...)
{	idx <- which(is.na(text))
	if(length(idx)>0)
		text[idx] <- ATT_VAL_UNK0
	wide <- length(text) > 8
	
	for(fformat in PLOT_FORMAT)
	{	if(hasArg(file))
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(paste0(file,fformat), width=25, height=25)
			else if(fformat==PLOT_FORMAT_PNG)
				png(paste0(file,fformat), width=1024, height=1024)
		}
#		par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
		if(wide)
			par(mar=c(9, 4, 1, 0)+0.1)
		else
			par(mar=c(5, 4, 1, 0)+0.1)
		if(length(dim(vals))<=1)
		{	barplot(
				height=vals,				# data
				names.arg=text,				# bar names
				col="#ffd6d6",				# bar color
				main=NA,					# no main title
				xlab=if(wide) NA else xlab,	# x-axis label
				ylab=ylab,					# y-axis label
				las=if(wide) 2 else 0,		# vertical label if too many bars
				...
			)
		}
		else
		{	cols <- get.palette(values=nrow(vals))
			barcols <- cols[(1:nrow(vals)-1) %% length(cols)+1]
			barplot(
				height=vals,				# data
				names.arg=text,				# bar names
				beside=TRUE,				# grouped bars
				col=barcols,				# bar colors
				main=NA,					# no main title
				xlab=if(wide) NA else xlab,	# x-axis label
				ylab=ylab,					# y-axis label
				las=if(wide) 2 else 0,		# vertical label if too many bars
				...
			)
			text2 <- rownames(vals)
			idx <- which(is.na(text2))
			if(length(idx)>0)
				text2[idx] <- ATT_VAL_UNK0
			legend(
				x="topleft",
				fill=barcols,
				title=names(dimnames(vals))[1],
				legend=text2
			)
		}
		if(hasArg(file))
			dev.off()
	}
}




###############################################################################
# Draws rectangles corresponding to volumes, on plots representing the evolution
# of some quantity for dynamic graphs.
#
# ylim: limits of the y axis in the plot.
# volume.stats: table containing the volume characteristics.
###############################################################################
draw.volume.rects <- function(ylim, volume.stats)
{	# rectangle colors
	rec.pal <- c("gray90","gray80")
	
	# draw each volume
	for(v in 1:nrow(volume.stats))
	{	rect(
			xleft=volume.stats[v,COL_SCENE_START_ID], 
			xright=volume.stats[v,COL_SCENE_END_ID], 
			ybottom=ylim[1]-abs(ylim[1])*0.05, 
			ytop=ylim[2], 
			col=rec.pal[(v %% 2)+1], 
			border=NA, density=NA
		)
		text(
			x=(volume.stats[v,COL_SCENE_START_ID]+volume.stats[v,COL_SCENE_END_ID])/2, 
			y=ylim[2], 
			labels=volume.stats[v,COL_VOLUME],
			cex=0.55, adj=c(0.5,1)
		)
	}
}
