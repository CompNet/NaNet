#############################################################################################
# Functions used to produce custom plots.
# 
# 09/2019 Vincent Labatut
#
# source("src/common/plots/_plot_misc.R")
#############################################################################################
source("src/common/plots/colors.R")
source("src/common/plots/plot_distr.R")




###############################################################################
# Draws rectangles corresponding to volumes, on plots representing the evolution
# of some quantity for dynamic graphs.
#
# ylim: limits of the y axis in the plot.
# unit.stats: table containing the scene or chapter characteristics.
# volume.stats: table containing the volume characteristics.
# narr.unit: narrative unit of the x axis (scene or chapter).
###############################################################################
draw.volume.rects <- function(ylim, unit.stats, volume.stats, narr.unit)
{	# rectangle colors
	rec.pal <- c("gray90","gray80")
	
	# identify appropriate columns
	if(narr.unit=="scene")
	{	start.col <- COL_SCENE_START_ID
		end.col <- COL_SCENE_END_ID
		id.col <- COL_SCENE_ID
	}
	else if(narr.unit=="chapter")
	{	start.col <- COL_CHAPTER_START_ID
		end.col <- COL_CHAPTER_END_ID
		id.col <- COL_CHAPTER_ID
	}
	
	# possible text rotation
	angle <- 0
	justf <- c(0.5, 1)
	ymargin <- ""
	if(max(sapply(volume.stats[,COL_VOLUME], nchar))>4)
	{	angle <- -90
		justf <- c(0, 0.5)
		ymargin <- " "
	}
	
	# compute x rectangle bounds
	bounds.x <- c(
		1, 
		(match(volume.stats[1:(nrow(volume.stats)-1),end.col],unit.stats[,id.col]) 
			+ match(volume.stats[2:nrow(volume.stats),start.col],unit.stats[,id.col]))/2,
		match(volume.stats[nrow(volume.stats),end.col],unit.stats[,id.col])
	)
	text.x <- c((match(volume.stats[v,start.col],unit.stats[,id.col]) + 
					match(volume.stats[v,end.col],unit.stats[,id.col]))/2)
	# draw each volume
	for(v in 1:nrow(volume.stats))
	{	rect(
			xleft=bounds.x[v], 
			xright=bounds.x[v+1], 
			ybottom=ylim[1],#-(ylim[2]-ylim[1])*0.05,
			ytop=ylim[2], 
			col=rec.pal[(v %% 2)+1], 
			border=NA, density=NA
		)
		text(
			x=text.x, 
			y=ylim[2], 
			labels=paste0(ymargin,volume.stats[v,COL_VOLUME]),
			cex=0.55,
			srt=angle, adj=justf
		)
	}
}
