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
# volume.stats: table containing the volume characteristics.
# narr.unit: narrative unit of the x axis (scene or chapter).
###############################################################################
draw.volume.rects <- function(ylim, volume.stats, narr.unit)
{	# rectangle colors
	rec.pal <- c("gray90","gray80")
	
	if(narr.unit=="scene")
	{	start.col <- COL_SCENE_START_ID
		end.col <- COL_SCENE_END_ID
	}
	else if(narr.unit=="narr.unit")
	{	start.col <- COL_CHAPTER_START_ID
		end.col <- COL_CHAPTER_END_ID
	}
	
	# draw each volume
	for(v in 1:nrow(volume.stats))
	{	rect(
			xleft=volume.stats[v,start.col], 
			xright=volume.stats[v,end.col], 
			ybottom=ylim[1]-abs(ylim[1])*0.05, 
			ytop=ylim[2], 
			col=rec.pal[(v %% 2)+1], 
			border=NA, density=NA
		)
		text(
			x=(volume.stats[v,start.col]+volume.stats[v,end.col])/2, 
			y=ylim[2], 
			labels=volume.stats[v,COL_VOLUME],
			cex=0.55, adj=c(0.5,1)
		)
	}
}
