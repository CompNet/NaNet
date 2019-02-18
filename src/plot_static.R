# This script contains functions related to the generation of plots representing
# the previously computed statistics.
# 
# Vincent Labatut
# 02/2019
###############################################################################


# graph measures based on assortativity
assort.groups <- list(
		c("betweenness", "betweenness-norm", "betweenness-weighted", "betweenness-weighted-norm"),
		c("closeness", "closeness-norm", "closeness-weighted", "closeness-weighted-norm"),
		c("degree", "degree-norm", "strength"),
		c("eccentricity"),
		c("eigenvector", "eigenvector-norm", "eigenvector-weighted", "eigenvector-weighted-norm"),
		c("transitivity-local", "transitivity-weighted-local")
)
assort.suffix <- "-assortativity"
# graph measures based on centralization
ctrlztn.group <- list(
		c("betweenness", "betweenness-norm"),
		c("closeness", "closeness-norm"),
		c("degree", "degree-norm"),
		c("eigenvector", "eigenvector-norm")
)
ctrlztn.suffix <- "-centralization"
# single graph measures
single.group <- c("modularity", "community-number", "modularity-weighted", "community-weighted-number", "component-number",
		"node-count", "link-count", "density", "transitivity-global"
)


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "segments", "panel.window" or "page.window".
# window.size: fixed value for this parameter.
# overlaps: vector of values for this parameter.
# measure: name of the concerned topological measure.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.by.window <- function(mode, window.size, overlaps, measure)
{	res <- rep(NA, length(overlaps))
	for(j in 1:length(overlaps))
	{	overlap <- overlaps[j]
		table.file <- get.statname.static(object="graph", mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[j] <- tmp.tab[measure,1]
	}
	
	return(res)
}


###############################################################################
# Loads a series corresponding to the specified parameters.
#
# mode: either "segments", "panel.window" or "page.window".
# window.sizes: vector of values for this parameter.
# overlap: fixed value for this parameter.
# measure: name of the concerned topological measure.
#
# returns: a vector of values representing the desired series.
###############################################################################
load.static.graph.stats.by.overlap <- function(mode, window.sizes, overlap, measure)
{	res <- rep(NA, length(window.sizes))
	for(i in 1:length(window.sizes))
	{	window.size <- window.sizes[i]
		table.file <- get.statname.static(object="graph", mode=mode, window.size=window.size, overlap=overlap)
		tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		res[i] <- tmp.tab[measure,1]
	}
	
	return(res)
}


###############################################################################
# Computes all preselected nodal topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# basename: name and path of the filename containing the graph.
#
# returns: an nxk table containing all computed values, where n is the number of
#          nodes and k the number of measures.
###############################################################################
generate.static.node.plots <- function(mode, window.sizes, overlap)
{	
}



###############################################################################
# Computes all preselected link topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# basename: name and path of the filename containing the graph.
#
# returns: an mxk table containing all computed values, where m is the number of
#          links and k the number of measures.
###############################################################################
generate.static.link.plots <- function(mode, window.sizes, overlap)
{	
}



###############################################################################
###############################################################################
generate.static.graph.plots.single <- function(mode, window.sizes, overlaps)
{	# process each appropriate measure
	for(meas.name in ASMM_MEASURES)
	{	# generate a plot for each window size value
		for(window.size in window.sizes)
		{	# the series corresponds to the values of the overlap
			avg.vals <- load.static.graph.stats.by.window(mode, window.size, overlaps, paste0(meas.name,SFX_AVG))
			std.vals <- load.static.graph.stats.by.window(mode, window.size, overlaps, paste0(meas.name,SFX_STDEV))
			min.vals <- load.static.graph.stats.by.window(mode, window.size, overlaps, paste0(meas.name,SFX_MIN))
			max.vals <- load.static.graph.stats.by.window(mode, window.size, overlaps, paste0(meas.name,SFX_MAX))
			
			# generate the stdev plot
			plot.file <- get.plotname.static(object="graph", mode=mode, window.size=window.size)
			pdf(file=plot.file,bg="white")
				plot(x=overlaps, y=avg.vals,
						ylim=c(min(avg.vals-std.vals),max(avg.vals+std.vals)),
						xlab="Overlap",
						ylab=""
				)
				segments(overlaps, avg.vals-std.vals, overlaps, avg.vals+std.vals)
				epsilon <- 0.02
				segments(overlaps-epsilon, avg.vals-std.vals, overlaps+epsilon, avg.vals-std.vals)
				segments(overlaps-epsilon, avg.vals+std.vals, overlaps+epsilon, avg.vals+std.vals)
			dev.off()
			
			# generate the min/max plot
			
		}
	}
}



###############################################################################
# Generates the plots related to the graph-related statistics of static graphs.
#
# g: graph whose statistics must be computed.
# basename: name and path of the filename containing the graph.
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
generate.static.graph.plots <- function(mode, window.sizes, overlaps)
{	
	generate.static.graph.plots.single(mode, window.sizes, overlaps)
	generate.static.graph.plots.multiple(mode, window.sizes, overlaps)
	
}



###############################################################################
# Main function for the generation of plots describing static graphs.
# The statistics must have been previously extracted.
#
# panel.window.sizes: values for this parameter
# panel.overlaps: values for this parameter, specified for of the above parameter values.
# page.window.sizes: same for page-based windows instead of panel-based.
# page.overlaps: same.
###############################################################################
generate.static.plots <- function(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Generating plots for static graphs")
	
	# statistics for the segment-based graph
	generate.all.static.plots(get.basename.static.segments())
	
	# statistics for the panel window-based static graphs
	for(i in 1:length(panel.window.sizes))
	{	window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
			generate.all.static.plots(get.basename.static.panel.window(window.size, overlap))
	}
	
	# statistics for the page window-based static graphs
	for(i in 1:length(page.window.sizes))
	{	window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
			generate.all.static.plots(get.basename.static.page.window(window.size, overlap))
	}
	tlog(1,"Generation of plots for static graphs complete")	
}
