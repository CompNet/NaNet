# This scripts contains all file- and path-related constants and functions.
# 
# Vincent Labatut
# 01/2019
#
# source("src/common/file_system.R")
###############################################################################


###############################################################################
# folder containing the other character networks
CHARNETS_FOLDER <- file.path("data","charnets")
# folder containing the log files
LOG_FOLDER <- "log"

# folder containing the extracted network files
NET_FOLDER <- file.path(DATA_FOLDER,"networks")
	# folder containing the page network files
	NET_PAGES_FOLDER <- file.path(NET_FOLDER,"pages")
	# folder containing the panel network files
	NET_PANELS_FOLDER <- file.path(NET_FOLDER,"panels")
	# folder containing the scene network files
	NET_SCENES_FOLDER <- file.path(NET_FOLDER,"scenes")

# folder containing the produced stat files
STAT_FOLDER <- file.path(DATA_FOLDER,"stats")
	# folder containing the corpus stat files
	STAT_CORPUS_FOLDER <- file.path(STAT_FOLDER,"corpus")
		# folder containing the corpus arc stat files
		STAT_CORPUS_ARCS_FOLDER <- file.path(STAT_CORPUS_FOLDER,"arcs")
		# folder containing the corpus character stat files
		STAT_CORPUS_CHARS_FOLDER <- file.path(STAT_CORPUS_FOLDER,"characters")
		# folder containing the corpus page stat files
		STAT_CORPUS_PAGES_FOLDER <- file.path(STAT_CORPUS_FOLDER,"pages")
		# folder containing the corpus panel stat files
		STAT_CORPUS_PANELS_FOLDER <- file.path(STAT_CORPUS_FOLDER,"panels")
		# folder containing the corpus scene stat files
		STAT_CORPUS_SCENES_FOLDER <- file.path(STAT_CORPUS_FOLDER,"scenes")
		# folder containing the corpus volume stat files
		STAT_CORPUS_VOLUMES_FOLDER <- file.path(STAT_CORPUS_FOLDER,"volumes")
	# folder containing the page stat files
	STAT_PAGES_FOLDER <- file.path(STAT_FOLDER,"pages")
	# folder containing the panel stat files
	STAT_PANELS_FOLDER <- file.path(STAT_FOLDER,"panels")
	# folder containing the scene stat files
	STAT_SCENES_FOLDER <- file.path(STAT_FOLDER,"scenes")
		# folder containing the scene stat files for duration weights
		STAT_SCENES_DURATION_FOLDER <- file.path(STAT_SCENES_FOLDER,"duration")
		# folder containing the scene stat files for occurrences weights
		STAT_SCENES_OCCURRENCES_FOLDER <- file.path(STAT_SCENES_FOLDER,"occurrences")
		
# folder containing the produced comparison plots
COMP_FOLDER <- file.path(DATA_FOLDER,"comparison")
	# folder containing the page comparison plots
	COMP_PAGES_FOLDER <- file.path(COMP_FOLDER,"pages")
	# folder containing the panel comparison plots
	COMP_PANELS_FOLDER <- file.path(COMP_FOLDER,"panels")
	# folder containing the scene comparison plots
	COMP_SCENES_FOLDER <- file.path(COMP_FOLDER,"scenes")
		# folder containing the scene comparison plots for duration weights
		COMP_SCENES_DURATION_FOLDER <- file.path(COMP_SCENES_FOLDER,"duration")
		# folder containing the scene comparison plots for occurrences weights
		COMP_SCENES_OCCURRENCES_FOLDER <- file.path(COMP_SCENES_FOLDER,"occurrences")
		



###############################################################################
# file containig the page information (number of panels, etc.)
PAGE_FILE <- file.path(DATA_FOLDER,"pages.csv")
# file containing the volume information (number of pages, etc.) 
VOLUME_FILE <- file.path(DATA_FOLDER,"volumes.csv")
# file containing character (co-)occurrences
INTER_FILE <- file.path(DATA_FOLDER,"interactions.txt")
# file containing character descriptions
CHAR_FILE <- file.path(DATA_FOLDER,"characters.csv")




###############################################################################
# Returns the full path for a graph file, based on the specified parameters. 
# File extension is included (.graphml).
# 
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter value.
# arc: concerned narrative arc (optional).
# vol: concerned volume (optional).
# filtered: whether this concerns the filtered version of the graph.
# 
# returns: full path.
###############################################################################
get.path.graph.file <- function(mode, window.size=NA, overlap=NA, arc=NA, vol=NA, filtered=FALSE)
{	# base folder
	if(mode=="panel.window")
		folder <- NET_PANELS_FOLDER
	else if(mode=="page.window")
		folder <- NET_PAGES_FOLDER
	else if(mode=="scenes")
		folder <- NET_SCENES_FOLDER
	# possibly add arc subfolder
	if(!is.na(arc))
		folder <- file.path(folder, "arcs")
	# possibly add volume subfolder
	if(!is.na(vol))
		folder <- file.path(folder, "volumes")
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	res <- file.path(folder, "static")
	# possibly add base name
	if(mode=="scenes")
		res <- 	paste0(res, "_scenes")
	# possibly add arc number
	if(!is.na(arc))
		res <- 	paste0(res, "_arc",arc)
	# possibly add volume name
	if(!is.na(vol))
		res <- 	paste0(res, "_vol",vol)
	# possible add filtered suffix
	if(filtered)
		res <- paste0(res, "_filtered")
	# possibly add window size
	if(!is.na(window.size))
		res <- paste0(res, "_ws=",window.size)
	# possibly add overlap
	if(!is.na(overlap))
		res <- paste0(res, "_ol=",overlap)
	
	# add extension
	res <- paste0(res, ".graphml")
	
	return(res)
}




###############################################################################
# Returns the full path for a table file containing the values of some topological 
# measure. File extension is included (.csv).
# 
# object: "nodes", "nodepairs", "links", "graph"...
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether this concerns the filtered version of the graph.
# 
# returns: full path.
###############################################################################
get.path.stat.table <- function(object, mode, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=FALSE)
{	# base folder
	if(mode=="panel.window")
		folder <- STAT_PANELS_FOLDER
	else if(mode=="page.window")
		folder <- STAT_PAGES_FOLDER
	else if(mode=="scenes")
		folder <- STAT_SCENES_FOLDER
	# possible add filtered suffix
	if(filtered)
		folder <- paste0(folder, "_filtered")
	# possibly add arc subfolder
	if(!is.na(arc))
		folder <- file.path(folder, "arcs", arc)
	# possibly add volume subfolder
	if(!is.na(vol))
		folder <- file.path(folder, "volumes", vol)
	# possibly add window size
	if(!is.na(window.size))
	{	folder <- file.path(folder, paste0("ws=",window.size))
		# possibly add overlap
		if(!is.na(overlap))
			folder <- paste0(folder, "_ol=",overlap)
	}
	# possibly add overlap
	else if(!is.na(overlap))
		folder <- file.path(folder, paste0("ol=",overlap))
	# possibly add scene weights
	if(!is.na(weights))
		folder <- file.path(folder, weights)
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	res <- file.path(folder, "static")
	# possibly add object
	if(!is.na(object))
		res <- 	paste0(res, "_", object)
	
	# add extension
	res <- paste0(res, ".csv")
	
	return(res)
}




###############################################################################
# Returns the full path for a file containing a plot displaying the topological
# measure of some network. File extension is not included and must be added 
# a posteriori.
# 
# object: "nodes", "nodepairs", "links", "graph"...
# mode: either "scenes", "panel.window", or "page.window".
# meas.name: file-ready name of the measure.
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether this concerns the filtered version of the graph.
# plot.type: type of plot (barplot, histogram, etc.).
# 
# returns: full path.
###############################################################################
get.path.topomeas.plot <- function(object, mode, meas.name=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=FALSE, plot.type=NA)
{	# base folder
	if(mode=="panel.window")
		folder <- STAT_PANELS_FOLDER
	else if(mode=="page.window")
		folder <- STAT_PAGES_FOLDER
	else if(mode=="scenes")
		folder <- STAT_SCENES_FOLDER
	# possible add filtered suffix
	if(filtered)
		folder <- paste0(folder, "_filtered")
	# possibly add arc subfolder
	if(!is.na(arc))
		folder <- file.path(folder, "arcs", arc)
	# possibly add volume subfolder
	if(!is.na(vol))
		folder <- file.path(folder, "volumes", vol)
	# possibly add window size
	if(!is.na(window.size))
	{	folder <- file.path(folder, paste0("ws=",window.size))
		# possibly add overlap
		if(!is.na(overlap))
			folder <- paste0(folder, "_ol=",overlap)
	}
	# possibly add overlap
	else if(!is.na(overlap))
		folder <- file.path(folder, paste0("ol=",overlap))
	# possibly add scene weights
	if(!is.na(weights))
		folder <- file.path(folder, weights)
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	res <- file.path(folder, "static")
	# possibly add object
	if(!is.na(object))
		res <- 	paste0(res, "_", object)
	# add measure
	if(!is.na(meas.name))
		res <- paste0(res, "_meas=",meas.name)
	# possibly add plot type
	if(!is.na(plot.type))
		res <- paste0(res, "_", plot.type)
	
	return(res)
}




###############################################################################
# Returns the full path for a file containing a plot of some network(s) topological
# measures with one of the references. File extension is not included and must 
# be added a posteriori.
# 
# object: "nodes", "nodepairs", "links", "graph"...
# mode: either "scenes", "panel.window", or "page.window".
# meas.name: name of the plot measure.
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter value.
# weights: value for this parameters, either "duration" or "occurrences".
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether this concerns the filtered version of the graph.
# plot.type: type of plot (barplot, histogram, etc.).
# 
# returns: full path.
###############################################################################
get.path.comparison.plot <- function(object, mode, meas.name=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=FALSE, plot.type=NA)
{	# base folder
	if(mode=="panel.window")
		folder <- COMP_PANELS_FOLDER
	else if(mode=="page.window")
		folder <- COMP_PAGES_FOLDER
	else if(mode=="scenes")
		folder <- COMP_SCENES_FOLDER
	# possible add filtered suffix
	if(filtered)
		folder <- paste0(folder, "_filtered")
	# possibly add arc subfolder
	if(!is.na(arc))
		folder <- file.path(folder, "arcs", arc)
	# possibly add volume subfolder
	if(!is.na(vol))
		folder <- file.path(folder, "volumes", vol)
	# add object folder
	folder <- file.path(folder, object)
	# possibly add measure folder
	if(!is.na(meas.name))
		folder <- file.path(folder, meas.name)
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	res <- file.path(folder, "static")
	# possibly add window size
	if(!is.na(window.size))
		res <- paste0(res, "_ws=",window.size)
	# possibly add overlap
	if(!is.na(overlap))
		res <- paste0(res, "_ol=",overlap)
	# possibly add scene weights
	if(!is.na(weights))
		res <- paste0(res, "_wt=", weights)
	# possibly add plot type
	if(!is.na(plot.type))
		res <- paste0(res, "_", plot.type)
	
	return(res)
}




###############################################################################
# Returns the full path for a file containing stats regarding the corpus. The
# extension is not included as the same basename can be used for both csv and plots.
# 
# object: "panels", "scenes", "characters", etc.
# vol: id of the considered volume.
# arc: id of the considered narrative arc.
# desc: ad hoc description of the file.
# att: vertex attribute concerned.
# val: considered value of the vertex attribute.
# 
# returns: full path.
###############################################################################
get.path.stat.corpus <- function(object=NA, vol=NA, arc=NA, desc, att=NA, val=NA)
{	# base folder
	if(!is.na(vol))
		folder <- file.path(STAT_CORPUS_VOLUMES_FOLDER, vol)
	else if(!is.na(arc))
		folder <- file.path(STAT_CORPUS_ARCS_FOLDER, arc)
	else
		folder <- STAT_CORPUS_FOLDER
	# possibly add object
	if(!is.na(object))
		folder <- file.path(folder, object)
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	res <- file.path(folder, desc)
	# possibly add attribute name
	if(!is.na(att))
		res <- paste0(res,"_att=",att)
	# possibly add attribute value
	if(!is.na(val))
		res <- paste0(res,"_val=",val)
	
	return(res)
}
