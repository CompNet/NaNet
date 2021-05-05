# This scripts contains all file- and path-related constants and functions.
# 
# Vincent Labatut
# 01/2019
###############################################################################


###############################################################################
# folder containing the log files
LOG_FOLDER <- "log"


###############################################################################
# folder containing the extracted network files
NET_FOLDER <- file.path(DATA_FOLDER,"networks")
dir.create(path=NET_FOLDER, showWarnings=FALSE, recursive=TRUE)
# folder containing the produced stat files
STAT_FOLDER <- file.path(DATA_FOLDER,"stats")
dir.create(path=STAT_FOLDER, showWarnings=FALSE, recursive=TRUE)
	# folder containing the corpus stat files
	STAT_CORPUS_FOLDER <- file.path(STAT_FOLDER,"corpus")
	dir.create(path=STAT_CORPUS_FOLDER, showWarnings=FALSE, recursive=TRUE)
		# folder containing the corpus character stat files
		STAT_CORPUS_CHARS_FOLDER <- file.path(STAT_CORPUS_FOLDER,"characters")
		dir.create(path=STAT_CORPUS_CHARS_FOLDER, showWarnings=FALSE, recursive=TRUE)
		# folder containing the corpus page stat files
		STAT_CORPUS_PAGES_FOLDER <- file.path(STAT_CORPUS_FOLDER,"pages")
		dir.create(path=STAT_CORPUS_PAGES_FOLDER, showWarnings=FALSE, recursive=TRUE)
		# folder containing the corpus panel stat files
		STAT_CORPUS_PANELS_FOLDER <- file.path(STAT_CORPUS_FOLDER,"panels")
		dir.create(path=STAT_CORPUS_PANELS_FOLDER, showWarnings=FALSE, recursive=TRUE)
		# folder containing the corpus scene stat files
		STAT_CORPUS_SCENES_FOLDER <- file.path(STAT_CORPUS_FOLDER,"scenes")
		dir.create(path=STAT_CORPUS_SCENES_FOLDER, showWarnings=FALSE, recursive=TRUE)
		# folder containing the corpus volume stat files
		STAT_CORPUS_VOLUMES_FOLDER <- file.path(STAT_CORPUS_FOLDER,"volumes")
		dir.create(path=STAT_CORPUS_VOLUMES_FOLDER, showWarnings=FALSE, recursive=TRUE)
# folder containing the produced plot files
PLOT_FOLDER <- file.path(DATA_FOLDER,"plots")
dir.create(path=PLOT_FOLDER, showWarnings=FALSE, recursive=TRUE)


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
# Returns the basename of the static graph based on the specified parameters.
# 
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter value.
# 
# Returns: basename of the graph and related files.
###############################################################################
get.graphname.static <- function(mode, window.size=NA, overlap=NA)
{	res <- "static"
	
	if(mode=="scenes")
		res <- 	file.path(NET_FOLDER, paste0(res, "_scenes"))
	else if(mode=="panel.window")
		res <- 	file.path(NET_FOLDER, paste0(res, "_panels_ws=",window.size,"_ol=",overlap))
	else if(mode=="page.window")
		res <- 	file.path(NET_FOLDER, paste0(res, "_pages_ws=",window.size,"_ol=",overlap))
	
	res <- paste0(res, ".graphml")
	
	return(res)
}


###############################################################################
# Returns the name of a statistics file based on the specified parameters.
# 
# object: either "nodes", "nodepairs", "links" or "graph".
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# 
# Returns: basename of the graph and related files.
###############################################################################
get.statname.static <- function(object, mode, window.size=NA, overlap=NA, weights=NA)
{	res <- "static"
	
	if(mode=="scenes")
		res <- 	file.path(STAT_FOLDER, paste0(res, "_scenes_wt=",weights))
	else if(mode=="panel.window")
		res <- 	file.path(STAT_FOLDER, paste0(res, "_panels_ws=",window.size,"_ol=",overlap))
	else if(mode=="page.window")
		res <- 	file.path(STAT_FOLDER, paste0(res, "_pages_ws=",window.size,"_ol=",overlap))
	
	res <- paste0(res, "_meas")
	
	if(object=="graph")
		res <- 	paste0(res, "_graph.csv")
	else if(object=="nodes")
		res <- 	paste0(res, "_nodes.csv")
	else if(object=="nodepairs")
		res <- 	paste0(res, "_nodepairs.csv")
	else if(object=="links")
		res <- 	paste0(res, "_links.csv")
	else if(object=="corr")
		res <- 	paste0(res, "_corr.csv")
	
	return(res)
}




###############################################################################
# Returns the name of the plot file based on the specified parameters.
# 
# object: either "nodes", "nodepairs", "links" or "graph".
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter value.
# 
# Returns: basename of the graph and related files.
###############################################################################
get.plotname.static <- function(object, mode, window.size=NA, overlap=NA)
{	res <- "static"
	
	if(mode=="panel.window")
	{	res <- 	file.path(PLOT_FOLDER, paste0(res, "_panels"))
		if(!is.na(window.size))
			res <- paste0(res, "_ws=",window.size)
		if(!is.na(overlap))
			res <- paste0(res, "_ol=",overlap)
	}
	else if(mode=="page.window")
	{	res <- 	file.path(PLOT_FOLDER, paste0(res, "_pages"))
		if(!is.na(window.size))
			res <- paste0(res, "_ws=",window.size)
		if(!is.na(overlap))
			res <- paste0(res, "_ol=",overlap)
	}
	
	if(object=="graph")
		res <- 	paste0(res, "_graph")
	else if(object=="nodes")
		res <- 	paste0(res, "_nodes")
	else if(object=="nodepairs")
		res <- 	paste0(res, "_nodepairs")
	else if(object=="links")
		res <- 	paste0(res, "_links")
	
	return(res)
}
