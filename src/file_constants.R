# This scripts contains all file- and path-related constants and functions.
# 
# Vincent Labatut
# 01/2019
###############################################################################


###############################################################################
# folder containing the extracted network files
NET_FOLDER <- file.path(DATA_FOLDER,"networks")
dir.create(NET_FOLDER, showWarnings=FALSE)


###############################################################################
# file containig the page information (number of panels, etc.)
PAGE_FILE <- file.path(DATA_FOLDER,"pages.csv")
# file containing the volume information (number of pages, etc.) 
VOLUME_FILE <- file.path(DATA_FOLDER,"volumes.csv")
# file containing character (co-)occurrences
INTER_FILE <- file.path(DATA_FOLDER,"interactions.txt")
# file containing character description
CHAR_FILE <- file.path(DATA_FOLDER,"characters.csv")


###############################################################################
# Returns the basename of the static graph based on the specified parameters.
# 
# mode: either "segments", "panel.window", or "page.window".
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter value.
# 
# Returns: basename of the graph and related files.
###############################################################################
get.basename.static <- function(mode, window.size=NA, overlap=NA)
{	res <- "static"
	
	if(mode=="segments")
		res <- 	file.path(NET_FOLDER, paste0(res, "_segments"))
	else if(mode=="panel.window")
		res <- 	file.path(NET_FOLDER, paste0(res, "_panels_ws=",window.size,"_ol=",overlap))
	else if(mode=="page.window")
		res <- 	file.path(NET_FOLDER, paste0(res, "_pages_ws=",window.size,"_ol=",overlap))
	
	return(res)
}


###############################################################################
# Returns the stat name of the static graph based on the specified parameters.
# 
# object: either "nodes", "links" or "graph".
# mode: either "segments", "panel.window", or "page.window".
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter value.
# 
# Returns: basename of the graph and related files.
###############################################################################
get.statname.static <- function(object, mode, window.size=NA, overlap=NA)
{	res <- paste0(get.basename.static(mode,window.size,overlap), "_meas")
	
	if(object=="graph")
		res <- 	paste0(res, "_graph.csv")
	else if(object=="nodes")
		res <- 	paste0(res, "_nodes.csv")
	else if(object=="links")
		res <- 	paste0(res, "_links.csv")
	
	return(res)
}


