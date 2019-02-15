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
# Returns the basename of the static graph based on segments.
# 
# Returns: basename of the graph extracted from narrative segments.
###############################################################################
get.basename.static.segments <- function()
{	res <- 	file.path(NET_FOLDER, "static_segments")
	return(res)
}

###############################################################################
# Returns the basename of the static graph based on panel sliding window.
# 
# Returns: basename of the graph extracted from the panel-based sliding window.
###############################################################################
get.basename.static.panel.window <- function(window.size, overlap)
{	res <- 	file.path(NET_FOLDER, paste0("static_panels_ws=",window.size,"_ol=",overlap))
	return(res)
}

###############################################################################
# Returns the basename of the static graph based on page sliding window.
# 
# Returns: basename of the graph extracted from the page-based sliding window.
###############################################################################
get.basename.static.page.window <- function(window.size, overlap)
{	res <- 	file.path(NET_FOLDER, paste0("static_pages_ws=",window.size,"_ol=",overlap))
	return(res)
}
