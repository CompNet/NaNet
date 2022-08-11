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

# folder containing additional annotations
ANNOTATIONS_FOLDER <- file.path(DATA_FOLDER,"annotations")
	# folder containing interaction annotations
	ANN_AGREEMENT_FOLDER <- file.path(ANNOTATIONS_FOLDER,"agreement")
	# folder containing hand-made summaries
	ANN_SUMMARIES_FOLDER <- file.path(ANNOTATIONS_FOLDER,"summaries")
	
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
#
# mode: either "scenes", "panel.window", or "page.window".
# net.type: "static", "cumulative", "narr_smooth".
# order: "publication" vs. "story" order for the dynamic networks.
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter value.
# arc: concerned narrative arc (optional).
# vol: concerned volume (optional).
# filtered: whether this concerns the filtered version of the graph.
# subfold: additional subfolder (optional).
# pref: prefix of the file name.
# suf: suffix of the file name.
# ext: file extension added at the end of the path.
# 
# returns: full path.
###############################################################################
get.path.data.graph <- function(mode, net.type, order=NA, window.size=NA, overlap=NA, arc=NA, vol=NA, filtered=NA, subfold=NA, pref=NA, suf=NA, ext=NA)
{	# base folder
	if(mode=="panel.window")
		folder <- NET_PANELS_FOLDER
	else if(mode=="page.window")
		folder <- NET_PAGES_FOLDER
	else if(mode=="scenes")
		folder <- NET_SCENES_FOLDER
	# add filtered folder
	if(!is.na(filtered))
		folder <- file.path(folder, if(filtered) "filtered" else "unfiltered")
	# add network type folder
	folder <- file.path(folder, net.type)
	# possibly add order subfolder
	if(!is.na(order))
		folder <- file.path(folder, order) 
	# possibly add arc subfolder
	if(!is.na(arc))
		folder <- file.path(folder, "arcs")
	# possibly add volume subfolder
	if(!is.na(vol))
		folder <- file.path(folder, "volumes")
	# possibly add extra subfolder
	if(!is.na(subfold))
		folder <- file.path(folder, subfold)
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	fname <- ""
	# possibly add prefix
	if(!is.na(pref))
		fname <- paste0(fname, pref)
	# possibly add arc number
	if(!is.na(arc))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, "arc",arc)
	}
	# possibly add volume name
	if(!is.na(vol))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, "vol",vol)
	}
	# possibly add window size
	if(!is.na(window.size))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, "ws=",window.size)
	}
	# possibly add overlap
	if(!is.na(overlap))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, "ol=",overlap)
	}
	# possibly add suffix
	if(!is.na(suf))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, suf)
	}
	# prevent empty filename 
	if(fname=="")
		fname <- "file"
	# add extension
	if(!is.na(ext))
		fname <- paste0(fname, ext)
	
	res <- file.path(folder, fname)
	return(res)
}




###############################################################################
# Returns the full path for a table file containing the values of some topological 
# measure. File extension is included (.csv).
# 
# object: "nodes", "nodepairs", "links", "graph"...
# mode: either "scenes", "panel.window", or "page.window".
# net.type: "static", "cumulative", "narr_smooth".
# order: "publication" vs. "story" order for the dynamic networks.
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether this concerns the filtered version of the graph.
# filtered: whether this concerns the filtered version of the graph.
# compare: whether to compute the regular stats or to compare with reference graphs.
# 
# returns: full path.
###############################################################################
get.path.stat.table <- function(object, mode, net.type, order=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=NA, compare=FALSE)
{	# base folder
	if(!compare)
	{	if(mode=="panel.window")
			folder <- STAT_PANELS_FOLDER
		else if(mode=="page.window")
			folder <- STAT_PAGES_FOLDER
		else if(mode=="scenes")
			folder <- STAT_SCENES_FOLDER
	}
	else
	{	if(mode=="panel.window")
			folder <- COMP_PANELS_FOLDER
		else if(mode=="page.window")
			folder <- COMP_PAGES_FOLDER
		else if(mode=="scenes")
			folder <- COMP_SCENES_FOLDER
	}
	# possibly add filtered subfolder
	if(!is.na(filtered))
		folder <- file.path(folder, filtered)
	# add network type folder
	folder <- file.path(folder, net.type)
	# possibly add order subfolder
	if(!is.na(order))
		folder <- file.path(folder, order) 
	# possibly add arc subfolder
	if(!is.na(arc) && (!is.logical(arc) || arc))
	{	folder <- file.path(folder, "arcs")
		if(!is.logical(arc))
			folder <- file.path(folder, "separate", arc)
	}
	# possibly add volume subfolder
	if(!is.na(vol) && (!is.logical(vol) || vol))
	{	folder <- file.path(folder, "volumes")
		if(!is.logical(vol))
			folder <- file.path(folder, "separate", vol)
	}
	# possibly add window size folder
	if(!is.na(window.size))
		folder <- file.path(folder, paste0("ws=",window.size))
	# possibly add overlap folder
	if(!is.na(overlap))
		folder <- file.path(folder, paste0("ol=",overlap))
	# add weights
	if(!is.na(weights))	
		folder <- file.path(folder, "weights", weights)
	# possibly add object
	folder <- file.path(folder, object)
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	fname <- "_stats"
	# repeat object
	fname <- paste0(fname, "_", object)
	# add extension
	fname <- paste0(fname, ".csv")
	
	res <- file.path(folder, fname)
	return(res)
}




###############################################################################
# Returns the full path for a file containing a plot displaying the topological
# measure of some network. File extension is not included and must be added 
# a posteriori.
# 
# mode: either "scenes", "panel.window", or "page.window".
# net.type: "static", "cumulative", "narr_smooth".
# order: "publication" vs. "story" order for the dynamic networks.
# att: vertex attribute concerned.
# meas.name: file-ready name of the measure.
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether this concerns the filtered version of the graph (or NA if not applicable).
# pref: prefix of the file name.
# suf: suffix of the file name.
# 
# returns: full path.
###############################################################################
get.path.stats.topo <- function(mode, net.type, order=NA, att=NA, meas.name=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=NA, subfold=NA, pref=NA, suf=NA)
{	# base folder
	if(mode=="panel.window")
		folder <- STAT_PANELS_FOLDER
	else if(mode=="page.window")
		folder <- STAT_PAGES_FOLDER
	else if(mode=="scenes")
		folder <- STAT_SCENES_FOLDER
	# possibly add filtered subfolder
	if(!is.na(filtered))
		folder <- file.path(folder, filtered)
	# add network type folder
	folder <- file.path(folder, net.type)
	# possibly add order subfolder
	if(!is.na(order))
		folder <- file.path(folder, order) 
	# possibly add arc subfolder
	if(!is.na(arc) && (!is.logical(arc) || arc))
	{	folder <- file.path(folder, "arcs")
		if(!is.logical(arc))
			folder <- file.path(folder, "separate", arc)
	}
	# possibly add volume subfolder
	if(!is.na(vol) && (!is.logical(vol) || vol))
	{	folder <- file.path(folder, "volumes")
		if(!is.logical(vol))
			folder <- file.path(folder, "separate", vol)
	}
	# possibly add window size subfolder
	if(!is.na(window.size))
		folder <- file.path(folder, paste0("ws=",window.size))
	# possibly add overlap subfolder
	if(!is.na(overlap))
		folder <- file.path(folder, paste0("ol=",overlap))
	# possibly add attribute name
	if(!is.na(att))
		folder <- file.path(folder,"attributes",att)
	# possibly add weight subfolder
	if(!is.na(weights))
		folder <- file.path(folder, "weights", weights)
	# possibly add extra subfolder
	if(!is.na(subfold))
		folder <- file.path(folder, subfold)
	# possibly add measure object
	if(!is.na(meas.name))
	{	if(is.null(ALL_MEASURES[[meas.name]]))
			folder <- file.path(folder, meas.name)
		else
			folder <- file.path(folder, ALL_MEASURES[[meas.name]]$object, ALL_MEASURES[[meas.name]]$folder)
	}
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	fname <- ""
	# possibly add prefix
	if(!is.na(pref))
		fname <- paste0(fname, pref)
	# add measure
	if(!is.na(meas.name))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, meas.name)
	}
	# possibly add suffix
	if(!is.na(suf))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, suf)
	}
	# prevent empty filename 
	if(fname=="")
		fname <- "file"
	
	res <- file.path(folder, fname)
	return(res)
}




###############################################################################
# Returns the full path for a file containing a plot of some network(s) topological
# measures with one of the references. File extension is not included and must 
# be added a posteriori.
# 
# mode: either "scenes", "panel.window", or "page.window".
# net.type: "static", "cumulative", "narr_smooth".
# order: "publication" vs. "story" order for the dynamic networks.
# meas.name: name of the plot measure.
# window.size: value for this parameter.
# overlap: value for this parameter, specified for of the above parameter value.
# weights: value for this parameters, either "duration" or "occurrences".
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether this concerns the filtered version of the graph.
# pref: prefix of the file name.
# suf: suffix of the file name.
# 
# returns: full path.
###############################################################################
get.path.stats.comp <- function(mode, net.type, order=NA, meas.name=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=NA, pref=NA, suf=NA)
{	# base folder
	if(mode=="panel.window")
		folder <- COMP_PANELS_FOLDER
	else if(mode=="page.window")
		folder <- COMP_PAGES_FOLDER
	else if(mode=="scenes")
		folder <- COMP_SCENES_FOLDER
	# possibly add filtered subfolder
	if(!is.na(filtered))
		folder <- file.path(folder, filtered)
	# add network type folder
	folder <- file.path(folder, net.type)
	# possibly add order subfolder
	if(!is.na(order))
		folder <- file.path(folder, order) 
	# possibly add arc subfolder
	if(!is.na(arc) && (!is.logical(arc) || arc))
	{	folder <- file.path(folder, "arcs")
		if(!is.logical(arc))
			folder <- file.path(folder, "separate", arc)
	}
	# possibly add volume subfolder
	if(!is.na(vol) && (!is.logical(vol) || vol))
	{	folder <- file.path(folder, "volumes")
		if(!is.logical(vol))
			folder <- file.path(folder, "separate", vol)
	}
	# possibly add window size and overlap
	if(!is.na(window.size) && !is.na(overlap))
		folder <- file.path(folder, paste0("ws=", window.size), paste0("ol=", overlap))
	else if(mode!="scenes")
		folder <- file.path(folder, "_all")
	# possibly add weight subfolder
	if(!is.na(weights))
		folder <- file.path(folder, "weights", weights)
	# possibly add measure object
	if(!is.na(meas.name))
	{	if(is.null(ALL_MEASURES[[meas.name]]))
			folder <- file.path(folder, meas.name)
		else
			folder <- file.path(folder, ALL_MEASURES[[meas.name]]$object, ALL_MEASURES[[meas.name]]$folder)
	}
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	fname <- ""
	# possibly add prefix
	if(!is.na(pref))
		fname <- paste0(fname, pref)
	# possibly add window size
	if(!is.na(window.size) && is.na(overlap))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, "ws=",window.size)
	}
	# possibly add overlap
	if(!is.na(overlap) && is.na(window.size))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, "ol=",overlap)
	}
	# possibly add suffix
	if(!is.na(suf))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname, suf)
	}
	# prevent empty filename 
	if(fname=="") 
		fname <- "comp"
	
	res <- file.path(folder, fname)
	return(res)
}




###############################################################################
# Returns the full path for a file containing stats regarding the corpus. The
# extension is not included as the same basename can be used for both csv and plots.
#
# object: "panels", "scenes", "characters", etc.
# vol: id of the considered volume.
# arc: id of the considered narrative arc.
# subfold: additional folder.
# att: vertex attribute concerned.
# val: considered value of the vertex attribute.
# pref: prefix of the file name.
# suf: suffix of the file name.
# 
# returns: full path.
###############################################################################
get.path.stats.corpus <- function(object=NA, vol=NA, arc=NA, subfold=NA, att=NA, val=NA, pref=NA, suf=NA)
{	# base folder
	if(!is.na(vol))
		folder <- file.path(STAT_CORPUS_VOLUMES_FOLDER, "separate", vol)
	else if(!is.na(arc))
		folder <- file.path(STAT_CORPUS_ARCS_FOLDER, "separate", arc)
	else
		folder <- STAT_CORPUS_FOLDER
	# possibly add object
	if(!is.na(object))
		folder <- file.path(folder, object)
	# possibly add extra subfolder
	if(!is.na(subfold))
		folder <- file.path(folder, subfold)
	# possibly add attribute name
	if(!is.na(att))
		folder <- file.path(folder,"attributes",att)
#		folder <- file.path(folder,paste0("attribute=",att))
	# possibly create folder
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	
	# set up file name
	fname <- ""
	# possibly add prefix
	if(!is.na(pref))
		fname <- paste0(fname, pref)
	# possibly add attribute value
	if(!is.na(val))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname,"value=",val)
	}
	# possibly add suffix
	if(!is.na(suf))
	{	if(fname!="") fname <- paste0(fname,"_")
		fname <- paste0(fname,suf)
	}
	# prevent empty filename 
	if(fname=="") 
		fname <- "stats"
	
	res <- file.path(folder, fname)
	return(res)
}
