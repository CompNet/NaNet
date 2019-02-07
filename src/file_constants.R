# This scripts contains all file- and path-related constants.
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
