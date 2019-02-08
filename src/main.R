# TODO: Add comment
# 
# Vincent Labatut
# 11/2018
###############################################################################
library("igraph")


###############################################################################
# init folder path
#DATA_FOLDER <- file.path("data","Test")
DATA_FOLDER <- file.path("data","Ralph_Azham")


###############################################################################
# load auxiliary functions
source("src/file_constants.R")
source("src/table_constants.R")
source("src/read_raw.R")
source("src/extract_graphs.R")
source("src/logging.R")


###############################################################################
# read raw data
data <- read.raw.data()


###############################################################################
# extract all networks
#extract.graphs(data)


###############################################################################
####### Stats part
# Goal: study the effect of the extraction parameters
#	- window size
#	- overlap
#	- extraction method
# Methods:
#	- standard topological measures:
#		- how they evolve with the parameters
#		- for nodal measure, we could study how they are rank-correlated (for 2 different parameter values)
#		  (in addition to studyig changes in absolute value)
#	- also consider global similarity measures between graphs


# TODO
# - check whether non-contiguous page numbering affects graph extraction functions
# - test everything
# - characters:
#	- script to compare the file with the interactions, show the missing/superfluous characters
#	- complete the file accordingly (with the missing characters)
# - identify which measures are used in apps of char nets
#   >> study how these are affected by extraction parameters

