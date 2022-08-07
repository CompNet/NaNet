#############################################################################################
# Functions used to handle various statistical tasks.
# 
# 05/2021 Vincent Labatut
#############################################################################################
library("ercv")

source("src/common/stats/distr_test.R")




#############################################################################################
# Computes the statistical mode for the specified sample.
# taken from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
#
# x: sample.
# na.rm: what to do with NA values.
#
# returns: the statistical mode(s).
#############################################################################################
stat.mode <- function(x, na.rm=FALSE)
{	if(na.rm)
		x = x[!is.na(x)]
	
	ux <- unique(x)
	tt <- tabulate(match(x, ux))
	idx <- which.max(tt)
	
	res <- ux[idx]
	return(res)
}
