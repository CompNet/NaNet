#############################################################################################
# Defines functions used to log messages.
# 
# 05/2016 Vincent Labatut
#
# source("src/common/logging.R")
#############################################################################################




#############################################################################################
# Start recording the logs in a text file.
#############################################################################################
start.rec.log <- function(text=NA)
{	prefix <- format(Sys.time(),"%Y%m%d_%H%M%S")
	log.file <- file.path(LOG_FOLDER,prefix)
	if(!is.na(text))
		log.file <- paste0(log.file,"_",text)
	log.file <- paste0(log.file,".txt")
	sink(log.file, append=TRUE, split=TRUE)
}



#############################################################################################
# Stops recording the logs in a text file.
#############################################################################################
end.rec.log <- function()
{	sink()
}



#############################################################################################
# Logs the specified message on screen, adding current date and time, and possibly some
# offset (to represent the hierarchy of function calls).
#
# offset: number of "." used to represent the hierarchical level of the message.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog <- function(offset=NA, ...)
{	prefix <- paste0("[",format(Sys.time(),"%a %d %b %Y %X"),"] ")
	if(!is.na(offset))
	{	if(is.numeric(offset))
		{	os <- paste(rep(".",offset), sep="", collapse="")
			prefix <- paste0(prefix, os)
		}
		else
			prefix <- paste0(prefix, offset)
	}
	cat(prefix, ..., "\n", sep="")
}
