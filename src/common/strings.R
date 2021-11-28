#############################################################################################
# Functions used to handle strings.
# 
# 11/2021 Vincent Labatut
#
# source("src/common/strings.R")
#############################################################################################




#############################################################################################
# Corrects encoding problems when diacritics appear in text.
# 
# strings: text to process.
#
# returns: same text, but corrected.
#############################################################################################
fix.encoding <- function(strings)
{	
	strings <- gsub(pattern="Ã€",replacement="À",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã ",replacement="à",x=strings,fixed=TRUE)
#	strings <- gsub(pattern="??",replacement="Á",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¡",replacement="á",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã‚",replacement="Â",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¢",replacement="â",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ãƒ",replacement="Ã",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã£",replacement="ã",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã„",replacement="Ä",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¤",replacement="ä",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã…",replacement="Å",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¥",replacement="å",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ã†",replacement="Æ",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¦",replacement="æ",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ã‡",replacement="Ç",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã§",replacement="ç",x=strings,fixed=TRUE)
	
#	strings <- gsub(pattern="??",replacement="Ð",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã°",replacement="ð",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ãˆ",replacement="È",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¨",replacement="è",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã‰",replacement="É",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã©",replacement="é",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ÃŠ",replacement="Ê",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ãª",replacement="ê",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã‹",replacement="Ë",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã«",replacement="ë",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="ÃŒ",replacement="Ì",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¬",replacement="ì",x=strings,fixed=TRUE)
#	strings <- gsub(pattern="??",replacement="Í",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã­",replacement="í",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ÃŽ",replacement="Î",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã®",replacement="î",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã" ,replacement="Ï",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¯",replacement="ï",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ã‘",replacement="Ñ",x=strings,fixed=TRUE)	
	strings <- gsub(pattern="Ã±",replacement="ñ",x=strings,fixed=TRUE)	
	
	strings <- gsub(pattern="Ã’",replacement="Ò",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã²",replacement="ò",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã“",replacement="Ó",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã³",replacement="ó",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã" ,replacement="Ô",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã´",replacement="ô",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã•",replacement="Õ",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ãµ",replacement="õ",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã–",replacement="Ö",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¶",replacement="ö",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã˜",replacement="Ø",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¸",replacement="ø",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Å’",replacement="Œ",x=strings,fixed=TRUE)	
	strings <- gsub(pattern="Å“",replacement="œ",x=strings,fixed=TRUE)	
	
	strings <- gsub(pattern="ÃŸ",replacement="ß",x=strings,fixed=TRUE)	
	
	strings <- gsub(pattern="Ã™",replacement="Ù",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¹",replacement="ù",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ãš",replacement="Ú",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ãº",replacement="ú",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã›",replacement="Û",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã»",replacement="û",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ãœ",replacement="Ü",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¼",replacement="ü",x=strings,fixed=TRUE)
	
#	strings <- gsub(pattern="??",replacement="Ý",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã½",replacement="ý",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Å¸",replacement="Ÿ",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¿",replacement="ÿ",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ãž",replacement="Þ",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã¾",replacement="þ",x=strings,fixed=TRUE)
	
	return(strings)
}
