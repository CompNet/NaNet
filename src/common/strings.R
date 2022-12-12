#############################################################################################
# Functions used to handle strings.
# 
# 11/2021 Vincent Labatut
#
# source("src/common/strings.R")
#############################################################################################




#############################################################################################
# Corrects encoding problems when diacritics appear in some text.
# 
# strings: text to process.
#
# returns: same text, but corrected.
#############################################################################################
fix.encoding <- function(strings)
{	
	strings <- gsub(pattern="Ã€",replacement="À",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã ",replacement="à",x=strings,fixed=TRUE)
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




#############################################################################################
# Removes diacritics.
# 
# strings: text to process.
#
# returns: same text, but without the diacritics.
#############################################################################################
remove.diacritics <- function(strings)
{	
	strings <- gsub(pattern="À",replacement="A",x=strings,fixed=TRUE)
	strings <- gsub(pattern="à",replacement="a",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Á",replacement="A",x=strings,fixed=TRUE)
	strings <- gsub(pattern="á",replacement="a",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Â",replacement="A",x=strings,fixed=TRUE)
	strings <- gsub(pattern="â",replacement="a",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ã",replacement="A",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ã",replacement="a",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ä",replacement="A",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ä",replacement="a",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Å",replacement="A",x=strings,fixed=TRUE)
	strings <- gsub(pattern="å",replacement="a",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Æ",replacement="Ae",x=strings,fixed=TRUE)
	strings <- gsub(pattern="æ",replacement="ae",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ç",replacement="C",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ç",replacement="c",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ð",replacement="D",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ð",replacement="d",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="È",replacement="E",x=strings,fixed=TRUE)
	strings <- gsub(pattern="è",replacement="e",x=strings,fixed=TRUE)
	strings <- gsub(pattern="É",replacement="E",x=strings,fixed=TRUE)
	strings <- gsub(pattern="é",replacement="e",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ê",replacement="E",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ê",replacement="e",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ë",replacement="E",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ë",replacement="e",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ì",replacement="I",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ì",replacement="i",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Í",replacement="I",x=strings,fixed=TRUE)
	strings <- gsub(pattern="í",replacement="i",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Î",replacement="I",x=strings,fixed=TRUE)
	strings <- gsub(pattern="î",replacement="i",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ï",replacement="I",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ï",replacement="i",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ñ",replacement="N",x=strings,fixed=TRUE)	
	strings <- gsub(pattern="ñ",replacement="n",x=strings,fixed=TRUE)	
	
	strings <- gsub(pattern="Ò",replacement="O",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ò",replacement="o",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ó",replacement="O",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ó",replacement="o",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ô",replacement="O",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ô",replacement="o",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Õ",replacement="O",x=strings,fixed=TRUE)
	strings <- gsub(pattern="õ",replacement="o",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ö",replacement="O",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ö",replacement="o",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ø",replacement="O",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ø",replacement="o",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Œ",replacement="Oe",x=strings,fixed=TRUE)	
	strings <- gsub(pattern="œ",replacement="oe",x=strings,fixed=TRUE)	
	
	strings <- gsub(pattern="ß",replacement="ss",x=strings,fixed=TRUE)	
	
	strings <- gsub(pattern="Ù",replacement="U",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ù",replacement="u",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ú",replacement="U",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ú",replacement="u",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Û",replacement="U",x=strings,fixed=TRUE)
	strings <- gsub(pattern="û",replacement="u",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ü",replacement="U",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ü",replacement="u",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Ý",replacement="Y",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ý",replacement="y",x=strings,fixed=TRUE)
	strings <- gsub(pattern="Ÿ",replacement="Y",x=strings,fixed=TRUE)
	strings <- gsub(pattern="ÿ",replacement="y",x=strings,fixed=TRUE)
	
	strings <- gsub(pattern="Þ",replacement="Th",x=strings,fixed=TRUE)
	strings <- gsub(pattern="þ",replacement="th",x=strings,fixed=TRUE)
	
	return(strings)
}
