library(tesseract)
library(magick)

eng <- tesseract("eng")

img.folders <- list(
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/1/01"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/1/02"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/1/03"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/1/04"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/1/05"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/1/06"=3:31,
	#
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/2/07"=3:30,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/2/08"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/2/09"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/2/10"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/2/11"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/2/12"=3:31,
	#
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/3/13"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/3/14"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/3/15"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/3/16"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/3/17"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/3/18"=3:31,
	#
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/4/19"=3:30,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/4/20"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/4/21"=3:30,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/4/22"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/4/23"=3:31,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/1. A Game of Thrones/4/24"=3:31,
	#
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/1/01"=6:26,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/1/02"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/1/03"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/1/04"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/1/05"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/1/06"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/1/07"=4:22,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/1/08"=4:23,
	#
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/2/09"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/2/10"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/2/11"=4:23,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/2/12"=4:23,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/2/13"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/2/14"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/2/15"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/2/16"=4:23,
	#
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/3/17"=5:25,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/3/18"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/3/19"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/3/20"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/3/21"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/3/22"=4:23,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/3/23"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/3/24"=4:24,
	#
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/4/25"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/4/26"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/4/27"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/4/28"=4:21,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/4/29"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/4/30"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/4/31"=4:24,
	"F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO/2. A Clash of Kings/4/32"=4:24
)

for(f in 1:length(img.folders))
{	img.folder <- names(img.folders)[f]
	img.nbrs <- img.folders[[f]]

	texts <- list()
	for(img in img.nbrs)
	{	image.file <- file.path(img.folder, paste0(sprintf("%02d",img),".jpg"))
		cat("Processing file \"",image.file,"\"\n",sep="")
		image <- image_read(image.file)
		#image <- image_convert(image, type="Grayscale")
		
		text <- tesseract::ocr(image, engine=eng)
		# segmenter le texte
		text <- strsplit(text, split="[^A-Za-z]")[[1]]
		text <- text[sapply(text, nchar) > 0]
		# garder uniquement les chaines en majuscules
		text <- text[toupper(text)==text]
		print(text)
		
		texts <- c(texts, list(text))
	}
	
	vol.text <- paste(unlist(texts),collapse=" ")
	row <- c(substring(img.folder,first=58), vol.text)
	if(f==1)
		tab <- row
	else
		tab <- rbind(tab, row)
}


colnames(tab) <- c("Issue","Text")
# record as text file
out.file <- file.path("F:/Bédés/Amériques/_En cours/_Fantasy/Game of Thrones/VO", "textual_content.txt")
write.table(tab, file=out.file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
