# Adds chapter subdivisions to ASOIAF. They correspond to the chapters in the novels,
# and this allows ordering the comic like the books.
# 
# Vincent Labatut
# 03/2023
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/asoiaf/add_chapter.R")
###############################################################################




################################################################################
# Substitute to the generic read.raw.data function, specific to ASOIAF. It does
# the same thing, and in addition it includes the novels' chapters as a subdivision.
#
# data: data extracted from the raw data.
################################################################################
read.raw.data.asoiaf <- function()
{	# apply the generic function
	data <- read.raw.data(char.det="implicit")
	
	# add the chapter table and info
	data <- add.chapters.asoiaf(data)
	
	return(data)
}




################################################################################
# Loads and adds a new subdivision to the existing tables: chapters.
#
# data: list of tables computed from the raw data, or loaded from files (if
#       already extracted before).
#
# returns: data with updated tables, and new tables representing chapters.
################################################################################
add.chapters.asoiaf <- function(data)
{	tlog(2,"Add a new subdivision to the ASOIAF comic tables, corresponding to the novels' chapters")
	char.det <- "implicit"
	
	
	####################################
	# retrieve existing tables
	tlog(4,"Retrieving previously computed corpus stats")
	#data <- read.raw.data(char.det=char.det)
	#data <- read.corpus.data(char.det="implicit")
	inter.df <- data$inter.df
	panel.stats <- data$panel.stats
	panel.chars <- data$panel.chars
	page.stats <- data$page.stats
	page.chars <- data$page.chars
	scene.stats <- data$scene.stats
	scene.chars <- data$scene.chars
	char.stats <- data$char.stats
	volume.stats <- data$volume.stats
	volume.chars <- data$volume.chars
	arc.stats <- data$arc.stats
	arc.chars <- data$arc.chars
	
	# read map file
	map.file <- file.path(DATA_FOLDER,"mapping.csv")
	map <- read.csv(map.file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	
	
	####################################
	# compute the new page ranks
	parts <- sapply(1:nrow(page.stats), function(p) map[map[,COL_VOLUME]==page.stats[p,COL_VOLUME] & map[,COL_PAGE_START]<=page.stats[p,COL_PAGE] & map[,COL_PAGE_END]>=page.stats[p,COL_PAGE], COL_RANK])
	page.stats[,COL_RANK] <- rank(parts*(nrow(page.stats)+1) + page.stats[,COL_PAGE])
	
	# compute the rest of the ranks based on the new page ranks
	panel.stats[,COL_RANK] <- rank(page.stats[panel.stats[,COL_PAGE_ID],COL_RANK]*(nrow(panel.stats)+1) + panel.stats[,COL_PANEL])
	scene.stats[,COL_RANK] <- rank(page.stats[scene.stats[,COL_PAGE_START_ID],COL_RANK]*(nrow(scene.stats)+1) + scene.stats[,COL_SCENE_ID], ties.method="first")
	inter.df[,COL_RANK] <- rank(page.stats[inter.df[,COL_PAGE_START_ID],COL_RANK]*(nrow(inter.df)+1) + 1:nrow(inter.df), ties.method="first")
	
	
	####################################
	# add chapter info to volume table
	chapter.vol.id <- match(map[,COL_VOLUME],volume.stats[,COL_VOLUME])
	volume.stats <- cbind(
		volume.stats,
		StartChapterId=sapply(1:nrow(volume.stats), function(v) min(which(chapter.vol.id==v))),
		EndChapterId=sapply(1:nrow(volume.stats), function(v) max(which(chapter.vol.id==v))),
		Chapters=sapply(1:nrow(volume.stats), function(v) length(which(chapter.vol.id==v)))
	)
	
	# add chapter info to arc table
	chapter.arc <- volume.stats[chapter.vol.id,COL_ARC]
	arc.stats <- cbind(
		arc.stats,
		Chapters=sapply(1:nrow(arc.stats), function(a) length(which(chapter.arc==arc.stats[a,COL_TITLE])))
	)
	
	# add chapter info to page table
	chapter.page.start.id <- sapply(1:nrow(map), function(c) page.stats[page.stats[,COL_VOLUME]==map[c,COL_VOLUME] & page.stats[,COL_PAGE]==map[c,COL_PAGE_START], COL_PAGE_ID])
	chapter.page.end.id <- sapply(1:nrow(map), function(c) page.stats[page.stats[,COL_VOLUME]==map[c,COL_VOLUME] & page.stats[,COL_PAGE]==map[c,COL_PAGE_END], COL_PAGE_ID])
	page.chapter.id <- sapply(1:nrow(page.stats), function(p) which(chapter.page.start.id<=p & chapter.page.end.id>=p))
	page.stats <- cbind(
		page.stats,
		#Chapter=map[page.chapter.id,COL_CHAPTER],		# don't really need that, and moreover it's confusing
		ChapterId=page.chapter.id
	)
	
	# add chapter info to panel table
	chapter.panel.start.id <- page.stats[chapter.page.start.id,COL_PANEL_START_ID]
	chapter.panel.end.id <- page.stats[chapter.page.end.id,COL_PANEL_END_ID]
	panel.chapter.id <- sapply(1:nrow(panel.stats), function(p) which(chapter.page.start.id<=panel.stats[p,COL_PAGE_ID] & chapter.page.end.id>=panel.stats[p,COL_PAGE_ID]))
	panel.stats <- cbind(
		panel.stats,
		#Chapter=map[panel.chapter.id,COL_CHAPTER],
		ChapterId=panel.chapter.id
	)
	
	# add chapter info to scene table
	scene.chapter.id <- sapply(1:nrow(scene.stats), function(s) which(chapter.page.start.id<=scene.stats[s,COL_PAGE_START_ID] & chapter.page.end.id>=scene.stats[s,COL_PAGE_END_ID]))
	scene.stats <- cbind(
		scene.stats,
		#Chapter=map[scene.chapter.id,COL_CHAPTER],
		ChapterId=scene.chapter.id
	)
	chapter.scene.start.id <- sapply(1:nrow(map), function(c) min(which(scene.stats[,COL_CHAPTER_ID]==c)))
	chapter.scene.end.id <- sapply(1:nrow(map), function(c) max(which(scene.stats[,COL_CHAPTER_ID]==c)))
			
	# add chapter info to interaction table
	inter.chapter.id <- sapply(1:nrow(inter.df), function(i) which(chapter.page.start.id<=inter.df[i,COL_PAGE_START_ID] & chapter.page.end.id>=inter.df[i,COL_PAGE_END_ID]))
	inter.df <- cbind(
		inter.df,
		#Chapter=map[inter.chapter.id,COL_CHAPTER],
		ChapterId=inter.chapter.id
	)
	
	# build character list by chapter
	chapter.chars <- sapply(1:nrow(map), function(c) sort(unique(unlist(scene.chars[scene.stats[,COL_CHAPTER_ID]==c]))))
	
	# add chapter info to character table
	tt <- table(unlist(chapter.chars))
	char.stats[match(names(tt),char.stats[,COL_NAME]),COL_CHAPTERS] <- tt
	
	
	####################################
	# create new chapter table
	chapter.stats <- data.frame(
			1:nrow(map), map[,COL_CHAPTER], paste(map[,"PoV"],map[,"Number"]),
			map[,COL_VOLUME],
			map[,COL_PAGE_START], map[,COL_PAGE_END],
			chapter.arc,
			map[,COL_RANK],
			chapter.vol.id,
			chapter.page.start.id, chapter.page.end.id,
			chapter.panel.start.id, chapter.panel.end.id,
			chapter.scene.start.id, chapter.scene.end.id,
			sapply(1:nrow(map), function(c) length(which(panel.chapter.id==c))),
			sapply(1:nrow(map), function(c) length(which(scene.chapter.id==c))),
			sapply(chapter.chars, length),
			chapter.page.end.id-chapter.page.start.id+1,
			stringsAsFactors=FALSE, check.names=FALSE
	)
	colnames(chapter.stats) <- c(
		COL_CHAPTER_ID, COL_CHAPTER, COL_TITLE,
		COL_VOLUME,
		COL_PAGE_START, COL_PAGE_END,
		COL_ARC,
		COL_RANK,
		COL_VOLUME_ID,
		COL_PAGE_START_ID, COL_PAGE_END_ID,
		COL_PANEL_START_ID, COL_PANEL_END_ID,
		COL_SCENE_START_ID, COL_SCENE_END_ID,
		COL_PANELS,
		COL_SCENES,
		COL_CHARS,
		COL_PAGES
	)
	
	
	####################################
	# update data structure
	data$inter.df <- inter.df
	data$char.stats <- char.stats
	data$scene.stats <- scene.stats
	data$scene.chars <- scene.chars
	data$panel.stats <- panel.stats
	data$page.stats <- page.stats
	data$volume.stats <- volume.stats
	data$scene.stats <- scene.stats
	data$arc.stats <- arc.stats
	#
	data$chapter.stats <- chapter.stats
	data$chapter.chars <- chapter.chars
	
	# record the updated and new tables
	write.corpus.data(
		char.det=char.det, 
		inter.df=inter.df, 
		panel.stats=panel.stats, panel.chars=panel.chars, 
		page.stats=page.stats, page.chars=page.chars, 
		scene.stats=scene.stats, scene.chars=scene.chars, 
		char.stats=char.stats, 
		chapter.stats=chapter.stats, chapter.chars=chapter.chars, 
		volume.stats=volume.stats, volume.chars=volume.chars, 
		arc.stats=arc.stats, arc.chars=arc.chars
	)
	
	return(data)
}
