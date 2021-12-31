# Computes the comic-based networks from article
#		Y. Rochat and M. Triclot, 
#		“Les réseaux de personnages de science-fiction : échantillons de lectures intermédiaires,” 
#		ReS Futurae 10, 2017.
#		DOI: 10.4000/resf.1183
#
# Vincent Labatut
# 11/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/comparison/comp_proc_Rochat2017_expl.R")
###############################################################################
source("src/common/distr_test.R")




###############################################################################
# start logging
start.rec.log(text="OtherComics")




################################################################################
# data folder
folder <- file.path(CHARNETS_FOLDER, "Rochat2017") 




################################################################################
# compute various stats for the graphical novel datasets

# Saga of the Meta-barons: 8 volumes
	# notes: according to external sources, each volume is 62-page long
	# however, there are 558 columns in the file (8*62=496, 9*62=558)
	# morever, starting column 511, the matrix is empty
	# assumption: pages 496--510 correspond to the short story "Le tatouage des Castaka" in "La Maison des Ancêtres"
	# we ignore it here, but not in the network extraction phase (above source code)
	vol.nbr <- 8
	file <- "1992-2003.Meta-Baron-adj.csv"
	tlog(0,"Processing Metabarons (",file,")")
	tt <- read.csv(file=file.path(folder,file), header=TRUE, row.names=1)
	tlog(2,"Number of characters: ",nrow(tt))
	tlog(2,"Number of pages: ",ncol(tt))
	tlog(2,"Number of volumes: ",vol.nbr)
	tt <- tt[,1:496]	# we ignore the short story
	rm.idx <- which(rowSums(tt)==0)
	tlog(2,"Detected ",length(rm.idx),"/",nrow(tt)," characters not occurring at all: deleting them")
	tt <- tt[-rm.idx,]
	tlog(2,"Number of pages by character: ",sum(tt)/nrow(tt))
	tlog(2,"Number of characters by page: ",sum(tt)/ncol(tt))
	pg <- ncol(tt)/vol.nbr
	vv <- apply(tt, 1, function(row) length(which(sapply(1:vol.nbr, function (v) sum(row[(1+(v-1)*pg):(v*pg)]))>0)))
	tlog(2,"Number of volumes by character: ",mean(vv))
	test.disc.distr(data=vv, 			# truncated
			xlab="Number of volumes by character", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_vols_by_char"))
	cc <- sapply(1:vol.nbr, function(v) length(which(rowSums(tt[,(1+(v-1)*pg):(v*pg)])>0)))
	tlog(2,"Number of characters by volume: ",mean(cc))
	test.disc.distr(data=cc, 			# good
			xlab="Number of characters by volume", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_chars_by_vol"))
	
# Worlds of Aldebaran: 5 volumes
	vol.nbr <- 5
	file <- "1994.Betelgeuse-adj.csv"
	tlog(0,"Processing Betelgeuse (",file,")")
	tt <- read.csv(file=file.path(folder,file), header=TRUE, row.names=1)
	tlog(2,"Number of characters: ",nrow(tt))
	tlog(2,"Number of pages: ",ncol(tt))
	tlog(2,"Number of volumes: ",vol.nbr)
	rm.idx <- which(rowSums(tt)==0)
	tlog(2,"Detected ",length(rm.idx),"/",nrow(tt)," characters not occurring at all: deleting them")
	#tt <- tt[-rm.idx,]
	tlog(2,"Number of pages by character: ",sum(tt)/nrow(tt))
	tlog(2,"Number of characters by page: ",sum(tt)/ncol(tt))
	pg <- ncol(tt)/vol.nbr
	vv <- apply(tt, 1, function(row) length(which(sapply(1:vol.nbr, function (v) sum(row[(1+(v-1)*pg):(v*pg)]))>0)))
	tlog(2,"Number of volumes by character: ",mean(vv))
	test.disc.distr(data=vv, 			# none
			xlab="Number of volumes by character", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_vols_by_char"))
	cc <- sapply(1:vol.nbr, function(v) length(which(rowSums(tt[,(1+(v-1)*pg):(v*pg)])>0)))
	tlog(2,"Number of characters by volume: ",sum(cc)/vol.nbr)
	test.disc.distr(data=cc, 			# good
			xlab="Number of characters by volume", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_chars_by_vol"))
	
# Akira: 3 volumes
	# note: we know that these volumes contain the following (original) chapters:
	# vol.1+2: correspond to vol.1 in the English version, so c1--18 - vol.3: half of vol.2 in the English version, so c19--33
	# but we could not find the number of pages in each chapter
	vols <- c(1, 181, 363, 546)
	vol.nbr <- length(vols) - 1
	chaps <- c(1, 30, 55, 74, 92, 105, 125, 143, 162,			# vol.1FR: c1--9
			181, 206, 223, 241, 262, 280, 299, 318, 335,		# vol.2FR: c10--18
			363, 389, 403, 422, 440, 455, 470, 491, 508, 527, 	# vol.3FR: c19--28
			546													# last page
	)	# we manually identified the chapters using https://mangadex.org/chapter/56355233-2dc4-4f95-83e9-56c9354b7618/2 as a reference
	chap.nbr <- length(chaps) - 1
	file <- "1982.Akira-adj.csv" #,"1982.Akira2-adj.csv","1982.Akira3-adj.csv")
	tlog(0,"Processing Akira (",file,")")
	tt1 <- as.matrix(read.csv(file=file.path(folder,"1982.Akira1-adj.csv"), header=TRUE, row.names=1))
	tt2 <- as.matrix(read.csv(file=file.path(folder,"1982.Akira2-adj.csv"), header=TRUE, row.names=1))
	tt3 <- as.matrix(read.csv(file=file.path(folder,"1982.Akira3-adj.csv"), header=TRUE, row.names=1))
	rn <- sort(union(rownames(tt1), union(rownames(tt2), rownames(tt3))))
	tt <- matrix(0, nrow=length(rn), ncol=ncol(tt1)+ncol(tt2)+ncol(tt3))
	colnames(tt) <- c(colnames(tt1),colnames(tt2),colnames(tt3))
	colnames(tt) <- 1:ncol(tt)
	rownames(tt) <- rn
	tt[match(rownames(tt1),rn),1:ncol(tt1)] <- tt1
	tt[match(rownames(tt2),rn),(ncol(tt1)+1):(ncol(tt1)+ncol(tt2))] <- tt2
	tt[match(rownames(tt3),rn),(ncol(tt1)+ncol(tt2)+1):(ncol(tt1)+ncol(tt2)+ncol(tt3))] <- tt3
	write.csv(x=tt, file=file.path(folder,"1982.Akira-adj_combined.csv"), row.names=TRUE)#, col.names=TRUE)
	tlog(2,"Number of characters: ",nrow(tt))
	tlog(2,"Number of pages: ",ncol(tt))
	tlog(2,"Number of volumes: ",vol.nbr)
	tlog(2,"Number of chapters: ",chap.nbr)
	rm.idx <- which(rowSums(tt)==0)
	tlog(2,"Detected ",length(rm.idx),"/",nrow(tt)," characters not occurring at all: deleting them")
	tt <- tt[-rm.idx,]
	tlog(2,"\tNumber of pages by character: ",sum(tt)/nrow(tt))
	tlog(2,"\tNumber of characters by page: ",sum(tt)/ncol(tt))
	tlog(2,"Number of pages by volume: ",mean(vols[2:length(vols)]-vols[1:(length(vols)-1)]))
	vv <- apply(tt, 1, function(row) length(which(sapply(1:vol.nbr, function (v) sum(row[vols[v]:(vols[v+1]-1)]))>0)))
	tlog(2,"Number of volumes by character: ",mean(vv))
	test.disc.distr(data=vv, 			# none
			xlab="Number of volumes by character", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_vols_by_char"))
	cc <- sapply(1:vol.nbr, function(v) length(which(rowSums(tt[,vols[v]:(vols[v+1]-1)])>0)))
	tlog(2,"Number of characters by volume: ",mean(cc))
	test.disc.distr(data=cc, 			# good
			xlab="Number of characters by volume", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_chars_by_vol"))
	tlog(2,"Number of pages by chapter: ",mean(chaps[2:chap.nbr]-chaps[1:(chap.nbr-1)]))
	vv <- apply(tt, 1, function(row) length(which(sapply(1:(chap.nbr-1), function (c) sum(row[chaps[c]:(chaps[c+1]-1)]))>0)))
	tlog(2,"Number of chapters by character: ",mean(vv))
	test.disc.distr(data=vv, 			# good
			xlab="Number of chapters by character", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_chaps_by_char"))
	cc <- sapply(1:(chap.nbr-1), function(c) length(which(rowSums(tt[,chaps[c]:(chaps[c+1]-1)])>0)))
	tlog(2,"Number of characters by chapter: ",mean(cc))
	test.disc.distr(data=cc, 			# good
			xlab="Number of characters by chapter", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_chars_by_chap"))
	
# Gunnm: 7 volumes
	# note: the number of pages in the CSV file (1023) does not match the expected number according to external sources (1475)
	# considering there are only 5 volumes would be a better fit (1037 pages). 
	# these 7 volumes are supposed to contain the following chapters:
	# vol.1: c1--6 - vol.2: 7--11 - vol.3: 12--17 - vol.4: 18--22 - vol.5: 23--29 - vol.6: 30--35 - vol.7: 36--41
	# manual verification shows that some chapters are missing, and that there are only 6 volumes (and the 6th is not complete)
	vols <- c(1, 198, 393, 568, 749, 956, 1024)
	vol.nbr <- length(vols) - 1
	chaps <- c(1, 26, 50, 78, 130, 177, 		# vol.1: c1--6
			198, 223, 248, 309, 373, 			# vol.2: c7--11
			393, 413, 447, 469, 510, 549, 		# vol.3: c12--17
			568, 635, 656, 685, 705, 			# vol.4: c18--22
			749, 766, 792, 835, 876, 890, 917, 	# vol.5: c23--29
			956, 961, 1002, 					# vol.6: c30--32 (incomplete: should be c30--35)
			1024								# last page
	)	# we manually identified the chapters using a digital version of the manga. There are fewer chapters than indicated in the article (only 32 chapters instead of 41, volume 7 missing)
	chap.nbr <- length(chaps) - 1
	file <- "1990.Gunnm-adj.csv"
	tlog(0,"Processing Gunnm (",file,")")
	tt <- read.csv(file=file.path(folder,file), header=TRUE, row.names=1)
	tlog(2,"Number of characters: ",nrow(tt))
	tlog(2,"Number of pages: ",ncol(tt))
	tlog(2,"Number of volumes: ",vol.nbr)
	tlog(2,"Number of chapters: ",chap.nbr)
	rm.idx <- which(rowSums(tt)==0)
	tlog(2,"Detected ",length(rm.idx),"/",nrow(tt)," characters not occurring at all: deleting them")
	tt <- tt[-rm.idx,]
	tlog(2,"Number of pages by character: ",sum(tt)/nrow(tt))
	tlog(2,"Number of characters by page: ",sum(tt)/ncol(tt))
	tlog(2,"Number of pages by volume: ",mean(vols[2:length(vols)]-vols[1:(length(vols)-1)]))
	vv <- apply(tt, 1, function(row) length(which(sapply(1:vol.nbr, function (v) sum(row[vols[v]:(vols[v+1]-1)]))>0)))
	tlog(2,"Number of volumes by character: ",mean(vv))
	test.disc.distr(data=vv, 			# good
			xlab="Number of volumes by character", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_vols_by_char"))
	cc <- sapply(1:vol.nbr, function(v) length(which(rowSums(tt[,vols[v]:(vols[v+1]-1)])>0)))
	tlog(2,"Number of characters by volume: ",mean(cc))
	test.disc.distr(data=cc, 			# good
			xlab="Number of characters by volume", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_chars_by_vol"))
	tlog(2,"Number of pages by chapter: ",mean(chaps[2:chap.nbr]-chaps[1:(chap.nbr-1)]))
	vv <- apply(tt, 1, function(row) length(which(sapply(1:(chap.nbr-1), function (c) sum(row[chaps[c]:(chaps[c+1]-1)]))>0)))
	tlog(2,"Number of chapters by character: ",mean(vv))
	test.disc.distr(data=vv, 			# good
			xlab="Number of chapters by character", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_chaps_by_char"))
	cc <- sapply(1:(chap.nbr-1), function(c) length(which(rowSums(tt[,chaps[c]:(chaps[c+1]-1)])>0)))
	tlog(2,"Number of characters by chapter: ",mean(cc))
	test.disc.distr(data=cc, 			# good
			xlab="Number of characters by chapter", return_stats=TRUE, 
			plot.file=paste0(file.path(folder,fs::path_ext_remove(file)),"_stats_chars_by_chap"))
	



###############################################################################
# stop logging
end.rec.log()
