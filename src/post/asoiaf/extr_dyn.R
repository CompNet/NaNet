# Extracts dynamic ASOIAF networks. The "publication" order follows the comic publication
# order. The "story" order matches parts of issues (which we called chapters) to the 
# chapters of the novels, and reorder them accordingly, in order to match the book story.
# 
# Vincent Labatut
# 02/2023
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/asoiaf/extr_dyn.R")
###############################################################################




################################################################################
# Extracts various dynamic networks used when studying the ASOIAF comic books.
#
# data: previously computed or loaded stats.
#
# returns: nothing, the dynamic networks are directly recorded as sequences of graphs.
################################################################################
extract.dyn.nets.asoiaf <- function(data)
{	tlog(2,"Extracting dynamic networks")
	char.det <- "implicit"
	narr.unit <- "scene"
	
	# read raw data
	inter.df <- data$inter.df
	char.stats <- data$char.stats
	scene.stats <- data$scene.stats
	scene.chars <- data$scene.chars
	panel.stats <- data$panel.stats
	page.stats <- data$page.stats
	volume.stats <- data$volume.stats
	scene.stats <- data$scene.stats
	arc.stats <- data$arc.stats
	
	# extract dynamic networks
	for(narr.unit in c("scene","chapter"))
	{	tlog(3,"Dealing with narrative unit \"",narr.unit,"\"")
		
		for(pub.order in c("publication","story"))
		{	tlog(4,"Dealing with ",pub.order," order")
			
			for(filtered in c(FALSE,TRUE))
			{	tlog(5,"Dealing with ",if(filtered) "" else "un","filtered networks")
				
				# extract dynamic graph
				gg <- inst.graph.extraction(
					inter.df=inter.df,
					char.stats=char.stats, 
					scene.chars=scene.chars, scene.stats=scene.stats, 
					volume.stats=volume.stats, 
					filtered=filtered, 
					pub.order=pub.order=="publication",
					narr.unit=narr.unit
				)
				# record files
				inst.write.graph(gs=gg, filtered=filtered, pub.order=pub.order=="publication", char.det=char.det)
				#gg <- inst.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=pub.order=="publication", char.det=char.det, narr.unit=narr.unit)
				
				# extract cumulative dynamic graph
				gg <- cum.graph.extraction(
						inter.df=inter.df,
						char.stats=char.stats, 
						scene.chars=scene.chars, scene.stats=scene.stats, 
						volume.stats=volume.stats, 
						filtered=filtered, 
						pub.order=pub.order=="publication",
						narr.unit=narr.unit
				)
				# record files
				cum.write.graph(gs=gg, filtered=filtered, pub.order=pub.order=="publication", char.det=char.det)
				#gg <- cum.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=pub.order=="publication", char.det=char.det, narr.unit=narr.unit)
			}
		}
	}
}




################################################################################
################################################################################
