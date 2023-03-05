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
# narr.units: vector of narrative units to use during the extraction of the dynamic
#             networks (scene, chapter, volume, arc).
#
# returns: nothing, the dynamic networks are directly recorded as sequences of graphs.
################################################################################
extract.dyn.nets.asoiaf <- function(data, narr.units)
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
	for(narr.unit in narr.units)
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
# Computes the properties of the dynamic networks.
#
# data: list of tables, previously computed.
# narr.units: vector of narrative units used to extract the dynamic networks.
################################################################################
plot.dyn.props.asoiaf <- function(data, narr.units)
{	# selected characters
	vtx.plot <- c("Tyrion Lannister", "Arya Stark", "Catelyn Stark", "Jon Snow", "Daenerys Targaryen", "Eddard Stark")
	char.det <- "implicit"
	
	# retrieve stats
	inter.df <- data$inter.df
	char.stats <- data$char.stats
	scene.chars <- data$scene.chars
	scene.stats <- data$scene.stats
	volume.stats <- data$volume.stats
	
	for(narr.unit in narr.units)
	{	tlog(2, "Dealing with narr.unit=",narr.unit)
		
		for(net.type in c("instant","cumulative"))
		{	tlog(4, "Dealing with net.type=",net.type)
			
			for(filtered in c(FALSE,TRUE))
			{	tlog(6, "Dealing with filtered=",filtered)
				
				for(pub.order in c(FALSE, TRUE))
				{	tlog(8, "Dealing with pub.order=",pub.order)
					
					# reading dynamic graph
					if(net.type=="instant")
						gg <- inst.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=pub.order, char.det=char.det, narr.unit=narr.unit)
					else if(net.type=="cumulative")
						gg <- cum.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=pub.order, char.det=char.det, narr.unit=narr.unit)
					else if(net.type=="narr_smooth")
						gg <- ns.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=pub.order, char.det=char.det, narr.unit=narr.unit)
					
					# for the whole graph
					evol.prop.graph(gg=gg, volume.stats=volume.stats, net.type=net.type, filtered=filtered, pub.order=pub.order, plot.vols=pub.order)
					
					# for a selection of characters
					evol.prop.vertices(gg=gg, vtx.plot=vtx.plot, char.stats=char.stats, volume.stats=volume.stats, net.type=net.type, filtered=filtered, pub.order=pub.order, plot.vols=pub.order)
					
					# for a selection of character edges
					evol.prop.edges(gg=gg, vtx.plot=vtx.plot, char.stats=char.stats, volume.stats=volume.stats, net.type=net.type, filtered=filtered, pub.order=pub.order, plot.vols=pub.order)
				}
			}
		}
	}
}
