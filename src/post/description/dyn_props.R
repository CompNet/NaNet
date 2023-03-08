# Computes and plots the evolution of graph, edge, and vertex topological measures.
# 
# Vincent Labatut
# 03/2023
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/dyn_props.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="DynProps")




###############################################################################
tlog(0, "Evolution of various measures over scenes")

# define parameters
char.det <- "implicit"
narr.unit <- "scene"

# define the characters of interest for vertex and edge measures
vtx.plot <- c("Thorgal Aegirsson","Aaricia Gandalfsdottir","Kriss de Valnor","Jolan Thorgalsson","Louve Thorgalsdottir")

# read stats
data <- read.corpus.data(char.det=char.det)
inter.df <- data$inter.df
char.stats <- data$char.stats
scene.chars <- data$scene.chars
scene.stats <- data$scene.stats
volume.stats <- data$volume.stats




###############################################################################
tlog(2,"Extracting dynamic networks (may take a while)")

# narrative smoothing networks are supposed to be already extracted (char_sim script)

# extract both other types: instant and cumulative
for(filtered in c(FALSE,TRUE))
{	tlog(4,"Dealing with ",if(filtered) "" else "un","filtered networks")
	for(pub.order in c(FALSE,TRUE))
	{	tlog(6,"Dealing with ",if(pub.order) "publication" else "story","-ordered networks")
		
		tlog(8, "Dealing with the instant networks")
		# extract instant dynamic graph
		gg <- inst.graph.extraction(
				inter.df=inter.df,
				char.stats=char.stats, 
				scene.chars=scene.chars, scene.stats=scene.stats, 
				volume.stats=volume.stats, 
				filtered=filtered, 
				pub.order=pub.order,
				narr.unit=narr.unit
		)
		# record files
		inst.write.graph(gs=gg, filtered=filtered, pub.order=pub.order, char.det=char.det)
		
		tlog(8, "Dealing with the cumulative networks")
		# extract cumulative dynamic graph
		gg <- cum.graph.extraction(
				inter.df=inter.df,
				char.stats=char.stats, 
				scene.chars=scene.chars, scene.stats=scene.stats, 
				volume.stats=volume.stats, 
				filtered=filtered, 
				pub.order=pub.order,
				narr.unit=narr.unit
		)
		# record files
		cum.write.graph(gs=gg, filtered=filtered, pub.order=pub.order, char.det=char.det)
	}
}




###############################################################################
# compute and plot the evolution of the topological measures
for(net.type in c("instant","cumulative","narr_smooth"))
{	tlog(2, "Dealing with net.type=",net.type)
	
	for(filtered in c(FALSE,TRUE))
	{	tlog(4, "Dealing with filtered=",filtered)
		
		for(pub.order in c(FALSE, TRUE))
		{	tlog(6, "Dealing with pub.order=",pub.order)
			
			# reading dynamic graph
			if(net.type=="instant")
				gg <- inst.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=pub.order, char.det=char.det, narr.unit=narr.unit)
			else if(net.type=="cumulative")
				gg <- cum.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=pub.order, char.det=char.det, narr.unit=narr.unit)
			else if(net.type=="narr_smooth")
				gg <- ns.read.graph(filtered=filtered, remove.isolates=TRUE, pub.order=pub.order, char.det=char.det, narr.unit=narr.unit)
			
			# for the whole graph
			evol.prop.graph(gg=gg, unit.stats=scene.stats, volume.stats=volume.stats, net.type=net.type, filtered=filtered, pub.order=pub.order)
			
			# for a selection of characters
			evol.prop.vertices(gg=gg, vtx.plot=vtx.plot, char.stats=char.stats, unit.stats=scene.stats, volume.stats=volume.stats, net.type=net.type, filtered=filtered, pub.order=pub.order)
			
			# for a selection of character edges
			evol.prop.edges(gg=gg, vtx.plot=vtx.plot, char.stats=char.stats, unit.stats=scene.stats, volume.stats=volume.stats, net.type=net.type, filtered=filtered, pub.order=pub.order)
		}
	}
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
