# Extracts the narrative as two networks, in order to produce two distinct
# graphical reprensetations and highlight changes in the global structure
# of the interpersonal relationships.
#
# Vincent Labatut
# 01/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/partial_extr.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="PartialExtr")




###############################################################################
# represents the narrative as two distinct networks 
tlog(0,"Extract two distinct networks for the narrative")

# plot parameters
pal <- get.palette(2)

# load full graph to get filtered characters
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
filt.names <- V(g)$name[V(g)$Filtered]

# load raw data
tlog(0,"Extract the sequence of scene-related cumulative graphs")
data <- read.raw.data()
# compute the sequence of scene-based graphs (possibly one for each scene)
gs.unf <- extract.static.graph.scenes(volume.info=data$volume.info, char.info=data$char.info, page.info=data$page.info, inter.df=data$inter.df, stats.scenes=data$stats.scenes, ret.seq=TRUE)
gs.filt <- future_lapply(gs.unf, function(g) delete_vertices(g, v=intersect(filt.names,V(g)$name)))
# build the list for latter use
gsl <- list()
gsl[[1]] <- gs.unf
gsl[[2]] <- gs.filt




################################################################################
# integrate the networks over the narrative
gs <- gsl[[1]]

split.vol <- "23"	# The Cage
int.gs <- list()
int.g <- gs[[1]]
tlog(0,"Integrate graphs, processing each graph of the sequence")
for(i in 2:length(gs))
{	tlog(2,"Processing graph ",i,"/",length(gs))
	g <- gs[[i]]
	s <- g$Scene
	
	# check if we reached the new part
	if(data$stats.scenes[s,COL_STATS_VOLUME]==split.vol)
	{	tlog(4,"Splitting here (scene #",s,"/",nrow(data$stats.scenes),")")
		int.gs[[1]] <- simplify(graph=int.g)
		int.g <- g
	}
	else
		int.g <- union(int.g, g, byname=TRUE)
}
int.gs[[2]] <- simplify(graph=int.g)

# TODO
# - att pref : réviser encore les valeurs filtrées
#   >> voir si la correc change qq ch à kappa, sinon même pas la peine de le mettre en annexe (si pas les exposants)
# - avg dist : modifier le script (vérif liste pas vide) et relancer, vérifier
# - ajouter la vérif partout où on utilise les filtrés
#

# TODO 
# - regarder rochat
# - barplot : utiliser deux variantes de rouge pr montrer que c du non-filtré

###############################################################################
# end logging
end.rec.log()
