# Values of certain topological measure, estimated for a null model
# that takes into account the bipartite nature of the graph.
#
# Formulas taken from the following paper:
# 	M. E. J. Newman, S. H. Strogatz, and D. J. Watts, 
# 	“Random graphs with arbitrary degree distributions and their applications,” 
#	Physical Review E, 6402(2):26118, 2001.
# 
# Vincent Labatut
# 11/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/avgdist_evol.R")
###############################################################################
library("polynom")

source("src/common/include.R")




# load raw data
data <- read.raw.data()
char.info <- data$char.info
char.scenes <- data$char.scenes

# init bipartite graph
g <- make_empty_graph(n=nrow(char.info)+length(char.scenes), directed=FALSE)
V(g)$type <- c(rep("Character",nrow(char.info)), rep("Scene",length(char.scenes)))
# add character names and info
for(cn in colnames(char.info))
	g <- set_vertex_attr(graph=g, name=cn, index=1:nrow(char.info), value=char.info[,cn])
# add scene names
g <- set_vertex_attr(graph=g, name="Name", index=nrow(char.info)+(1:length(char.scenes)), value=paste0("Scene_",1:length(char.scenes)))
# build edge list
el <- matrix(nrow=0, ncol=2)
for(s in 1:length(char.scenes))
{	tlog(2,"Processing scene ",s,"/",length(char.scenes))
	idx1 <- match(char.scenes[[s]],V(g)$Name)
	if(any(is.na(idx)))
		stop("ERROR: could not find character ",char.scenes[[s]][which(is.na(idx))])
	idx2 <- rep(which(V(g)$Name==paste0("Scene_",s)), length(idx1))
	new.el <- cbind(idx1, idx2)
	print(new.el)
	el <- rbind(el, new.el)
}
g <- add_edges(graph=g, edges=el)
g <- delete_vertices(graph=g, v=which(degree(g, mode="all")<1))

# compute properties
dd <- degree(g, mode="all")
tt <- table(dd)
cc <- rep(0, max(dd))
cc[as.integer(names(tt))] <- tt
cc <- c(0,cc)
cc <- cc/sum(dd)
F0 <- polynomial(coef=cc)
predict(F0, 1)
