# Computes the topological properties of a collection of characte networks.
#
# Vincent Labatut
# 08/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/comparison/comp_proc.R")
###############################################################################
library("igraph")
library("rgexf")
library("poweRlaw")

source("src/common/include.R")




###############################################################################
# names of the stats
measnames <- c(
	"Node nbr",
	"Edge nbr",
	"Density",
	"Avg degr",
	"Avg dist",
	"Rand avg dist",
	"Norm avg dist",
	"Avg trans",
	"Rand avg trans",
	"Norm avg trans",
	"Smallworldness",
	"Max degr",
	"Degr distrib",
	"Degr ass",
	"Exp trans-deg rel"
)




###############################################################################
# Computes the standard topological properties of the specified network.
#
# g: network to process.
#
# returns: table containing the computed values.
###############################################################################
charnet.prop <- function(g)
{	# init stat table
	tab <- matrix(NA, nrow=1, ncol=length(measnames))
	colnames(tab) <- measnames
	
	# fill stats table
	tab[1,"Node nbr"] <- gorder(g)
	tab[1,"Edge nbr"] <- gsize(g)
	tab[1,"Density"] <- graph.density(g)
	tab[1,"Avg degr"] <- mean(degree(g))
	tab[1,"Avg dist"] <- mean_distance(graph=g)
	tab[1,"Rand avg dist"] <- log(tab[1,"Node nbr"]) / log(tab[1,"Avg degr"])
	tab[1,"Norm avg dist"] <- tab[1,"Avg dist"] / tab[1,"Rand avg dist"]
	tab[1,"Avg trans"] <- transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
	tab[1,"Rand avg trans"] <- tab[1,"Avg degr"] / tab[1,"Node nbr"]
	tab[1,"Norm avg trans"] <- tab[1,"Avg trans"] / tab[1,"Rand avg trans"]
	tab[1,"Smallworldness"] <- tab[1,"Norm avg trans"] / tab[1,"Norm avg dist"]
	tab[1,"Max degr"] <- max(degree(g))
	tab[1,"Degr distrib"] <- NA
	tab[1,"Degr ass"] <- assortativity_degree(graph=g, directed=FALSE)
	tab[1,"Exp trans-deg rel"] <- NA
	
	return(tab)
}




###############################################################################
# Applies a standard preprocessing to the specified network, in order to 
# make it comparable to the others in the collection.
#
# g: original network
# 
# returns: preprocessed network.
###############################################################################
charnet.clean <- function(g)
{	# multiple components
	if(!is_connected(g, mode="weak"))
	{	old.n <- gorder(g)
		g <- get.largest.component(g, indices=FALSE)
		new.n <- gorder(g)
		tlog(2,"WARNING: The network has several components >> keeping the largest one (",old.n," vs. ",new.n," -- ",sprintf("%.2f", new.n/old.n*100),"% )")
	}
	
	# signed edges
	if("weight" %in% edge_attr_names(g) && any(E(g)$weight<0))
	{	tot.m <- gsize(g)
		neg.m <- length(which(E(g)$weight<0))
		tlog(2,"WARNING: The network contains negative weights (",neg.m,"/",tot.m," -- ",sprintf("%.2f", neg.m/tot.m*100),"%) >> keeping only the absolute values")
		E(g)$weight <- abs(E(g)$weight)
	}
	
	# presence of loops
	if(any(which_loop(g)))
	{	tot.m <- gsize(g)
		loop.m <- length(which(which_loop(g)))
		tlog(2,"WARNING: The network contains loops (",loop.m,"/",tot.m," -- ",sprintf("%.2f", loop.m/tot.m*100),"%) >> removing these edges")
		g <- simplify(g, remove.multiple=FALSE, remove.loops=TRUE)
	}
	
	# presence of multiple edges
	if(any(which_multiple(g)))
	{	tot.m <- gsize(g)
		mult.m <- length(which(which_multiple(g)))
		tlog(2,"WARNING: The network contains multiple links (",mult.m,"/",tot.m," -- ",sprintf("%.2f", mult.m/tot.m*100),"%) >> removing these edges")
		g <- simplify(g, remove.multiple=TRUE, remove.loops=FALSE)
	}
	
	# weighted edges
	if("weight" %in% edge_attr_names(g))
	{	tlog(2,"WARNING: The network is weighted >> removing edge weights")
		delete_edge_attr(g, "weight")
	}
	
	# directed edges
	if(is.directed(g))
	{	tlog(2,"WARNING: The network is directed >> dropping edge directions")
		g <- as.undirected(g, mode="collapse")
	}
	
	return(g)
}



