# Computes the topological properties of a collection of characte networks.
#
# Vincent Labatut
# 08/2021
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/comparison/comp_proc.R")
###############################################################################
library("igraph")
library("poweRlaw")
library("minpack.lm")

source("src/common/_include.R")




###############################################################################
# names of the stats
C_MEAS <- c()
C_MEAS_NODE_NBR <- "Node nbr"; C_MEAS <- c(C_MEAS, C_MEAS_NODE_NBR)
C_MEAS_EDGE_NBR <- "Edge nbr"; C_MEAS <- c(C_MEAS, C_MEAS_EDGE_NBR)
C_MEAS_DENSITY <- "Density"; C_MEAS <- c(C_MEAS, C_MEAS_DENSITY)
C_MEAS_DEG_AVG <- "Avg degr"; C_MEAS <- c(C_MEAS, C_MEAS_DEG_AVG)
C_MEAS_DIST_AVG <- "Avg dist"; C_MEAS <- c(C_MEAS, C_MEAS_DIST_AVG)
C_MEAS_TRANS_AVG <- "Avg trans"; C_MEAS <- c(C_MEAS, C_MEAS_TRANS_AVG)
C_MEAS_SWI <- "SWI"; C_MEAS <- c(C_MEAS, C_MEAS_SWI)
C_MEAS_DEG_MAX <- "Max Degr"; C_MEAS <- c(C_MEAS, C_MEAS_DEG_MAX)
C_MEAS_DEG_DISTRIB <- "Degr Distrib"; C_MEAS <- c(C_MEAS, C_MEAS_DEG_DISTRIB)
C_MEAS_DEG_ASSORT <- "Degr Ass"; C_MEAS <- c(C_MEAS, C_MEAS_DEG_ASSORT)
C_MEAS_TRANS_DEG_EXP <- "Exp Trans-Deg rel"; C_MEAS <- c(C_MEAS, C_MEAS_TRANS_DEG_EXP)
C_MEAS_TRANS_DEG_GOF <- "GoF Trans-Deg rel"; C_MEAS <- c(C_MEAS, C_MEAS_TRANS_DEG_GOF)




###############################################################################
# Initializes the tables used to store the statistics.
# 
# files: list of network names.
#
# returns: a list of empty tables.
###############################################################################
charnet.init.tables <- function(files)
{	# general properties
	prop.tab <- data.frame(
			matrix(NA, length(files), length(C_MEAS), dimnames=list(files, C_MEAS)), 
			stringsAsFactors=FALSE)
	
	# small word table
	sw.tab <- data.frame(
			matrix(NA, length(files), length(C_SW), dimnames=list(files, C_SW)), 
			stringsAsFactors=FALSE)
	
	# degree distribution table
	distr.tab <- data.frame(
			matrix(NA, length(files), length(C_DISTR), dimnames=list(files, C_DISTR)), 
			stringsAsFactors=FALSE)

	tabs <- list(prop.tab=prop.tab, sw.tab=sw.tab, distr.tab=distr.tab)
	return(tabs)
}




###############################################################################
# Computes the standard topological properties of the specified network.
#
# g: network to process.
# tabs: list of tables to complete.
#
# returns: tables containing the computed values.
###############################################################################
charnet.prop <- function(g, tabs)
{	prop.tab <- tabs$prop.tab
	sw.tab <- tabs$sw.tab
	distr.tab <- tabs$distr.tab
	#
	file <- g$name
	deg <- degree(g, mode="all")
	
	# general measures
	prop.tab[file,C_MEAS_NODE_NBR] <- gorder(g)
	prop.tab[file,C_MEAS_EDGE_NBR] <- gsize(g)
	prop.tab[file,C_MEAS_DENSITY] <- graph.density(g)
	prop.tab[file,C_MEAS_DEG_AVG] <- mean(deg)
	prop.tab[file,C_MEAS_DIST_AVG] <- mean_distance(graph=g)
	prop.tab[file,C_MEAS_TRANS_AVG] <- transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
	prop.tab[file,C_MEAS_DEG_MAX] <- max(deg)
	prop.tab[file,C_MEAS_DEG_ASSORT] <- assortativity_degree(graph=g, directed=FALSE)
	
	# relation between degree and transitivity
	tmp <- transitivity.vs.degree(g, weight=FALSE, filename=file.path(folder, g$name))
	prop.tab[file,C_MEAS_TRANS_DEG_EXP] <- tmp$exponent
	prop.tab[file,C_MEAS_TRANS_DEG_GOF] <- tmp$gof
	
	# small world index
	tmp <- compute.smallworldness(g, theoretical=FALSE, iterations=10)
	sw.tab[file,C_SW] <- tmp[C_SW]
	prop.tab[file,C_MEAS_SWI] <- tmp[C_SWI]
	
	# degree distribution
	tmp <- test.disc.distr(data=deg, return_stats=TRUE, sims=100, plot.file=paste0(base.filename,"_deg_distrib"))
	distr.tab[file,C_DISTR] <- tmp[1,C_DISTR]
	prop.tab[file,C_MEAS_DEG_DISTRIB] <- tmp[1,C_DECISION]
	
	# record tables
	tab.file <- file.path(folder,"_measures.csv")
	write.table(prop.tab, file=tab.file, sep=",", row.names=TRUE, col.names=TRUE)
	tab.file <- file.path(folder,"_small_world.csv")
	write.table(sw.tab, file=tab.file, sep=",", row.names=TRUE, col.names=TRUE)
	tab.file <- file.path(folder,"_degree_dist.csv")
	write.table(distr.tab, file=tab.file, sep=",", row.names=TRUE, col.names=TRUE)
	
	tabs <- list(prop.tab=prop.tab, sw.tab=sw.tab, distr.tab=distr.tab)
	return(tabs)
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
