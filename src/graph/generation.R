###########################################################################
# Models to generate various types of graphs.

# Vincent Labatut
# 09/2021
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("D:/eclipse/workspaces/Networks/NaNet")
#
# source("src/graph/generation.R")
###########################################################################




###########################################################################
# Generates a connected undirected lattice-like graph respecting the specified 
# degree sequence, with maximal transitivity (i.e. clustering coefficient). 
# 
# Note: It is assumed that it is possible to build a connected graph based 
# on the specified degree sequence.
#
# degrees: desired degree sequence.
#
# returns: the produced graph.
###########################################################################
generate.transitive.graph <- function(degrees)
{	DEBUG <- TRUE
	if(DEBUG) tlog(2,"Generating a lattice-like graph:\tn: ",length(degrees),"\tm: ",sum(degrees)/2)
	
	# init edges list
	el <- matrix(vector(), nrow=0, ncol=2)
	
	# separate vertices with k<4
	n.sep <- c()
	tmp <- degrees
	for(k in 1:3)
	{	idx <- which(tmp==k)
		n.sep <- c(n.sep, length(idx))
		tmp <- tmp[-idx]
		if(DEBUG) tlog(4,"Found ",n.sep[k]," vertices of degree ",k,", remaining ",length(tmp)," vertices")
	}
	
	# computing number of ring vertices
	m.avail <- sum(degrees)/2 - n.sep[1] - 2*n.sep[2] - 3*n.sep[3]
	n.ring <- m.avail / 2
	if(DEBUG) tlog(4,"Available links: ",m.avail," ring vertices: ",n.ring)
	
	
	if(<length(tmp)*2)
	
	# create a ring as a base
	g <- make_ring(n=length(tmp), directed=FALSE, circular=TRUE)
	g <- add_vertices(g, n1)
	deg <- sort(tmp, decreasing=TRUE) - 2
	
	# not enough edges to have a ring?
	
	
	# add the leaves to the largest hubs
	if(DEBUG) tlog(4, "Adding leaves to the largest hubs")
	i <- 1
	last <- length(deg)
	while(n1!=0)
	{	nc <- min(deg[i], n1)
		if(DEBUG) tlog(6, "Using vertex ",i,": deg ",deg[i]," -> ",deg[i]-nc," (",n1-nc," left)")
		deg[i] <- deg[i] - nc
		el <- rbind(el, cbind(rep(i,(nc+1)),last:(last+nc)))
		last <- last + nc
		n1 <- n1 - nc
		i <- i + 1
	}
	if(DEBUG) tlog(4,"Defined ",nrow(el)," edges attached to leaves")
	
	# order the remaining vertices
	if(DEBUG) tlog(4, "Ordering remaining vertices")
	idx <- order(deg, decreasing=TRUE)
	ord <- c()
	for(s in 1:length(idx))
	{	if(s %% 2 == 0)
			ord <- c(ord, idx[s])
		else
			ord <- c(idx[s], ord)
	}
	
	# go around the graph adding increasingly remote edges 
	if(DEBUG) tlog(4, "Adding increasingly remote edges")
	dist <- 2
	while(!all(deg==0) && dist<length(deg))
	{	if(DEBUG) tlog(6, "Iteration dist: ",dist,"\t remaining nodes: ",length(which(deg>0)))
		for(i in 1:length(idx))
		{	u <- ord[i]
			if(deg[u]>0)
			{	if(DEBUG) tlog(8, "Processing i: ",i, " (deg=",deg[u],")")
#				readline()
				j <- (i + dist - 1) %% length(idx) + 1
				v <- ord[j]
				if(deg[v]>0)
				{	if(DEBUG) tlog(10, "Connecting to j: ",j, " (deg=",deg[v],")")
					el <- rbind(el, c(u,v))
					deg[u] <- deg[u] - 1 
					deg[v] <- deg[v] - 1 
				}
			}
		}
		dist <- dist + 1
	}
	
	# actually build the edges
	g <- add_edges(g, c(t(el)))
	full <- c(ord,(length(ord)+1):length(degrees))
	if(DEBUG) image(as.matrix.csr(as_adjacency_matrix(g,type="both",names=FALSE,sparse=FALSE)))
	if(DEBUG) image(as.matrix.csr(as_adjacency_matrix(g,type="both",names=FALSE,sparse=FALSE)[1:25,300:350]))
	if(DEBUG) image(as.matrix.csr(as_adjacency_matrix(g,type="both",names=FALSE,sparse=FALSE)[full,full]))
	transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
	
# - virer les k=1, k=2, k=3
# - construire l'anneau avec les k>3, en utilisant k=4 liens
# - décompter le nombre de stubs libres sur l'anneau
# - identifier les combis de k=1, 2, 3 permettant de remplir ces stubs
# 	- un k2 peut directement se connecter à deux stubs consécutifs
#	- un k3 à 3 stubs
#  	- les k1 sont attachés aux hubs
# - pour les stubs restants, ont connecte les noeuds quasi-consécutifs 

#g <- ba.game(100, directed=FALSE)
#is.connected(g)
#degrees <- degree(g)
	

	tt <- table(degrees)
	if(!("1" %in% names(tt)))
		tt["1"] <- 0
	var <- matrix(vector(), nrow=0, ncol=3*(length(tt)-1)+2)
	colnames(var) <- c("m", names(tt), paste("r",names(tt)[2:length(tt)],sep=""), paste("b",names(tt)[2:length(tt)],sep=""))
	cst <- c()
	
	
	
	return(g)
}