###########################################################################
# Models to generate various types of graphs.
# 
# Vincent Labatut
# 09/2021
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("D:/eclipse/workspaces/Networks/NaNet")
#
# source("src/graphs/generation.R")
###########################################################################




###########################################################################
# Generates a tree with the specified degree sequence. This is adapted from
# the Networkx function:
#		https://networkx.org/documentation/stable/_modules/networkx/generators/degree_seq.html#degree_sequence_tree
# See also the algorithm described here by Brian M. Scott:
# 		https://math.stackexchange.com/a/1948759/122368
#
# deg.seq: desired degree sequence.
#
# returns: the generated graph, or NA if the degree sequence does not allow
#		   generating a graph.
###########################################################################
generate.tree <- function(deg.seq)
{	#deg.seq <- c(4,3,2,1,1,1,1,1)
	#deg.seq <- c(4,3,3,2,1,1,1,1,1,1)
	deg.sum = sum(deg.seq)
	n <- length(deg.seq)
	m <- deg.sum / 2
	
	# check that the degree sequence is fine
	if(deg.sum %% 2 != 0)
		tlog(2,"WARNING: sum of degrees must be even")
	else if(m != (n-1))
		tlog(2,"WARNING: sum of degrees should be ",2*(n-1),", not",2*m)
	
	# init graph
	g <- make_empty_graph(n, directed=FALSE)
	# remove leaves and sort remaining degrees
	deg.sort <- sort(deg.seq[deg.seq>1], decreasing=TRUE)
	
	# make path graph as backbone
	path.len <- length(deg.sort) + 2
	el <- cbind(1:(path.len-1), 2:path.len)
	g <- add_edges(graph=g, edges=c(t(el)))
	
	# add the leaves
	lst <- path.len
	for(u in 2:(path.len-1))
	{	n.edges <- deg.sort[length(deg.sort)] - 2
		deg.sort <- deg.sort[-length(deg.sort)]
		if(n.edges>0)
		{	el <- cbind(rep(u,n.edges), (lst+1):(lst+n.edges))
			g <- add_edges(graph=g, edges=c(t(el)))
			lst <- lst + n.edges
		}
	}
plot(g)
	# in case we added one too many
	if(gorder(g) > n)
		g <- delete_vertices(g, 1)
}




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
	
	# init the linear equation matrices
	tt <- table(degrees)
	if(!("1" %in% names(tt)))
		tt["1"] <- 0
	var <- matrix(0, nrow=1, ncol=2*(length(tt)-1))
	colnames(var) <- c(paste("r",names(tt)[2:length(tt)],sep=""), paste("b",names(tt)[2:length(tt)],sep=""))
	cst <- ecount(g)
	for(k in names(tt))
	{	if(k!="1")
		{	var <- rbind(var, rep(0, ncol(var)))
			var[nrow(var), paste0("r",k)] <- 1
			var[nrow(var), paste0("b",k)] <- 1
			cst <- c(cst, tt[k])
			var[1, paste0("b",k)] <- as.integer(k)-1
			if(k!="2" && k!="3")
				var[1, paste0("r",k)] <- as.integer(k)/2
		}
	}
	#print(cbind(var, cst))
	
	
	
	# integer programming
degrees <- c(7, 5, 5, 5, 5, 5, 5, 4, 4, 3, 1, 1)
	library("lpSolve")
	tt <- table(degrees)
	if("1" %in% names(tt))
	{	n1 <- tt["1"]
		tt <- tt[names(tt)!="1"]
	}else
	{	n1 <- 0}
	f.con <- matrix(0, nrow=1, ncol=2*length(tt))
	colnames(f.con) <- c(paste("r",names(tt),sep=""), paste("b",names(tt),sep=""))
	f.rhs <- sum(degrees)/2
	for(k in names(tt))
	{	f.con <- rbind(f.con, rep(0, ncol(f.con)))
		f.con[nrow(f.con), paste0("r",k)] <- 1
		f.con[nrow(f.con), paste0("b",k)] <- 1
		f.rhs <- c(f.rhs, tt[k])
		f.con[1, paste0("b",k)] <- as.integer(k)-1
		if(k!="2" && k!="3")
			f.con[1, paste0("r",k)] <- as.integer(k)/2
	}
	f.con[1,] <- 2*f.con[1,]
	f.rhs[1] <- 2*f.rhs[1]
	f.con <- rbind(f.con, rep(1, ncol(f.con)))
	f.rhs <- c(f.rhs, length(degrees)-n1)
	f.dir <- rep("=",length(f.rhs))
	#print(cbind(f.con, f.dir, f.rhs))
	f.obj <- c(rep(1,length(tt)), rep(0,length(tt))) 
	res <- lp(direction="max", 
			objective.in=f.obj, const.mat=f.con, const.dir=f.dir, const.rhs=f.rhs, 
			int.vec=length(f.rhs), all.bin=FALSE, all.int=TRUE)
	
	# f.con %*% matrix(c(0, 2, 6, 1, 1, 0, 0, 0),ncol=1)
	if(res$status==0)
	{	n.ring <- res$objval
		sol.vals <- res$solution
	}
	else
	{	# no solution
		# TODO
	}
	
	# build the ring
		# 1) keep only r>3
		# 2) build k=4 ring
		# 3) connect the r2 and r3 nodes
		# 4) add edges between r>3 nodes
		# possibly switch 3 & 4 ?
	# remaining stubs = branches
	# should be only one branch on the hug, right? (no)
		# 1) put one minimal tree on every remaining stub but the one on the hub
		# 2) build a tree with the remaining b nodes a connect to hub

	return(g)
}