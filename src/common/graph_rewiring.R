###########################################################################
# Rewires an existing network in a more or less random way. One function 
# is completely random, the other is a latticization.

# Vincent Labatut
# 06/2014
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("D:/eclipse/workspaces/Networks/NaNet")
#
# source("src/common/graph_rewiring.R")
###########################################################################




###########################################################################
# Returns the adjacency matrix when considering only the specified nodes
# vs. the whole network. The result is a k*n matrix, where n is the total 
# number of nodes in the network and k the number of nodes of interest.
#
# g: network to process.
# nodes: nodes of interest.
#
# returns: the partial adjacency matrix.
###########################################################################
get.partial.matrix <- function(g, nodes)
{	result <- matrix(data=FALSE,ncol=vcount(g),nrow=length(nodes))
	#print(nodes)
	if(length(nodes)>0)
	{	for(i in 1:length(nodes))
		{	neigh <- neighbors(graph=g,v=nodes[i],mode="all")
			if(length(neigh)>0)
				result[i,neigh] <- TRUE
		}
	}
	return(result)
}




###########################################################################
# Checks if the specified rewiring is going to split the network, i.e.
# lead to the apparition of a new component. Before the rewiring, the existing 
# links are (a,b) and (c,d). After the rewiring, they are (a,d) and (b,c).
#
# (estimated) Complexity: O(m+n)
#
# g: network to process.
# comp.nb: number of components in the network.
# a,b,c,d: nodes concerned by the rewiring.
#
# returns: TRUE iff the rewiring splits the graph.
###########################################################################
is.splitting.network <- function(g, comp.nb, a, b, c, d)
{	result <- FALSE
	
	if(!are.connected(g, a, c) & !are.connected(g, b, d))
	{	# original method
#		# we take both adjacency matrix rows for a and d
#		p0 <- get.partial.matrix(g,c(a,d))
#		# remove their respective links to b and c
#		p0[1,b] <- FALSE
#		p0[2,c] <- FALSE
#		# copy as p1, add the new respective links to d and a
#		p1 <- p0
#		p1[,d] <- TRUE
#		p1[,a] <- TRUE
#		
#		while(!result & !any(p0[,c(b,c)]))
#		{	# get the neighborhood of the reachable nodes in p0 
#			p0[1,] <- apply(get.partial.matrix(g,which(p0[1,])),2,any)
#			p0[2,] <- apply(get.partial.matrix(g,which(p0[2,])),2,any)
#			p0 <- p0 & (!p1)
#			if(!all(apply(p0,1,any)))
#				result <- TRUE
#			p1 <- p1 | p0
#		}
		
		# igraph-based method (faster)
		# modify and check the graph
		g <- delete.edges(graph=g, edges=c(E(g)[a %--% b],E(g)[c %--% d]))
		g <- add.edges(graph=g, edges=c(a,d,b,c))
		# single component
		if(comp.nb==1)
			result <- !is.connected(graph=g)
		# disconnected network
		else
			result <- no.clusters(graph=g)!=comp.nb # complexity: O(m+n)
	}
	
	return(result)
}




###########################################################################
# Randomly rewires the network, while preserving the degree distribution.
# Adapted from function "randmio_und_connected" from BCT:
# https://sites.google.com/site/bctnet
#
# (estimated) Complexity: O(m+n) (* iterations * attempts)
#
# g: network to be rewired.
# iterations: number of times an edge is rewired (approximately).
#
# returns: the rewired network.
###########################################################################
randomize.network <- function(g, iterations)
{	# init
	n <- gorder(g)
	m <- gsize(g)
	iter <- m*iterations
	comp.nb <- no.clusters(graph=g)
	# maximal number of rewiring attempts per iteration
	max.attempts <- max(5,round(n*m/(n*(n-1.0))))
	# actual number of successful rewirings
	eff <- 0
	
	# repeat process
	for(it in 1:iter)
	{	att <- 0
		rewire <- FALSE
		
		# while not rewired
		while(!rewire & att<=max.attempts)
		{	rewire <- FALSE
			
			# randomly draw 2 links
			es <- igraph.sample(1,m,2)
			temp <- get.edges(graph=g,es=es)
			a <- temp[1,1]
			b <- temp[1,2]
			c <- temp[2,1]
			d <- temp[2,2]
			
			# check if they involve different nodes
			if(length(intersect(c(a,b),c(c,d)))==0)
			{	# possibly flip the first link
				p <- runif(1)
				if(p>0.5)
				{	a <- temp[1,2]
					b <- temp[1,1]
				}
				
				# check if some of the 2 new links already exist
				if(!are.connected(g,a,d) & !are.connected(g,c,b))
				{	# check if the rewiring is going to split the network
					rewire <- !is.splitting.network(g,comp.nb,a,b,c,d)
					if(rewire)
					{	g <- delete.edges(graph=g, edges=es)
						g <- add.edges(graph=g, edges=c(a,d,b,c))
						eff <- eff + 1
					}
				}
			}
			
			# increment the number of attempts
			att <- att + 1
		}
	}
	
	return(g)
}




###########################################################################
# Randomly rewires the specified network, so that it becomes more similar 
# to a lattice.
# Adapted from function "latmio_und_connected" from BCT:
# https://sites.google.com/site/bctnet
#
# (estimated) Complexity: O(m+n) (* iterations * attempts)
#
# g: network to process.
# iterations: number of times a link is rewired (approximately).
#
# returns: the rewired network.
###########################################################################
latticize.network <- function(g, iterations)
{	DEBUG <- TRUE
	
	# init
	n <- vcount(g)
	m <- ecount(g)
	iter <- m*iterations
	comp.nb <- no.clusters(graph=g)
	# randomize node order
	rdmz <- sample(1:n)
	# maximal number of rewiring attempts per iteration
	max.attempts <- round(n*m/(n*(n-1.0)/2))
	# actual number of successful rewirings
	eff <- 0

	if(DEBUG)
	{	trans <- transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
		tlog(2, "Original graph: \tn: ",n,"\tm: ",m, "\tTransitivity: ",trans)
		transs <- trans
#		image(as.matrix.csr(as_adjacency_matrix(g,type="both",names=FALSE,sparse=FALSE)))
	}
	
	for(it in 1:iter)
	{	if(DEBUG) tlog(4, "Iteration ",it,"/",iter)
		att <- 0
		rewire <- FALSE
		
		# while not rewired
		while(!rewire & att<=max.attempts)
		{	rewire <- FALSE
			#cat("it=",it," iter=",iter," att=",att," max.attempts=",max.attempts,"\n",sep="")			
			# randomly draw 2 links
			es <- igraph.sample(1,m,2)
			temp <- get.edges(graph=g,es=es)
			a <- temp[1,1]
			b <- temp[1,2]
			c <- temp[2,1]
			d <- temp[2,2]
			
			# check if they involve different nodes
			if(length(intersect(c(a,b),c(c,d)))==0)
			{	# possibly flip the first link
				p <- runif(1)
				if(p>0.5)
				{	a <- temp[1,2]
					b <- temp[1,1]
				}
				
				# check if some of the 2 new links already exist
				if(!are.connected(g,a,d) & !are.connected(g,c,b))
				{	
					# lattice condition
					if((abs(rdmz[a]-rdmz[b])+abs(rdmz[c]-rdmz[d]))
						>=(abs(rdmz[a]-rdmz[d])+abs(rdmz[c]-rdmz[b])))
					{	
						# check if the rewiring is going to split the network
						rewire <- !is.splitting.network(g,comp.nb,a,b,c,d) # complexity: O(m+n)
						if(rewire)
						{	g <- delete.edges(graph=g, edges=es) # complexity: O(m+n)
							g <- add.edges(graph=g, edges=c(a,d,b,c)) # complexity: O(m+n)
							eff <- eff + 1
						}
					}
				}
			}
			
			# increment the number of attempts
			att <- att + 1
		}
		if(DEBUG) 
		{	trans <- transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
			tlog(6, "m: ",gsize(g)," Transitivity: ",trans)
			transs <- c(transs, trans)
		}
	}
	
	if(DEBUG) 
	{	tlog(4, paste(transs,collapse=", ")) 
#		image(as.matrix.csr(as_adjacency_matrix(g,type="both",names=FALSE,sparse=FALSE)))
	}
		
	return(g)
}




###########################################################################
# Custom version of function latticize.network, but faster.
#
# g: network to process.
# iterations: number of times a link is rewired (approximately).
#
# returns: the rewired network.
###########################################################################
latticize.network.alt <- function(g, iterations)
{	DEBUG <- TRUE
	
	# init
	n <- gorder(g)
	m <- gsize(g)
	if(DEBUG)
	{	trans <- transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
		tlog(2, "Original graph: \tn: ",n,"\tm: ",m, "\tTransitivity: ",trans)
		transs <- trans
		image(as.matrix.csr(as_adjacency_matrix(g,type="both",names=FALSE,sparse=FALSE)))
	}
	
	# randomly select pairs of edges
	el0 <- as_edgelist(g, names=FALSE)
		
	# repeat as many times as specified
	for(j in 1:iterations)
	{	if(DEBUG) tlog(4, "Iteration ",j,"/",iterations)
		
		# randomize the edges
		smpl <- sample(m)
		el0 <- el0[smpl,]
		smpl1 <- smpl[1:(m %/% 2)]
		smpl2 <- smpl[((m %/% 2)+1):(2*(m %/% 2))]
		
		# split the list of edges
		el1 <- el0[smpl1,]
		el2 <- el0[smpl2,]
		
		# init list of untouched edges
		if(nrow(el)>(nrow(el1)+nrow(el2)))
			el0 <- el[nrow(el),]
		else
			el0 <- matrix(vector(), nrow=0, ncol=2)
		if(DEBUG) tlog(6, "Split:\tel0: ",nrow(el0),"\tel1: ",nrow(el1),"\tel2: ",nrow(el2))
		
		# check that vertices are different
		flag <- sapply(1:nrow(el1), function(i) length(intersect(el1[i,], el2[i,]))==0)
		if(DEBUG) tlog(6, "Different vertices: ",length(which(flag)),"/",nrow(el1))
		el0 <- rbind(el0, el1[!flag,], el2[!flag,])
		el1 <- el1[flag,,drop=FALSE]
		el2 <- el2[flag,,drop=FALSE]
		if(DEBUG) tlog(8, "After filtering:\tel0: ",nrow(el0),"\tel1: ",nrow(el1),"\tel2: ",nrow(el2))
		
		# some edges left
		if(nrow(el1)>0)
		{	# switch vertices for half the edges in the first part
			idx <- which(runif(nrow(el1), min=0, max=1)<0.5)
			if(DEBUG) tlog(6, "Switched edges: ",length(idx),"/",nrow(el1))
			el1b <- el1
			el1b[idx,] <- cbind(el1[idx,2], el1[idx,1])
			
			# build new edges by mixing both parts
			nel1 <- cbind(el1b[,1], el2[,2])
			nel1 <- t(apply(nel1, 1, sort))
			nel2 <- cbind(el2[,1], el1b[,2])
			nel2 <- t(apply(nel2, 1, sort))
			
			# check that edges are not already present
			# identify existing edges
			dt <- as.data.table(el)
			setkey(dt, "V1", "V2")
			dt1 <- as.data.table(nel1)
			setkey(dt1, "V1", "V2")
			idx1 <- dt1[dt, nomatch=0, which=TRUE]
			dt2 <- as.data.table(nel2)
			setkey(dt2, "V1", "V2")
			idx2 <- dt2[dt, nomatch=0, which=TRUE]
			idx <- union(idx1, idx2)
			if(DEBUG) tlog(6, "Non-Existing edges:\tel1: ",nrow(el1)-length(idx1),"/",nrow(el1), "\tel2: ",nrow(el2)-length(idx2),"/",nrow(el2),"\t overall:",nrow(el1)-length(idx),"/",nrow(el1))
			# udpate tables
			el0 <- rbind(el0, el1[idx,], el2[idx,])
			el1 <- el1[-idx,,drop=FALSE]
			el2 <- el2[-idx,,drop=FALSE]
			nel1 <- nel1[-idx,,drop=FALSE]
			nel2 <- nel2[-idx,,drop=FALSE]
			if(DEBUG) tlog(8, "After filtering:\tel0: ",nrow(el0),"\tel1: ",nrow(el1),"\tel2: ",nrow(el2))
			
			# some edges left
			if(nrow(el1)>0)
			{	# check lattice condition
				flag <- abs(el1[,1]-el1[,2])+abs(el2[,1]-el2[,2]) >= abs(nel1[,1]-nel1[,2])+abs(nel2[,1]-nel2[,2])
				if(DEBUG) tlog(6, "Lattice condition: ",length(which(flag)),"/",nrow(el1))
				el0 <- rbind(el0, el1[!flag,,drop=FALSE], el2[!flag,,drop=FALSE])
				el1 <- el1[flag,,drop=FALSE]
				el2 <- el2[flag,,drop=FALSE]
				nel1 <- nel1[flag,,drop=FALSE]
				nel2 <- nel2[flag,,drop=FALSE]
				if(DEBUG) tlog(8, "After filtering:\tel0: ",nrow(el0),"\tel1: ",nrow(el1),"\tel2: ",nrow(el2))
				
				# apply the rewiring
				if(nrow(el1)>0)
					el0 <- rbind(el0, nel1, nel2)
				
				# update graph and transitivity
				if(DEBUG) 
				{	g2 <- delete.edges(g, E(g))
					g2 <- add_edges(g2, edges=c(t(el0)))
					trans <- transitivity(graph=g2, type="localaverage", weights=NA, isolates="zero")
					tlog(6, "End of iteration: edges: ",nrow(el0),"\tTransitivity: ",trans)
					transs <- c(transs, trans)
					#image(as.matrix.csr(as_adjacency_matrix(g2,type="both",names=FALSE,sparse=FALSE)))
				}
			}
		}
	}
	
	# create final graph
	g <- delete.edges(g, E(g))
	g <- add_edges(g, edges=c(t(el0)))
	if(DEBUG) 
	{	tlog(2, "Transitivity: ",paste(transs, collapse=", "))
#		image(as.matrix.csr(as_adjacency_matrix(g,type="both",names=FALSE,sparse=FALSE)))
	}
	
	return(g)
}




###########################################################################
# a few tests
#g <- barabasi.game(n=1000,directed=FALSE,m=3)
#t <- system.time(g2 <- randomize.network(g2,10))
#	print(t)
#t <- system.time(g3 <- latticize.network(g3,10))
#	print(t)
#print(sapply(list(g,g2,g3), function(x) average.path.length(graph=x,directed=FALSE,unconnected=TRUE)))
#print(sapply(list(g,g2,g3), function(x) transitivity(graph=x,type="globalundirected",isolates="zero")))
