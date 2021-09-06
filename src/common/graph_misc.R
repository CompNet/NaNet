#############################################################################################
# Functions used to handle various graph-related tasks.
# 
# 05/2021 Vincent Labatut
#############################################################################################




#############################################################################################
# Returns the weakly connected largest component of the specified graph, and possibly the
# indices of the concerned vertices in the original graph.
#
# graph: graph to process.
# indices: if TRUE, the function returns a list with both the component and the vertex indices.
# 
# returns: the subgraph corresponding to the largest component, and possibly the ids of the
#          corresponding vertices in the original graph.
#############################################################################################
get.largest.component <- function(g, indices=FALSE)
{	comps <- components(graph=g, mode="weak")
	largest.comp.idx <- which.max(comps$csize)
	idx <- which(comps$membership==largest.comp.idx)
	largest.comp <- induced_subgraph(g, v=idx)
	
	if(indices)
		result <- list(comp=largest.comp, indices=idx)
	else
		result <- largest.comp
	
	return(result)
}




#############################################################################################
# Reverse the weights of the specified graph: if it is a cost, it becomes a capacity, and
# vice-versa. The bounds stay the same.
#
# This is usefull when you have capacities but want to compute a shortest path: igraph
# expects a cost, and not a capacity.
#
# g: weighted graph.
#
# returns: same graph, but reversed weights.
#############################################################################################
reverse.graph.weights <- function(g)
{	w <- E(g)$weight
	w <- reverse.weights(w)
	E(g)$weight <- w
	return(g)
}




#############################################################################################
# Reverse the specified weights, as explained in reverse.graph.weights, except this function
# directly works with weights instead of graphs.
#
# weights: weigts to reverse.
#
# returns: reversed weights.
#############################################################################################
reverse.weights <- function(w)
{	w <- (w-min(w))/(max(w)-min(w)) * (-1) * (max(w)-min(w)) + max(w)
	return(w)
}




#############################################################################################
# Computes a collection of indices that measure how much a network is small-world.
# The graph is assumed undirected, unweighted, and simple (no loop or multiple edges).
#
# Measure sigma comes from:
#	M. D. Humphries, K. Gurney, and T. J. Prescott, 
#	“The brainstem reticular formation is a small-world, not scale-free, network,” 
#	Proceedings of the Royal Society B, 273:503–511, 2006.
#	DOI: 10.1098/rspb.2005.3354
#
# Measure omega comes from:
#	Q. K. Telesford, K. E. Joyce, S. Hayasaka, J. H. Burdette, and P. J. Laurienti, 
#	“The Ubiquity of Small-World Networks,” 
#	Brain Connectivity, 1(5):367–375, 2011.
#	DOI: 10.1089/brain.2011.0038
#
# Measure SWI comes from:
#	Z. Neal, 
#	“Making Big Communities Small: Using Network Science to Understand the Ecological and Behavioral Requirements for Community Social Capital,” 
#	American Journal of Community Psychology, 55(3–4):369–380, 2015.
#	DOI: 10.1007/s10464-015-9720-4
#
# Measure Phi comes from:
#	S. Feldt Muldoon, E. W. Bridgeford, and D. S. Bassett, 
#	“Small-World Propensity and Weighted Brain Networks,” 
#	Scientific Reports, 6:22057, 2016.
#	DOI: 10.1038/srep22057
#
# g: graph to process.
# theoretical: whether to use the theoretical values for random and lattice average distance
#              and transitivity, or a rewiring process respecting the degree distribution.
#              The theoretical approach is much faster, but can result in non-normalized values.
# iterations: parameter used during the rewiring process, if "theoretical" is FALSE.
#
# returns: vector of of named values.
#############################################################################################
compute.smallworldness <- function(g, theoretical=FALSE, iterations=10)
{	res <- c()
	
	# compute topological measuers on the original graph
	l <- mean_distance(graph=g)
	c <- transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
	
	# compute topological measuers on the reference graphs
	if(theoretical)
	{	k <- mean(degree(g))
		n <- gorder(g)
		# random graph
		c_r <- k / n
		l_r <- log(n)/log(k)
		# lattice
		c_l <- (3*(k-2))/(4*(k-1))
		l_l <- n/(2*k)
	}
	else
	{	# randomized graph
		g_r <- randomize.network(g, iterations)
		l_r <- mean_distance(graph=g_r)
		c_r <- transitivity(graph=g_r, type="localaverage", weights=NA, isolates="zero")
		
		# latticed graph
		tolerance <- 0.005
		i <- 0
		c_l <- 0
		go.on <- TRUE
		g2 <- g
		# we may perform several attempts to improve transitivity
		while(i<3 || go.on)
		{	# latticize the network (the more iterations, the closer to a lattice)
			g2 <- latticize.network(g=g2, iterations)
			
			# compute transitivity
			cur.res <- transitivity(graph=g_l, type="localaverage", weights=NA, isolates="zero")
			# compare with current best 
			go.on <- cur.res-c_l > tolerance
			if(cur.res>c_l)
			{	c_l <- cur.res
				g_l <- g2
			}
			tlog(6,"i=",i," current=",cur.res," best=",c_l,"\n",sep="")
			i <- i + 1
		}
		l_l <- mean_distance(graph=g_l)
	}
	
	# sigma measure
	res["sigma_c"] <- c/c_r
	res["sigma_l"] <- l/l_r
	res["sigma"] <- res["sigma_c"] / res["sigma_l"]
	# omega measure
	res["omega_l"] <- l_r/l
	res["omega_c"] <- c/c_l
	res["omega"] <- res["omega_l"] - res["omega_c"]
	# small world index
	res["swi_l"] <- (l-l_l)/(l_r-l_l)
	res["swi_c"] <- (c-c_r)/(c_l-c_r)
	res["swi"] <- res["swi_l"] * res["swi_c"]
	# phi measure
	res["Phi_c"] <- min(1,max(0,(c_l-c)/(c_l-c_r)))
	res["Phi_l"] <- min(1,max(0,(l-l_r)/(l_l-l_r)))
	res["Phi"] <- 1 - sqrt((res["Phi_l"]^2 + res["Phi_c"]^2)/2)
	
	return(res)
}
