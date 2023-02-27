#############################################################################################
# Functions used to study the small-world property.
# 
# 06/2014 Vincent Labatut
#
# source("src/graphs/smallworldness.R")
#############################################################################################




#############################################################################################
# column names
C_SW <- c()
# simple measures
C_SW_TRANS_OBS <- "Obs Avg Trans"; C_SW <- c(C_SW, C_SW_TRANS_OBS)
C_SW_TRANS_LAT_THE <- "Theor Avg Trans Lattice "; C_SW <- c(C_SW, C_SW_TRANS_LAT_THE)
C_SW_TRANS_LAT_EMP <- "Empir Avg Trans Lattice"; C_SW <- c(C_SW, C_SW_TRANS_LAT_EMP)
C_SW_TRANS_RAND_THE <- "Theor Avg Trans Rand "; C_SW <- c(C_SW, C_SW_TRANS_RAND_THE)
C_SW_TRANS_RAND_EMP <- "Empir Avg Trans Rand"; C_SW <- c(C_SW, C_SW_TRANS_RAND_EMP)
C_SW_DIST_OBS <- "Avg Dist"; C_SW <- c(C_SW, C_SW_DIST_OBS)
C_SW_DIST_LAT_THE <- "Theor Avg Dist Lattice"; C_SW <- c(C_SW, C_SW_DIST_LAT_THE)
C_SW_DIST_LAT_EMP <- "Empir Avg Dist Lattice"; C_SW <- c(C_SW, C_SW_DIST_LAT_EMP)
C_SW_DIST_RAND_THE <- "Theor Avg Dist Rand"; C_SW <- c(C_SW, C_SW_DIST_RAND_THE)
C_SW_DIST_RAND_EMP <- "Empir Avg Dist Rand"; C_SW <- c(C_SW, C_SW_DIST_RAND_EMP)
# smallworldness measures
C_SIGMA <- "sigma"; C_SW <- c(C_SW, C_SIGMA)
C_SIGMA_TRANS <- "sigma_c"; C_SW <- c(C_SW, C_SIGMA_TRANS)
C_SIGMA_DIST <- "sigma_l"; C_SW <- c(C_SW, C_SIGMA_DIST)
C_OMEGA <- "omega"; C_SW <- c(C_SW, C_OMEGA)
C_OMEGA_TRANS <- "omega_c"; C_SW <- c(C_SW, C_OMEGA_TRANS)
C_OMEGA_DIST <- "omega_l"; C_SW <- c(C_SW, C_OMEGA_DIST)
C_SWI <- "swi"; C_SW <- c(C_SW, C_SWI)
C_SWI_TRANS <- "swi_c"; C_SW <- c(C_SW, C_SWI_TRANS)
C_SWI_DIST <- "swi_l"; C_SW <- c(C_SW, C_SWI_DIST)
C_PHI <- "Phi"; C_SW <- c(C_SW, C_PHI)
C_PHI_TRANS <- "Phi_c"; C_SW <- c(C_SW, C_PHI_TRANS)
C_PHI_DIST <- "Phi_l"; C_SW <- c(C_SW, C_PHI_DIST)




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
	tlog(10,"Computing small-worldness measures")
	
	# compute topological measures on the original graph
	l <- mean_distance(graph=g)
	res[C_SW_DIST_OBS] <- l
	tlog(12,"Average distance: ",l)
	c <- transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
	res[C_SW_TRANS_OBS] <- c
	tlog(12,"Average transitivity: ",c)
	
	# compute theoretical values of topological measures on the reference graphs
	k <- mean(degree(g))
	n <- gorder(g)
	# random graph
	l_r <- log(n)/log(k)
	res[C_SW_DIST_RAND_THE] <- l_r
	tlog(12,"Theoretical Average distance in a similar random network: ",l_r)
	c_r <- k / n
	res[C_SW_TRANS_RAND_THE] <- c_r
	tlog(12,"Theoretical Average transitivity in a similar random network: ",c_r)
	# lattice
	l_l <- n/(2*k)
	res[C_SW_DIST_LAT_THE] <- l_l
	tlog(12,"Theoretical Average distance in a similar lattice: ",l_l)
	c_l <- (3*(k-2))/(4*(k-1))
	res[C_SW_TRANS_LAT_THE] <- c_l
	tlog(12,"Theoretical Average transitivity in a similar lattice: ",c_l)
	
	# possibly compute empirical values of topological measures on the reference graphs
	if(!theoretical)
	{	# randomized graph
		#g_r <- randomize.network(g, iterations)
		# NOTE: Instead of randomizing, we now just generate a random graph using the
		# 		Configuration Model. It is much faster, and supposedly equivalent.
		#		We consequently now consider several realizations, to get more 
		#		reliable estimations.
		l_r <- c()
		c_r <- c()
		for(i in 1:25)
		{	g_r <- sample_degseq(out.deg=degree(g), method="vl")
			if(!is.connected(g_r))
				stop("Disconnected random graph generated in compute.smallworldness")
			l_r <- c(l_r, mean_distance(graph=g_r))
			c_r <- c(c_r, transitivity(graph=g_r, type="localaverage", weights=NA, isolates="zero"))
		}
		tlog(12,"Empirical Average distance in a similar random network: [",min(l_r),";",max(l_r),"] ",mean(l_r)," (",sd(l_r),")")
		res[C_SW_DIST_RAND_EMP] <- min(l_r)
		tlog(12,"Empirical Average transitivity in a similar random network: [",min(c_r),";",max(c_r),"] ",mean(c_r)," (",sd(c_r),")")
		res[C_SW_TRANS_RAND_EMP] <- min(c_r)
		
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
			cur.res <- transitivity(graph=g2, type="localaverage", weights=NA, isolates="zero")
			# compare with current best 
			go.on <- cur.res-c_l > tolerance
			if(cur.res>c_l)
			{	c_l <- cur.res
				g_l <- g2
			}
			tlog(14,"iteration: ",i," -- current trans: ",cur.res," best trans: ",c_l)
			i <- i + 1
		}
		l_l <- mean_distance(graph=g_l)
		res[C_SW_DIST_LAT_EMP] <- l_l
		tlog(12,"Empirical Average distance in a similar lattice: ",l_l)
		res[C_SW_TRANS_LAT_EMP] <- c_l
		tlog(12,"Empirical Average transitivity in a similar lattice: ",c_l)
	}
	
	# sigma measure
	res[C_SIGMA_TRANS] <- c/c_r
	res[C_SIGMA_DIST] <- l/l_r
	res[C_SIGMA] <- res[C_SIGMA_TRANS] / res[C_SIGMA_DIST]
	# omega measure
	res[C_OMEGA_DIST] <- l_r/l
	res[C_OMEGA_TRANS] <- c/c_l
	res[C_OMEGA] <- res[C_OMEGA_DIST] - res[C_OMEGA_TRANS]
	# small world index
	res[C_SWI_DIST] <- (l-l_l)/(l_r-l_l)
	res[C_SWI_TRANS] <- (c-c_r)/(c_l-c_r)
	res[C_SWI] <- res[C_SWI_DIST] * res[C_SWI_TRANS]
	# phi measure
	res[C_PHI_DIST] <- min(1,max(0,(l-l_r)/(l_l-l_r)))
	res[C_PHI_TRANS] <- min(1,max(0,(c_l-c)/(c_l-c_r)))
	res[C_PHI] <- 1 - sqrt((res[C_PHI_DIST]^2 + res[C_PHI_TRANS]^2)/2)
	
	return(res)
}
