# Values of certain topological measure, estimated for a null model
# that takes into account the bipartite nature of the graph. Could not
# get usable results through Newman's formula (Cf. rand_meas_archive.R 
# for details), using simulation instead.
#
# Model taken from the following paper:
# 	M. E. J. Newman, S. H. Strogatz, and D. J. Watts, 
# 	“Random graphs with arbitrary degree distributions and their applications,” 
#	Physical Review E, 6402(2):26118, 2001.
#
# Vincent Labatut
# 11/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/rand_meas.R.R")
###############################################################################
source("src/common/include.R")




###############################################################################
# Reads the raw data and builds a bipartite graph. The first dimension (FALSE)
# is Characters, whereas the second (TRUE) is Scenes.
#
# returns: list containing two bipartite graphs: not filtered and filtered.
###############################################################################
load.as.bipartite <- function()
{	# load raw data
	data <- read.raw.data()
	char.info <- data$char.info
	f.n <- nrow(char.info)
	char.scenes <- data$char.scenes
	g.n <- length(char.scenes)
	
	# init bipartite graph
	bg <- make_empty_graph(n=f.n+g.n, directed=FALSE)
	V(bg)$MyType <- c(rep("Character",f.n), rep("Scene",g.n))
	V(bg)$type <- c(rep(FALSE,f.n), rep(TRUE,g.n))
	
	# add character names and info
	for(coln in colnames(char.info))
		bg <- set_vertex_attr(graph=bg, name=coln, index=1:f.n, value=char.info[,coln])
	
	# add scene names
	bg <- set_vertex_attr(graph=bg, name="Name", index=f.n+(1:g.n), value=paste0("Scene_",1:g.n))
	
	# build edge list
	el <- matrix(nrow=0, ncol=2)
	for(s in 1:g.n)
	{	tlog(2,"Processing scene ",s,"/",g.n)
		idx1 <- match(char.scenes[[s]],V(bg)$Name)
		if(any(is.na(idx1)))
			stop("ERROR: could not find character ",char.scenes[[s]][which(is.na(idx1))])
		idx2 <- rep(which(V(bg)$Name==paste0("Scene_",s)), length(idx1))
		new.el <- cbind(idx1, idx2)
		#print(new.el)
		el <- rbind(el, new.el)
	}
	bg <- add_edges(graph=bg, edges=c(t(el)))
	
	# remove isolates
	bg <- delete_vertices(graph=bg, v=which(degree(bg, mode="all")<1))
	
	# filtered version
	graph.file <- get.path.graph.file(mode="scenes")
	g <- read_graph(file=graph.file, format="graphml")
	idx <- match(V(g)[V(g)$Filtered]$name, V(bg)$Name)
	bg.filtr <- delete_vertices(graph=bg, v=idx)
	bg.filtr <- delete_vertices(graph=bg.filtr, v=which(degree(bg.filtr, mode="all")<1))
	
	res <- list(natural=bg, filtered=bg.filtr)
	return(res)
}




###############################################################################
# Computes a selection of topological measures for both random networks corresponding
# to the projections of the specified bipartite network.
#
# g: bipartite network.
# iters: number of generated networks used to perform the estimation.
# 
# returns: matrix containing the measures for each projection of the network
#          (character vs. scenes).
###############################################################################
rand.graph.measures <- function(g, iters=10)
{	# init result table
	cn <- c("AvgDegree","MaxDegree","DegAssort","AvgDistance","MaxDistance","AvgLocalTrans")
	lst.top <- list()
	for(i in 1:2)
	{	tab <- matrix(NA, nrow=iters, ncol=length(cn))
		rownames(tab) <- 1:iters
		colnames(tab) <- cn
		lst.top[[i]] <- tab
	}
	
	# compute degrees
	f.n <- length(which(!V(g)$type))
	g.n <- length(which(V(g)$type))
	dd.f <- degree(g, v=which(!V(g)$type), mode="all")
	dd.g <- degree(g, v=which(V(g)$type), mode="all")
	
	# generate simulation data
	vals.f <- unlist(sapply(1:f.n, function(v) rep(v,dd.f[v]))) 
	vals.g <- unlist(sapply(1:g.n, function(v) rep(v+f.n,dd.g[v])))
	for(iter in 1:iters)
	{	tlog(2,"Processing iteration ",iter,"/",iters)
		el <- cbind(sample(vals.f, size=length(vals.f), replace=FALSE), sample(vals.g, size=length(vals.g), replace=FALSE))
		rand.g <- make_graph(edges=c(t(el)), directed=FALSE)
		V(rand.g)$type <- c(rep(FALSE, f.n), rep(TRUE, g.n))
		#tlog(2, "Bipartite graph: ",is.bipartite(rand.g))
		lst.graph <- bipartite_projection(rand.g, multiplicity=FALSE)
		for(i in 1:2)
		{	lst.top[[i]][iter,"AvgDegree"] <- mean(degree(lst.graph[[i]], mode="all"))
			lst.top[[i]][iter,"MaxDegree"] <- max(degree(lst.graph[[i]], mode="all"))
			lst.top[[i]][iter,"DegAssort"] <- assortativity_degree(lst.graph[[i]], directed=FALSE)
			lst.top[[i]][iter,"AvgDistance"] <- mean_distance(graph=lst.graph[[i]], directed=FALSE)
			lst.top[[i]][iter,"MaxDistance"] <- max(distances(graph=lst.graph[[i]], mode="all"))
			lst.top[[i]][iter,"AvgLocalTrans"] <- transitivity(graph=lst.graph[[i]], type="localaverage", isolates="zero")
		}
	}
	
	# fill result table
	res <- cbind(
			apply(lst.top[[1]],2,mean),
			apply(lst.top[[2]],2,mean)
		)
	colnames(res) <- c(FALSE, TRUE)
	return(res)
}




###############################################################################
# Produces a (possibly partial) lattice graph containing the specified numbers
# of vertices and edges. It is a closed one dimensional lattice (a ring).
# The method is deterministic and always returns the exact same graph. 
#
# n: number of vertices.
# m: number of edges.
#
# returns: produced quasi-lattice.
###############################################################################
make.lattice <- function(n, m)
{	g <- make_ring(n=n, directed=FALSE)
	
	v <- 1
	nei.order <- 2
	rem <- m - gsize(g)
	while(rem>0)
	{	# treat left neighbor
		left.nei <- ((v-1-nei.order+n) %% n) + 1
		g <- add_edges(graph=g, edges=c(v,left.nei))
		rem <- rem - 1
		# possibly treat right neighbor
		if(rem>0)
		{	right.nei <- ((v-1+nei.order) %% n) + 1
			g <- add_edges(graph=g, edges=c(v,right.nei))
			rem <- rem - 1
		}
		# next vertex
		v <- v + 1
		if(v>n)
		{	v <- 1
			nei.order <- nei.order + 1
		}
	}
	
	tlog(2,"n=",n,"m=",m," vs. ",gorder(g)," ",gsize(g))
	return(g)
}




###############################################################################
# Computes a selection of topological measures for a lattice network comparable
# to the original character network (same size, same density).
#
# filtered: whether to process the natural or filtered network.
#
# returns: vector containing the measures.
###############################################################################
lattice.graph.measures <- function(filtered=FALSE)
{	# load the original network 
	graph.file <- get.path.graph.file(mode="scenes")
	g <- read_graph(file=graph.file, format="graphml")
	if(filtered)
		g <- delete_vertices(graph=g, v=which(V(g)$Filtered))
	# build lattice
	g <- make.lattice(n=gorder(g), m=gsize(g)) 
	
	# init result table
	cn <- c("AvgDegree","MaxDegree","DegAssort","AvgDistance","MaxDistance","AvgLocalTrans")
	res <- matrix(NA,nrow=length(cn),ncol=1)
	rownames(res) <- cn
	
	res["AvgDegree",1] <- mean(degree(g, mode="all"))
	res["MaxDegree",1] <- max(degree(g, mode="all"))
	res["DegAssort",1] <- assortativity_degree(g, directed=FALSE)
	res["AvgDistance",1] <- mean_distance(graph=g, directed=FALSE)
	res["MaxDistance",1] <- max(distances(graph=g, mode="all"))
	res["AvgLocalTrans",1] <- transitivity(graph=g, type="localaverage", isolates="zero")
	
	return(res)
}




###############################################################################
# build bipartite graphs from raw data
iters <- 100
lst.g <- load.as.bipartite()

# compute measures for natural graph
tab <- rand.graph.measures(g=lst.g$natural, iters=iters)
tlog(0,"Measures for the natural network:");print(tab)
file <- get.path.topomeas.plot(object=NA, mode="scenes", meas.name=NA, filtered=FALSE, plot.type="random_model")
write.csv(x=tab, file=paste0(file,".csv"), row.names=TRUE)

# compute measures for filtered graph
tab.filtr <- rand.graph.measures(g=lst.g$filtered, iters=iters)
tlog(0,"Measures for the filtered network:");print(tab.filtr)
file <- get.path.topomeas.plot(object=NA, mode="scenes", meas.name=NA, filtered=TRUE, plot.type="random_model")
write.csv(x=tab.filtr, file=paste0(file,".csv"), row.names=TRUE)




###############################################################################
# process natural network
tab <- lattice.graph.measures(filtered=FALSE)
tlog(0,"Measures for the natural network:");print(tab)
file <- get.path.topomeas.plot(object=NA, mode="scenes", meas.name=NA, filtered=FALSE, plot.type="quasi_lattice")
write.csv(x=tab, file=paste0(file,".csv"), row.names=TRUE)

# process filtered network
tab.filtr <- lattice.graph.measures(filtered=TRUE)
tlog(0,"Measures for the natural network:");print(tab.filtr)
file <- get.path.topomeas.plot(object=NA, mode="scenes", meas.name=NA, filtered=FALSE, plot.type="quasi_lattice")
write.csv(x=tab.filtr, file=paste0(file,".csv"), row.names=TRUE)
