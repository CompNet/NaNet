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
	rn <- c("AvgDegree","AvgDistance","AvgLocalTrans")
	cn <- c(FALSE,TRUE)
	tab <- matrix(NA, nrow=length(rn), ncol=length(cn))
	rownames(tab) <- rn
	colnames(tab) <- cn
	
	# compute degrees
	f.n <- length(which(!V(g)$type))
	g.n <- length(which(V(g)$type))
	dd.f <- degree(g, v=which(!V(g)$type), mode="all")
	dd.g <- degree(g, v=which(V(g)$type), mode="all")
	
	# generate simulation data
	vals.f <- unlist(sapply(1:f.n, function(v) rep(v,dd.f[v]))) 
	vals.g <- unlist(sapply(1:g.n, function(v) rep(v+f.n,dd.g[v])))
	avg.degs <- matrix(ncol=2, nrow=iters)
	avg.dists <- matrix(ncol=2, nrow=iters)
	avg.trans <- matrix(ncol=2, nrow=iters)
	for(iter in 1:iters)
	{	tlog(2,"Processing iteration ",iter,"/",iters)
		el <- cbind(sample(vals.f, size=length(vals.f), replace=FALSE), sample(vals.g, size=length(vals.g), replace=FALSE))
		rand.g <- make_graph(edges=c(t(el)), directed=FALSE)
		V(rand.g)$type <- c(rep(FALSE, f.n), rep(TRUE, g.n))
		#tlog(2, "Bipartite graph: ",is.bipartite(rand.g))
		lst <- bipartite_projection(rand.g, multiplicity=FALSE)
		for(i in 1:2)
		{	avg.degs[iter,i] <- mean(degree(lst[[i]], mode="all"))
			avg.dists[iter,i] <- mean_distance(graph=lst[[i]], directed=FALSE)
			avg.trans[iter,i] <- transitivity(graph=lst[[i]], type="localaverage", isolates="zero")
		}
	}
	
	# fill result table
	tab["AvgDegree",] <- apply(avg.degs,2,mean)
	tab["AvgDistance",] <- apply(avg.dists,2,mean)
	tab["AvgLocalTrans",] <- apply(avg.trans,2,mean)
	
	return(tab)
}




###############################################################################
# build bipartite graphs from raw data
lst <- load.as.bipartite()

# compute measures for natural graph
tab <- rand.graph.measures(g=lst$natural, iters=3)
file <- get.path.topomeas.plot(object=NA, mode="scenes", meas.name=NA, filtered=FALSE, plot.type="random_model")
write.csv(x=tab, file=paste0(file,".csv"), row.names=TRUE)

# compute measures for filtered graph
tab.filtr <- rand.graph.measures(g=lst$filtered, iters=3)
file <- get.path.topomeas.plot(object=NA, mode="scenes", meas.name=NA, filtered=TRUE, plot.type="random_model")
write.csv(x=tab.filtr, file=paste0(file,".csv"), row.names=TRUE)
