# Computes certain topological measures, estimated for a null model
# that takes into account the bipartite nature of the graph. Could not
# get usable results through Newman's formula (cf. rand_meas_archive.R 
# for details), so using simulation instead.
#
# Model taken from the following paper:
#	M. E. J. Newman, S. H. Strogatz, and D. J. Watts, 
#	“Random graphs with arbitrary degree distributions and their applications,” 
#	Physical Review E, 6402(2):26118, 2001.
#	DOI: 10.1103/PhysRevE.64.026118
#
# Vincent Labatut
# 11/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/rand_meas.R.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="RandMeas")




###############################################################################
# Loads the stat table for the functions in this script.
#
# filtered: whether we are dealing with the filtered network.
#
# returns: the stat table.
###############################################################################
load.randmeas.stats <- function(filtered=FALSE)
{	tab.file <- get.path.topomeas.plot(object=NA, mode="scenes", meas.name=NA, filtered=filtered, plot.type="model_stats.csv")
	if(file.exists(tab.file))
		res <- read.csv(tab.file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE, row.names=1)
	else
	{	# init result table
		rn <- c("AvgDegree","MaxDegree","DegAssort","AvgDistance","MaxDistance","AvgLocalTrans")
		cn <- c("RandBipartite_FALSE","RandBipartite_TRUE","RandErdosRenyi","RandConfig","Lattice")
		res <- matrix(NA, nrow=length(rn), ncol=length(cn))
		rownames(res) <- rn
		colnames(res) <- cn
		record.randmeas.stats(tab=res, filtered=filtered)
	}
	return(res)
}




###############################################################################
# Records the stat table for the functions in this script.
#
# tab: the stat table.
# filtered: whether we are dealing with the filtered network.
###############################################################################
record.randmeas.stats <- function(tab, filtered=FALSE)
{	tab.file <- get.path.topomeas.plot(object=NA, mode="scenes", meas.name=NA, filtered=filtered, plot.type="model_stats.csv")
	write.csv(x=tab, file=tab.file, row.names=TRUE)
}




###############################################################################
# Reads the raw data and builds a bipartite graph. The first dimension (FALSE)
# is Characters, whereas the second (TRUE) is Scenes.
#
# returns: list containing two bipartite graphs: not filtered and filtered.
###############################################################################
load.as.bipartite <- function()
{	# load raw data
	data <- read.raw.data()
	char.stats <- data$char.stats
	f.n <- nrow(char.stats)
	char.scenes <- data$char.scenes
	g.n <- length(char.scenes)
	
	# init bipartite graph
	bg <- make_empty_graph(n=f.n+g.n, directed=FALSE)
	V(bg)$MyType <- c(rep("Character",f.n), rep("Scene",g.n))
	V(bg)$type <- c(rep(FALSE,f.n), rep(TRUE,g.n))
	
	# add character names and info
	for(coln in colnames(char.stats))
		bg <- set_vertex_attr(graph=bg, name=coln, index=1:f.n, value=char.stats[,coln])
	
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
	graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
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
# filtered: whether the graph is filtered.
# iters: number of generated networks used to perform the estimation.
# 
# returns: stats table.
###############################################################################
rand.bipartite.graph.measures <- function(g, filtered=FALSE, iters=10)
{	# load stats table
	res <- load.randmeas.stats(filtered=filtered)
	
	# init stat list
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
			tmp <- distances(graph=lst.graph[[i]], mode="all")
			lst.top[[i]][iter,"MaxDistance"] <- max(tmp[!is.infinite(tmp)])
			lst.top[[i]][iter,"AvgLocalTrans"] <- transitivity(graph=lst.graph[[i]], type="localaverage", isolates="zero")
		}
	}
	
	# fill result table
	res[cn,"RandBipartite_FALSE"] <- apply(lst.top[[1]],2,mean)
	res[cn,"RandBipartite_TRUE"] <- apply(lst.top[[2]],2,mean)
	# record the table
	record.randmeas.stats(tab=res, filtered=filtered)
	
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
# returns: stats table.
###############################################################################
lattice.graph.measures <- function(filtered=FALSE)
{	# load stats table
	res <- load.randmeas.stats(filtered=filtered)
	
	# load the original network 
	graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
	g <- read_graph(file=graph.file, format="graphml")
	if(filtered)
		g <- delete_vertices(graph=g, v=which(V(g)$Filtered))
	# build lattice
	g <- make.lattice(n=gorder(g), m=gsize(g)) 
	
	# update stat table
	res["AvgDegree","Lattice"] <- mean(degree(g, mode="all"))
	res["MaxDegree","Lattice"] <- max(degree(g, mode="all"))
	res["DegAssort","Lattice"] <- assortativity_degree(g, directed=FALSE)
	res["AvgDistance","Lattice"] <- mean_distance(graph=g, directed=FALSE)
	tmp <- distances(graph=g, mode="all")
	res["MaxDistance","Lattice"] <- max(tmp[!is.infinite(tmp)])
	res["AvgLocalTrans","Lattice"] <- transitivity(graph=g, type="localaverage", isolates="zero")
	
	# record the table
	record.randmeas.stats(tab=res, filtered=filtered)
	
	return(res)
}




###############################################################################
# Compute the topological measures in the Erdos-Rényi or Configuration random models.
#
# filtered: whether the graph is filtered.
# iters: number of generated networks used to perform the estimation.
# model: either Erdos-Rényi ("RandErdosRenyi") or Configuration ("RandConfig").
#
# returns: stat table.
###############################################################################
rand.igraphmodel.graph.measures <- function(filtered=FALSE, iters=iters, model="RandErdosRenyi")
{	# load stats table
	res <- load.randmeas.stats(filtered=filtered)
	
	# load the original network 
	graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
	g <- read_graph(file=graph.file, format="graphml")
	if(filtered)
		g <- delete_vertices(graph=g, v=which(V(g)$Filtered))
	
	# compute degree
	dd <- degree(g, mode="all")
	n <- gorder(g)
	m <- gsize(g)
	
	# init iter stat table
	cn <- c("AvgDegree","MaxDegree","DegAssort","AvgDistance","MaxDistance","AvgLocalTrans")
	tab <- matrix(NA, nrow=iters, ncol=length(cn))
	rownames(tab) <- 1:iters
	colnames(tab) <- cn
	
	# generate simulation data
	for(iter in 1:iters)
	{	tlog(2,"Processing iteration ",iter,"/",iters)
		if(model=="RandErdosRenyi")
			rg <- sample_gnm(n=n, m=m, directed=FALSE, loops=FALSE)
		else if(model=="RandConfig")
			rg <- sample_degseq(out.deg=dd)
		
		tab[iter,"AvgDegree"] <- mean(degree(rg, mode="all"))
		tab[iter,"MaxDegree"] <- max(degree(rg, mode="all"))
		tab[iter,"DegAssort"] <- assortativity_degree(rg, directed=FALSE)
		tab[iter,"AvgDistance"] <- mean_distance(graph=rg, directed=FALSE)
		tmp <- distances(graph=rg, mode="all")
		tab[iter,"MaxDistance"] <- max(tmp[!is.infinite(tmp)])
		tab[iter,"AvgLocalTrans"] <- transitivity(graph=rg, type="localaverage", isolates="zero")
	}

	# fill result table
	res[cn,model] <- apply(tab,2,mean)
	# record the table
	record.randmeas.stats(tab=res, filtered=filtered)
	
	return(res)
}




###############################################################################
# build bipartite graphs from raw data
tlog(0,"Dealing with Newman's bipartite model")
iters <- 100
lst.g <- load.as.bipartite()

# compute measures for projections
tab <- rand.bipartite.graph.measures(g=lst.g$natural, filtered=FALSE, iters=iters)
tlog(2,"Measures for the natural network:");print(tab)

# compute measures for filtered graph
tab.filtr <- rand.bipartite.graph.measures(g=lst.g$filtered, filtered=TRUE, iters=iters)
tlog(2,"Measures for the filtered network:");print(tab.filtr)




###############################################################################
# process ER model
tlog(0,"Dealing with the Erdos-Rényi model")

# process natural network
tab <- rand.igraphmodel.graph.measures(filtered=FALSE, iters=iters, model="RandErdosRenyi")
tlog(2,"Measures for the natural network:");print(tab)

# process filtered network
tab.filtr <- rand.igraphmodel.graph.measures(filtered=TRUE, iters=iters, model="RandErdosRenyi")
tlog(2,"Measures for the filtered network:");print(tab.filtr)




###############################################################################
# process configuration model
tlog(0,"Dealing with the Configuration model")

# process natural network
tab <- rand.igraphmodel.graph.measures(filtered=FALSE, iters=iters, model="RandConfig")
tlog(2,"Measures for the natural network:");print(tab)

# process filtered network
tab.filtr <- rand.igraphmodel.graph.measures(filtered=TRUE, iters=iters, model="RandConfig")
tlog(2,"Measures for the filtered network:");print(tab.filtr)




###############################################################################
tlog(0,"Dealing with the lattice")

# process natural network
tab <- lattice.graph.measures(filtered=FALSE)
tlog(2,"Measures for the natural network:");print(tab)

# process filtered network
tab.filtr <- lattice.graph.measures(filtered=TRUE)
tlog(2,"Measures for the natural network:");print(tab.filtr)



###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
