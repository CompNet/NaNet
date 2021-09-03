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
	
	
	
	
	
	
	y <- transitivity(graph=g, type="local", weights=NA, isolates="zero")
	x <- degree(g)
	df <- data.frame(x,y)
	plot(x,y)
	
	fit <- nlsLM(y ~ c1*x^c2 + c3, 
			start=list(c1=0, c2=-3, c3=0),
			data = df,
			control=list(maxiter=100))
	fit <- nlsLM(y ~ c1*x^c2, 
			start=list(c1=1, c2=-1),
			data = df)
	fit <- nlsLM(y ~ x^c2 + c3, 
			start=list(c2=-1, c3=0),
			data = df)
	fit <- nlsLM(y ~ x^c2, 
			start=list(c2=-1),
			data = df)
	
	tmp <- lm(log(y+1) ~ log(x)) 
	fit <- nlsLM(y ~ c1*x^c2 + c3, 
			start=list(c1=exp(tmp$coefficients[2]), c2=tmp$coefficients[1], c3=0),
			data = df,
			control=list(maxiter=70))
	fit <- nlsLM(y ~ c1*x^c2, 
			start=list(c1=exp(tmp$coefficients[2]), c2=tmp$coefficients[1]),
			data = df)
	
	plot(x,y,log="xy")
	lines(1:200, predict(fit, list(x=1:200)), col="GREEN")
	
	
	
	# nlsLM without the zeros
	y0 <- y[y>0]
	x0 <- x[y>0]
	df0 <- data.frame(x0,y0)
	fit <- nlsLM(y0 ~ c1*x0^c2 + c3, 
			start=list(c1=0, c2=-3, c3=0),
			data = df0,
			control=list(maxiter=100))
	fit <- nlsLM(y0 ~ x0^c2, 
			start=list(c2=-1),
			data = df0)
	summary(fit)
	plot(x0,y0,log="xy")
	lines(1:200, predict(fit, list(x0=1:200)), col="GREEN")
	
	# same with GLM
	powfit <- glm(y0~log(x0),family=gaussian(link=log))
	summary(powfit)
	plot(x0,y0,log="xy")
	#lines(x0,fitted(powfit), col="GREEN")
	lines(1:200,predict(powfit, list(x0=1:200), type="response"), col="GREEN")
	
	
	
	
	# get coeff from a nlsLM fit
	alpha <- summary(fit)$coefficients["c2","Estimate"]
	pval <- summary(fit)$coefficients["c2","Pr(>|t|)"]
	
	
	# average the values for each k
	trans <- transitivity(graph=g, type="local", weights=NA, isolates="zero")
	degr <- degree(g)
	ks <- sort(unique(degr))
	tr.avg <- sapply(ks, function(k) mean(trans[degr==k]))
	plot(ks,tr.avg)
	# try fitting
	df2 <- data.frame(ks,tr.avg)
	powfit2 <- glm(tr.avg~log(ks),family=gaussian(link=log))
	summary(powfit2)
	plot(ks,tr.avg,log="xy")
	lines(1:200,predict(powfit, list(degr=1:200), type="response"), col="GREEN")
	# normalize to get something similar to probas
	tr.avg <- tr.avg/sum(tr.avg)
	data <- unlist(lapply(1:length(ks), function(i) rep(ks[i], round(tr.avg[i]*10000))))
	
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



