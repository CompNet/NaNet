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
library("minpack.lm")

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
	"Exp trans-deg rel",
	"GoF trans-deg rel"
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
	tmp <- trans_degr_rel(g)
	tab[1,"Exp trans-deg rel"] <- tmp$exponent
	tab[1,"GoF trans-deg rel"] <- tmp$gof
	
	return(tab)
}




###############################################################################
# Estimates the exponent of the power law relation between the degree and the
# local transitivity.
#
# g: graph to compute.
#
# returns: result of the estimation and goodness of fit.
###############################################################################
trans_degr_rel <- function(g)
{	# NOTE: not clear whether the transitivity values should be
	# averaged by degree first.
	
	# compute the values
	tra.vals <- transitivity(graph=g, type="local", weights=NA, isolates="zero")
	deg.vals <- degree(g)
	# filter out zero transitivity
	filt.tra <- tra.vals[tra.vals>0]
	filt.deg <- deg.vals[tra.vals>0]
	
	# make a few tries
	best.exp <- NA
	best.gof <- .Machine$integer.max
	for(i in 1:4)
	{	threshold <- quantile(deg.vals)[i]
		
		# only keep the right tail?
		cut.tra <- filt.tra[filt.deg>=threshold]
		cut.deg <- filt.deg[filt.deg>=threshold]
		# build data frame
		df <- data.frame(cut.deg, cut.tra)
		
		# fit model
		fit <- nlsLM(cut.tra ~ c1*cut.deg^c2 + c3, 
				start=list(c1=0, c2=-3, c3=0),
				data = df,
				control=list(maxiter=75))
		
		# retrieve estimated exponent
		exponent <- summary(fit)$coefficients["c2","Estimate"]
		gof <- summary(fit)$sigma
		#gof <- cor(y,predict(fit)) # by curiosity
		
		# plot for future visualization
		plot.file <- file.path(folder, paste0(g$name,"_deg_trans_thre=",threshold,".pdf"))
		pdf(file=plot.file, width=15, height=15)
			plot(
				x=filt.deg, y=filt.tra, 
				main=paste0("Exponent: ",exponent," -- GoF: ",gof),
				xlab="Local Transitivity",
				ylab="Degree",
				log="xy",
				xlim=c(1,max(deg.vals)*1.1),
				ylim=c(0.0001,1)
			)
			x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
			lines(x, predict(fit, list(cut.deg=x)), col="GREEN")
		dev.off()
		
		# keep best fit
		if(gof<best.gof)
		{	best.exp <- exponent
			best.gof <- gof
		}
	}
	
	result <- list(exponent=best.exp, gof=best.gof)
	return(result)
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



