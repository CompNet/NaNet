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

source("src/common/logging.R")
source("src/common/distr_test.R")




###############################################################################
# names of the stats
measnames <- c(
	"n",
	"m",
	"Density",
	"Avg k",
	"Avg dist",
	"Rand avg dist",
	"Norm avg dist",
	"Avg trans",
	"Rand avg trans",
	"Norm avg trans",
	"Smallworldness",
	"Max k",
	"Degr distrib",
	"Degr ass",
	"Exp trans-deg rel"
)

# init stat table
tab <- matrix(NA, nrow=length(files), ncol=length(measnames))
rownames(tab) <- files
colnames(tab) <- measnames

# compute all stats for all networks
for(file in files)
{	# read and convert graph file
	f <- file.path(folder,paste0(file,".gexf"))
	cat("Processing file",f,"\n")
	gexf.g <- read.gexf(f)
	g <- gexf.to.igraph(gexf.g)
	# remove negative links
	#g <- delete_edges(graph=g, edges=which(E(g)$weight<0))
	
	# fill stats table
	tab[file,"n"] <- gorder(g)
	tab[file,"m"] <- gsize(g)
	tab[file,"Density"] <- graph.density(g)
	tab[file,"Avg k"] <- mean(degree(g))
	tab[file,"Avg dist"] <- mean_distance(graph=g)
	tab[file,"Rand avg dist"] <- log(tab[file,"n"]) / log(tab[file,"Avg k"])
	tab[file,"Norm avg dist"] <- tab[file,"Avg dist"] / tab[file,"Rand avg dist"]
	tab[file,"Avg trans"] <- transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
	tab[file,"Rand avg trans"] <- tab[file,"Avg k"] / tab[file,"n"]
	tab[file,"Norm avg trans"] <- tab[file,"Avg trans"] / tab[file,"Rand avg trans"]
	tab[file,"Smallworldness"] <- tab[file,"Norm avg trans"] / tab[file,"Norm avg dist"]
	tab[file,"Max k"] <- max(degree(g))
	tab[file,"Exp deg power law"] <- test.cont.distr(if(any(degree(g)==0)) degree(g)+1 else degree(g))
	tab[file,"Exp deg power law"] <- test.pl(degree(g))
	tab[file,"Degr ass"] <- assortativity_degree(graph=g, directed=FALSE)
tab[file,"Exp trans-deg rel"] <- NA
	
	# export graph as graphml for possible later use
	f <- file.path(folder,paste0(file,".graphml"))
	write.graph(graph=g, file=f, format="graphml")
}

# record stats table
tab.file <- file.path(folder,"stats.csv")
write.table(tab, file=tab.file, sep=",", row.names=T, col.names=T)
