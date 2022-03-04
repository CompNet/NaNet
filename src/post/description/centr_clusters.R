# Cluster analysis of the centrality measures.
# Did not yield anything interesting, not time to dig further.
# 
# Vincent Labatut
# 02/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/centr_clusters.R.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="CentrClusters")




###############################################################################
tlog(0, "Cluster analysis of the centrality measures")
klim <- 20	# maximal k value when clustering the data

# read the graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
# clean names
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
kept <- which(!V(g)$Filtered)

# measure names
centr.names <- c(MEAS_DEGREE, MEAS_BETWEENNESS, MEAS_CLOSENESS, MEAS_EIGENCNTR)

# get centrality values
tlog(0, "Read centrality values")
vals.unf <- matrix(NA, nrow=gorder(g), ncol=length(centr.names), dimnames=list(V(g)$name,centr.names))
vals.flt <- matrix(NA, nrow=length(kept), ncol=length(centr.names), dimnames=list(V(g)$name[kept],centr.names))
for(centr.name in centr.names)
{		vals.unf[,centr.name] <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=centr.name, filtered=FALSE)
		vals.flt[,centr.name] <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=centr.name, filtered=TRUE)
}

# filter NA values
vals.unf <- vals.unf[apply(vals.unf, 1, function(row) all(!is.na(row))),]
vals.flt <- vals.flt[apply(vals.flt, 1, function(row) all(!is.na(row))),]
# or set them to zero?
#idx <- which(is.na(vals.unf), arr.ind=TRUE)
#vals.unf[idx[,1],idx[,2]] <- 0
#idx <- which(is.na(vals.flt), arr.ind=TRUE)
#vals.flt[idx[,1],idx[,2]] <- 0

# standardize the data
vals.unf.sc <- scale(vals.unf)
vals.flt.sc <- scale(vals.flt)

# apply k-means
dd <- as.matrix(dist(vals.unf.sc))
sils <- rep(NA,klim-1)
best.sil <- -1
best.membership <- NA
best.k <- NA
for(k in 2:klim)
{	membership <- kmeans(x=vals.unf.sc, centers=k)$cluster
	tmp <- silhouette(x=membership, dist=dd)
	sil <- summary(tmp)$avg.width
	sils[k-1] <- sil
	if(sil>best.sil)
	{	best.sil <- sil
		best.membership <- membership
		best.k <- k
	}
}
tmp <- silhouette(x=best.membership, dist=dd)
#res <- hclust(d=dd)	# dataset is too large

# plot silhouette values
plot(2:klim, sils, ylim=0:1, xlab="Number of clusters (k)", ylab="Average Silhouette Width", col="RED")
# plot silhouette profiles
plot(tmp, border=NA, main="Silhouette plot", col=CAT_COLORS_8[1:best.k])

# display the average centrality measures for each cluster
print(sapply(1:best.k, function(c) colMeans(vals.unf.sc[best.membership==c,])))
print(sapply(1:best.k, function(c) colMeans(vals.unf[best.membership==c,])))

# 3D plot of the first 3 centrality measures
# scatterplot3d(log(vals.unf[,1]+1), log(vals.unf[,2]+1), log(vals.unf[,3]+1), color=membership, log="xyz")

# 2D plot of a projection
library("FactoMineR")
fit <- cmdscale(dd, eig=FALSE, k=2)
fit <- PCA(X=vals.unf.sc, ncp=2)
# https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/
plot(fit-min(fit), col=CAT_COLORS_8[best.membership], log="xy")




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
