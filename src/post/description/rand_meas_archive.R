# Computes certain topological measures, estimated for a null model
# that takes into account the bipartite nature of the graph.
#
# Formulas taken from the following paper:
#	M. E. J. Newman, S. H. Strogatz, and D. J. Watts, 
#	“Random graphs with arbitrary degree distributions and their applications,” 
#	Physical Review E, 6402(2):26118, 2001.
#	DOI: 10.1103/PhysRevE.64.026118
# 
# Note: I never managed to get appropriate results when implementing the formulas
# of the paper. First, some of the estimated values that I get just do not make any 
# sense (ex. transitivity > 1). Second, they do not match the values obtained by simulation.
# >> In the end end, I proceeded using simulations instead of using the closed forms.
#
# Vincent Labatut
# 11/2021
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/rand_meas_archive.R")
###############################################################################
library("polynom")

source("src/common/include.R")




###############################################################################
# load raw data
data <- read.raw.data()
char.stats <- data$char.stats
f.n <- nrow(char.stats)
char.scenes <- data$char.scenes
g.n <- length(char.scenes)

# init result table
rn <- c("AvgDegree","AvgDistance","AvgLocalTrans")
cn <- c("Character","Scene")
tab <- matrix(NA, nrow=length(rn), ncol=length(cn))
rownames(tab) <- rn
colnames(tab) <- cn




###############################################################################
# init bipartite graph
g <- make_empty_graph(n=f.n+g.n, directed=FALSE)
V(g)$type <- c(rep("Character",f.n), rep("Scene",g.n))
# add character names and info
for(coln in colnames(char.stats))
	g <- set_vertex_attr(graph=g, name=coln, index=1:f.n, value=char.stats[,coln])
# add scene names
g <- set_vertex_attr(graph=g, name="Name", index=f.n+(1:g.n), value=paste0("Scene_",1:g.n))
# build edge list
el <- matrix(nrow=0, ncol=2)
for(s in 1:g.n)
{	tlog(2,"Processing scene ",s,"/",g.n)
	idx1 <- match(char.scenes[[s]],V(g)$Name)
	if(any(is.na(idx1)))
		stop("ERROR: could not find character ",char.scenes[[s]][which(is.na(idx1))])
	idx2 <- rep(which(V(g)$Name==paste0("Scene_",s)), length(idx1))
	new.el <- cbind(idx1, idx2)
	#print(new.el)
	el <- rbind(el, new.el)
}
g <- add_edges(graph=g, edges=c(t(el)))
g <- delete_vertices(graph=g, v=which(degree(g, mode="all")<1))
g.n <- length(which(V(g)$type=="Scene"))




###############################################################################
## real data test
#tt <- read.table("Newman-Cond_mat_95-99-two_mode.txt")
#f.n <- max(tt[,1])
#g.n <- max(tt[,2])
#gg <- make_empty_graph(n=f.n+g.n, directed=FALSE)
#gg <- set_vertex_attr(graph=gg, name="Name", index=1:(f.n+g.n), value=c(paste0("Author_",1:f.n),paste0("Article_",1:g.n)))
#V(gg)$type <- c(rep("Character",f.n), rep("Scene",g.n))
#tt[,2] <- tt[,2] + f.n
#gg <- add_edges(graph=gg, edges=c(t(tt)))
#gg <- delete_vertices(graph=gg, v=which(degree(gg, mode="all")<1))
#g <- gg
#tmp <- bipartite_projection(graph=gg, types=V(gg)$type=="Character", multiplicity=FALSE, remove.type=FALSE)
#gg1 <- tmp[[1]]
#mean(degree(gg1))
#gg2 <- tmp[[2]]
#mean(degree(gg2))




###############################################################################
dd.f <- degree(g, v=which(V(g)$type=="Character"), mode="all")
dd.g <- degree(g, v=which(V(g)$type=="Scene"), mode="all")




###############################################################################
# generated test data
vals.f <- unlist(sapply(1:f.n, function(v) rep(v,dd.f[v]))) 
vals.g <- unlist(sapply(1:g.n, function(v) rep(v+f.n,dd.g[v])))
iters <- 10
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




###############################################################################
## testing unipartite graph formulas
#n <- 1000
#gg <- barabasi.game(n=n,m=3,directed=FALSE)
#
## compute formulas
#dd <- degree(gg)
#tt <- table(dd)
#cc <- rep(0, max(dd.f))
#cc[as.integer(names(tt))] <- tt
#cc <- c(0,cc)
#cc <- cc/sum(cc)
#G0 <- polynomial(coef=cc)												# eq.(23)
#G0p <- deriv(G0)
#G0pp <- deriv(G0p)
#G1 <- G0p/mu															# eq.(9)
#G1p <- deriv(G1)
#tlog(4,"G0(1) should be 1: ",predict(G0, 1))							# eq.(3)
#z1 <- mu <- predict(G0p, 1)												# eq.(5)
#tlog(4,"z1 = mu = avg degree = ",mu)
#z2 <- predict(G0pp,1)													# eq.(11)
#tlog(4,"z2=",z2," should be equal to: ",predict(G0p,1)*predict(G1p,1))	# eq.(11)
#d <- (log((n-1)*(z2-z1)+z1^2)-log(z1^2))/log(z2/z1)						# eq.(53)
#d.alt <- log(n/z1)/log(z2/z1) + 1										# eq.(54)
#tlog(4,"Avg. distance: ",d," (alt. ",d.alt,")")
#
## perform simulation
#iters <- 25
#avg.z1 <- rep(NA, iters)
#avg.z2 <- rep(NA, iters)
#avg.d <- rep(NA, iters)
#for(iter in 1:iters)
#{	rand.g <- sample_degseq(out.deg=dd)
#	avg.z1[iter] <- mean(degree(rand.g, mode="all"))
#	avg.z2[iter] <- mean(ego_size(g=rand.g, order=2))
#	avg.d[iter] <- mean_distance(graph=rand.g, directed=FALSE)
#}
#
## >> the expected degree obviously matches the measured one
## >> the expected second order degree is good enough: 69.74 vs. 66.32
## >> but the expected distances are quite different: 3.05 vs. 3.5




###############################################################################
tlog(2,"Deriving necessary functions")
# generative functions for characters
tt <- table(dd.f)
cc <- rep(0, max(dd.f))
cc[as.integer(names(tt))] <- tt
cc <- c(0,cc)
cc <- cc/sum(cc)
f0 <- polynomial(coef=cc)												# eq.(67)
#coef(f0)
tlog(4,"f0(1) should be 1: ",predict(f0, 1))							# eq.(68)
f0p <- deriv(f0)
f0pp <- deriv(f0p)
f0ppp <- deriv(f0pp)
mu <- predict(f0p, 1)													# eq.(68)
tlog(4,"mu=f0'(1) should be ",sum(dd.f)/f.n,": ",mu)
f1 <- f0p/mu															# eq.(69)
f1p <- deriv(f1)

# generative functions for scenes
tt <- table(dd.g)
cc <- rep(0, max(dd.g))
cc[as.integer(names(tt))] <- tt
cc <- c(0,cc)
cc <- cc/sum(cc)
g0 <- polynomial(coef=cc)												# eq.(67)
tlog(4,"g0(1) should be 1: ",predict(g0, 1))							# eq.(68)
g0p <- deriv(g0)
g0pp <- deriv(g0p)
g0ppp <- deriv(g0pp)
nu <- predict(g0p, 1)													# eq.(68)
tlog(4,"nu=g0'(1) should be ",sum(dd.g)/g.n,": ",nu)
g1 <- g0p/nu															# eq.(69)
g1p <- deriv(g1)

# checking the ratios
tlog(4,"Ratios mu/M and nu/N should be equal: ",mu/g.n," vs ",nu/f.n)	# eq.(66)

# generative functions for character projection
F0 <- predict(f0, g1)													# eq.(70)
F0p <- deriv(F0)
F0pp <- deriv(F0p)
F1 <- predict(f1, g1)													# eq.(71)
F1p <- deriv(F1)
f.z1 <- predict(F0p, 1)													# eq.(72)
tlog(4,"z1=F0'(1) should be ",predict(f0p,1)*predict(g1p,1),": ",f.z1)
f.z2 <- predict(F0p, 1)*predict(F1p, 1)									# eq.(73)
tlog(4,"z2=F0'(1)F1'(1) should be ",predict(f0p,1)*predict(f1p,1)*predict(g1p,1)^2,": ",f.z2)

# generative functions for scene projection
G0 <- predict(g0, f1)													# eq.(70)
G0p <- deriv(G0)
G0pp <- deriv(G0p)
G1 <- predict(g1, f1)													# eq.(71)
G1p <- deriv(G1)
g.z1 <- predict(G0p, 1)													# eq.(72)
tlog(4,"z1=G0'(1) should be ",predict(g0p,1)*predict(f1p,1),": ",g.z1)
g.z2 <- predict(G0p, 1)*predict(G1p, 1)									# eq.(73)
tlog(4,"z2=G0'(1)G1'(1) should be ",predict(g0p,1)*predict(g1p,1)*predict(f1p,1)^2,": ",g.z2)




###############################################################################
# average degree
tlog(2,"Average degree")
tlog(4,"For characters: ",f.z1)
tab["AvgDegree","Character"] <- f.z1									# eq.(5)
tlog(4,"For scenes: ",g.z1)
tab["AvgDegree","Scene"] <- g.z1										# eq.(5)
tlog(4,"Simulation results: ",paste(apply(avg.degs, 2, mean),collapse=" "))
# >> large difference with simulated results:
#   36.89 vs.  19.62
# 1219.21 vs. 841.32

# average distance
tlog(2,"Average distance")
f.d <- (log((f.n-1)*(f.z2-f.z1)+f.z1^2)-log(f.z1^2))/log(f.z2/f.z1)		# eq.(53)
f.d0 <- log(f.n/f.z1)/log(f.z2/f.z1) + 1								# eq.(54)
tlog(4,"For characters: ",f.d," (alt. ",f.d0,")")
tab["AvgDistance","Character"] <- f.d
g.d <- (log((g.n-1)*(g.z2-g.z1)+g.z1^2)-log(g.z1^2))/log(g.z2/g.z1)		# eq.(53)
g.d0 <- log(g.n/g.z1)/log(g.z2/g.z1) + 1								# eq.(54)
tlog(4,"For scenes: ",g.d," (alt. ",g.d0,")")
tab["AvgDistance","Scene"] <- g.d
tlog(4,"Simulation results: ",paste(apply(avg.dists, 2, mean),collapse=" "))
# >> large difference with simulated results:
# 1.51 vs. 2.35
# 1.18 vs. 1.90

# average local transitivity
tlog(2,"Average local transitivity")
f.t <- g.n/f.n * predict(f0ppp,1)/predict(F0pp,1)						# eq.(81)
tlog(4,"For characters: ",f.t)
tab["AvgLocalTrans","Character"] <- f.t
g.t <- f.n/g.n * predict(g0ppp,1)/predict(G0pp,1)						# eq.(81)
tlog(4,"For scenes: ",g.t)
tab["AvgLocalTrans","Scene"] <- g.t
tlog(4,"Simulation results: ",paste(apply(avg.trans, 2, mean),collapse=" "))
# >> large difference with simulated results, and not even <1:
# 268.76 vs. 0.64
#  ~0.00 vs. 0.80




###############################################################################
# record result table
