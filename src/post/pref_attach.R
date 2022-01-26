# Compute and plots the preferential attachment descriptor from:
#	H. Jeong, Z. Neda, and A.-L. Barabási, 
#	“Measuring preferential attachment for evolving networks,” 
#	Europhysics Letters, vol. 61, no. 4, pp. 567–572, 2003.
#	DOI: 10.1209/epl/i2003-00166-9
# 
# I also used this paper, which explains the methods in more detail:
#	J. M. Olesen, J. Bascompte, H. Elberling, and P. Jordano, 
#	“Temporal Dynamics in a Pollination Network,” 
#	Ecology, vol. 89, no. 6, pp. 1573–1582, 2008.
#	DOI: 10.1890/07-0451.1
#
# And here is a slightly different approach:
#	M. E. J. Newman, 
#	“Clustering and preferential attachment in growing networks,” 
#	Physical Review E, vol. 64, no. 2, p. 25102, 2001.
#	DOI: 10.1103/PhysRevE.64.025102
# 
# Note: I got results unstable results when studying the evolution of
# the estimated exponent. Not sure why (could be NLS).
#
# Vincent Labatut
# 01/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/pref_attach.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="PrefAttach")




###############################################################################
# plots unfiltered and filtered figures as separate files
tlog(0,"Starting to produce the preferential attachment plots")

# plot parameters
pal <- get.palette(2)
xlab <- "Degree $k$"
ylab <- "Attachment probability"

# load full graph to get filtered characters
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
filt.names <- V(g)$name[V(g)$Filtered]

# load raw data
tlog(0,"Extract the sequence of scene-related cumulative graphs")
data <- read.raw.data()
# compute the sequence of scene-based graphs (possibly one for each scene)
gs <- extract.static.graph.scenes(volume.info=data$volume.info, char.info=data$char.info, page.info=data$page.info, inter.df=data$inter.df, stats.scenes=data$stats.scenes, ret.seq=TRUE)
gs.filt <- future_lapply(gs, function(g) delete_vertices(g, v=intersect(filt.names,V(g)$name)))




################################################################################
# focus on the middle of the period for unfiltered net
t0 <- round(length(gs)*0.5)
t1 <- t0+1 #round(length(gs)*0.40)
dt <- round(length(gs)*0.2)
t2 <- min(t1+dt, length(gs))

# retrieve vertex sets
v0 <- V(gs[[t0]])$name
v1 <- V(gs[[t1]])$name
v2 <- V(gs[[t2]])$name
# new vertices in the [t1,t1+dt] period
dv <- union(setdiff(v1, v0), setdiff(v2, v1))
# their neighbors
nei <- names(unlist(unname(adjacent_vertices(graph=gs[[t2]], v=dv, mode="all"))))
# keep only vertices existing at t0
nei <- nei[nei %in% v0]
idx <- match(nei, v0)
norm <- length(idx)

# compute cumulative distribution
deg0 <- igraph::degree(graph=gs[[t0]], mode="all")
tt <- table(deg0[idx])
deg.vals <- as.integer(names(tt))
norms <- sapply(deg.vals, function(d) length(which(deg0==d)))
#vals <- tt/norm									# Barabasi's version
#vals <- tt/norm*gorder(gs[[t0]])/norms				# Newman's version
vals <- tt/norms/sum(tt/norms)						# alt Barabasi's version
cum.vals <- cumsum(vals)
	
## compute degree changes (alt, but equivalent to above block)
#tt <- table(idx)
#idx <- as.integer(names(tt))
## compute degree and merge same degree values
#deg0 <- igraph::degree(graph=gs[[t0]], v=idx, mode="all")
#deg.vals <- sort(unique(deg0))
#vals <- sapply(deg.vals, function(d) sum(tt[which(deg0==d)]))
## compute the cumulative values
#cum.vals <- cumsum(vals/norm)
	
## compute degree changes (alt 2, slightly different from above block, distinguish all vertices as Olesen et al. do)
#tt <- table(factor(nei, levels=v0))
## compute degree
#deg0 <- igraph::degree(graph=gs[[t0]], mode="all")
#ord.idx <- order(deg0)
#deg.vals <- deg0[ord.idx]
#vals <- tt[ord.idx]/norm
## compute the cumulative values
#cum.vals <- cumsum(vals)
## clean values
#idx <- which(cum.vals>0 & deg.vals>0)
#deg.vals <- deg.vals[idx]
#cum.vals <- cum.vals[idx]

# plot
#plot(x=deg.vals, y=cum.vals, log="xy", xlab=TeX(xlab), ylab=TeX(ylab))

# try with various thresholds
thresholds <- quantile(deg.vals, probs=c(1,0.75,0.50,0.25))
threshold <- thresholds[2]	# exp=1+1.17

# only keep the left tail
cut.cum <- cum.vals[deg.vals<=threshold]
cut.deg <- deg.vals[deg.vals<=threshold]

# init parameters using a linear regression
fit <- lm(log(cut.cum) ~ log(cut.deg))
print(summary(fit))
params <- fit$coefficients
val1 <- exp(params[1]); names(val1) <- NULL
val2 <- params[2]; names(val2) <- NULL
val3 <- 0

# perform NL regression
df <- data.frame(cut.deg, cut.cum)
fit <-nlsLM(cut.cum ~ c1*cut.deg^c2, 
			start=list(c1=val1, c2=val2),
			data=df,
			control=list(maxiter=200))
print(summary(fit))
	
# plot results
plot.file <- get.path.comparison.plot(object="nodes", mode="scenes", meas.name="degree", filtered=FALSE, plot.type="pref_attach")
tlog(2,"Plot in file ",plot.file)
pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
par(
	mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
	fig=c(0,1,0,1),		# set coordinate space of the original plot
	mgp=c(3,1,0)		# distance between axis ticks and values
)
# plot cumulative values
plot(
	x=deg.vals, y=cum.vals,		# cumulative plot
#		x=deg.vals, y=vals,			# non-cumulative plot
	xlab=TeX(xlab), ylab=TeX(ylab),
	col=pal[1], 
	log="xy", 
	las=1
)
# fitted line
threshold <- max(deg.vals)
x <- seq(from=min(deg.vals), to=threshold, by=(threshold-min(deg.vals))/100)
y <- predict(fit, list(cut.deg=x))
idx <- which(y>0 & y<=1)
lines(x[idx], y[idx], col="BLACK", lty=2)




################################################################################
# same thing for filtered net
t0 <- round(length(gs.filt)*0.5)
t1 <- t0+1 #round(length(gs)*0.40)
dt <- round(length(gs.filt)*0.2)
t2 <- min(t1+dt, length(gs.filt))

# retrieve vertex sets
v0 <- V(gs.filt[[t0]])$name
v1 <- V(gs.filt[[t1]])$name
v2 <- V(gs.filt[[t2]])$name
# new vertices in the [t1,t1+dt] period
dv <- union(setdiff(v1, v0), setdiff(v2, v1))
# their neighbors
nei <- names(unlist(unname(adjacent_vertices(graph=gs.filt[[t2]], v=dv, mode="all"))))
# keep only vertices existing at t0
nei <- nei[nei %in% v0]
idx <- match(nei, v0)
norm <- length(idx)

# compute cumulative distribution
deg0 <- igraph::degree(graph=gs.filt[[t0]], mode="all")
tt <- table(deg0[idx])
deg.vals <- as.integer(names(tt))
norms <- sapply(deg.vals, function(d) length(which(deg0==d)))
#vals <- tt/norm									# Barabasi's version
#vals <- tt/norm*gorder(gs.filt[[t0]])/norms				# Newman's version
vals <- tt/norms/sum(tt/norms)						# alt Barabasi's version
cum.vals <- cumsum(vals)
	
## compute degree changes (alt, but equivalent to above block)
#tt <- table(idx)
#idx <- as.integer(names(tt))
## compute degree and merge same degree values
#deg0 <- igraph::degree(graph=gs.filt[[t0]], v=idx, mode="all")
#deg.vals <- sort(unique(deg0))
#vals <- sapply(deg.vals, function(d) sum(tt[which(deg0==d)]))
## compute the cumulative values
#cum.vals <- cumsum(vals/norm)
	
## compute degree changes (alt 2, slightly different from above block, distinguish all vertices as Olesen et al. do)
#tt <- table(factor(nei, levels=v0))
## compute degree
#deg0 <- igraph::degree(graph=gs.filt[[t0]], mode="all")
#ord.idx <- order(deg0)
#deg.vals <- deg0[ord.idx]
#vals <- tt[ord.idx]/norm
## compute the cumulative values
#cum.vals <- cumsum(vals)
## clean values
#idx <- which(cum.vals>0 & deg.vals>0)
#deg.vals <- deg.vals[idx]
#cum.vals <- cum.vals[idx]

# plot
#plot(x=deg.vals, y=cum.vals, log="xy", xlab=TeX(xlab), ylab=TeX(ylab))

# try with various thresholds
thresholds <- quantile(deg.vals, probs=c(1,0.75,0.50,0.25))
threshold <- thresholds[2]	# exp=1+0.65

# only keep the left tail
cut.cum <- cum.vals[deg.vals<=threshold]
cut.deg <- deg.vals[deg.vals<=threshold]

# init parameters using a linear regression
fit <- lm(log(cut.cum) ~ log(cut.deg))
print(summary(fit))
params <- fit$coefficients
val1 <- exp(params[1]); names(val1) <- NULL
val2 <- params[2]; names(val2) <- NULL
val3 <- 0

# perform NL regression
df <- data.frame(cut.deg, cut.cum)
fit <-nlsLM(cut.cum ~ c1*cut.deg^c2, 
			start=list(c1=val1, c2=val2),
			data=df,
			control=list(maxiter=200))
print(summary(fit))
	
# plot results
par(
	fig=c(0.45,0.98, 0.05, 0.62), 
	new=T,
	mgp=c(3,0.5,0)
)
# plot cumulative values
plot(
	x=deg.vals, y=cum.vals,		# cumulative plot
#	x=deg.vals, y=vals,			# non-cumulative plot
	xlab=NA, ylab=NA,
	col=pal[2], 
	log="xy", 
	las=1
)
# fitted line
threshold <- max(deg.vals)
x <- seq(from=min(deg.vals), to=threshold, by=(threshold-min(deg.vals))/100)
y <- predict(fit, list(cut.deg=x))
idx <- which(y>0 & y<=1)
lines(x[idx], y[idx], col="BLACK", lty=2)
dev.off()




###############################################################################
# try to plot the evolution of the exponent (very unstable, not sure why)

# loop over t0
t0s <- round(seq(from=length(gs)*0.50, to=length(gs)*0.80, by=length(gs)*0.05))
expos <- c()
for(t0 in t0s)
{	# set up periods
	t1 <- t0+1 #round(length(gs)*0.40)
	dt <- round(length(gs)*0.2)
	t2 <- min(t1+dt, length(gs))
	tlog(4,"Processing t0=",t0," vs. period [",t1,";",t2,"]")
	
	# retrieve vertex sets
	v0 <- V(gs[[t0]])$name
	v1 <- V(gs[[t1]])$name
	v2 <- V(gs[[t2]])$name
	# new vertices in the [t1,t1+dt] period
	dv <- union(setdiff(v1, v0), setdiff(v2, v1))
	# their neighbors
	nei <- names(unlist(unname(adjacent_vertices(graph=gs[[t2]], v=dv, mode="all"))))
	# keep only vertices existing at t0
	nei <- nei[nei %in% v0]
	idx <- match(nei, v0)
	norm <- length(idx)
	
	# compute cumulative distribution
	deg0 <- igraph::degree(graph=gs[[t0]], mode="all")
	tt <- table(deg0[idx])
	deg.vals <- as.integer(names(tt))
	norms <- sapply(deg.vals, function(d) length(which(deg0==d)))
	vals <- tt/norms/sum(tt/norms)						# alt Barabasi's version
	cum.vals <- cumsum(vals)
	
	# make a few tries
	thresholds <- quantile(deg.vals, probs=c(1,0.75,0.50,0.25))
	best.exp <- NA
	best.r2 <- 0
	for(i in 1:length(thresholds))
	{	threshold <- thresholds[i]
		tlog(6,"Threshold=",thresholds[i]," (",i,"/",length(thresholds),")")
		
		# only keep the left tail
		cut.cum <- cum.vals[deg.vals<=threshold]
		cut.deg <- deg.vals[deg.vals<=threshold]
		# build data frame
		df <- data.frame(cut.deg, cut.cum)
	
		# init parameters using a linear regression
		fit <- lm(log(cut.cum) ~ log(cut.deg))
		#print(summary(fit))
		params <- fit$coefficients
		val1 <- exp(params[1]); names(val1) <- NULL
		val2 <- params[2]; names(val2) <- NULL
		val3 <- 0
		
		# perform NL regression
		fit <- NA
		iter <- 0
		while(all(is.na(fit)) && iter<5)
		{	fit <- tryCatch(
					expr=nlsLM(cut.cum ~ c1*cut.deg^c2, 
							start=list(c1=val1, c2=val2),
							data=df,
							control=list(maxiter=200)),
					error=function(e) NA)
			if(all(is.na(fit)))
			{	iter <- iter + 1
				val1 <- runif(1,-5,5)
				val2 <- runif(1,-5,5)
				val3 <- runif(1,-5,5)
			}
		}
		
		if(!all(is.na(fit)))
		{	# retrieve estimated exponent and GoF
			exponent <- summary(fit)$coefficients["c2","Estimate"]
			r2 <- cor(cut.cum,predict(fit))^2 # according to https://stats.stackexchange.com/a/472444/26173
			tlog(8,"exp=",exponent," -- pseudoR2=",r2)
			
			# keep best fit
			if(r2>best.r2)
			{	best.exp <- exponent
				best.r2 <- r2
			}
		}
	}
	
	# store PL exponent
	tlog(5,"best: exp=",best.exp," -- pseudoR2=",best.r2)
	expos <- c(expos, best.exp)
}
# plot evolution of exponent
plot(
	x=t0s, y=expos-1,
	ylim=c(0,3),
	xlab="Scene", ylab=TeX("Exponent $\\alpha$"),
	las=1, col=pal[1],
	type="l", #type="o", pch=20
)




###############################################################################
# end logging
end.rec.log()





################################################################################
## checking with the data of Olesen et al.
#degs <- c(1,1,2,2,2,3,3,3,4,5,5,5,6,6)
#deg.vals <- sort(unique(degs))
#pis <- c(0,0.11,0,0,0.11,0.11,0,0.11,0,0.11,0.22,0.11,0,0.11)
#vals <- sapply(deg.vals, function(d) sum(pis[which(degs==d)]))
#cum.vals <- cumsum(vals)
#plot(x=deg.vals, y=cum.vals, log="xy")
################################################################################
## same with the graphs: seems ok too
#gs <- list()
#gs[[1]] <- sample_degseq(out.deg=c(1,1,2,2,2,3,3,3,4,5,5,5,6,6), method="vl")
#V(gs[[1]])$name <- paste0("V",1:gorder(gs[[1]]))
##plot(gs[[1]])
#gs[[2]] <- add_vertices(graph=gs[[1]], nv=1)
#gs[[2]] <- add_edges(graph=gs[[2]], edges=c(2,15,5,15,6,15,8,15,10,15,11,15,12,15,14,15))
#V(gs[[2]])$name <- paste0("V",1:gorder(gs[[2]]))
#gs[[3]] <- add_vertices(graph=gs[[2]], nv=1)
#gs[[3]] <- add_edges(graph=gs[[3]], edges=c(11,16))
#V(gs[[3]])$name <- paste0("V",1:gorder(gs[[3]]))
#t0 <- 1; t1 <- 2; t2 <- 3

################################################################################
## checking with the Barabasi-Albert model
################################################################################
## igraph BA model
#gs <- list()
#gs[[1]] <- barabasi.game(n=1000, m=3, directed=FALSE)
#V(gs[[1]])$name <- paste0("V",1:gorder(gs[[1]]))
#gs[[2]] <- barabasi.game(n=1001, m=3, directed=FALSE, start.graph=gs[[1]])
#V(gs[[2]])$name <- paste0("V",1:gorder(gs[[2]]))
#gs[[3]] <- barabasi.game(n=1030, m=3, directed=FALSE, start.graph=gs[[2]])
#V(gs[[3]])$name <- paste0("V",1:gorder(gs[[3]]))
#t0 <- 1; t1 <- 2; t2 <- 3
################################################################################
## custom BA model, just for verification
#build.ba <- function(dn, m, g)
#{	for(i in 1:dn)
#	{	ps <- degree(g)/sum(degree(g))
#		u <- sample(x=1:gorder(g),size=m,prob=ps)
#		g <- add.vertices(graph=g, nv=1, attr=list(name=paste0("V",gorder(g)+1)))
#		edges <- c(rbind(rep(gorder(g),length(u)),u))
#		g <- add.edges(graph=g, edges=edges)
#	}
#	return(g)
#}
#gs <- list()
#gs[[1]] <- barabasi.game(n=1000, m=3, directed=FALSE)
#V(gs[[1]])$name <- paste0("V",1:gorder(gs[[1]]))
#gs[[2]] <- build.ba(dn=1, m=3, g=gs[[1]])
#V(gs[[2]])$name <- paste0("V",1:gorder(gs[[2]]))
#gs[[3]] <- build.ba(dn=30, m=3, g=gs[[2]])
#V(gs[[3]])$name <- paste0("V",1:gorder(gs[[3]]))
#t0 <- 1; t1 <- 2; t2 <- 3
