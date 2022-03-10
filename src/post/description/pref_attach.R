# Computes and plots the preferential attachment descriptor from:
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
# Note: I am not completely sure of this implementation, because I
# am not sure I understood correctly the description of this method. 
# According to Jeong et al.'s paper : 
#   "Consider all nodes existing in the system at time T0, called “T0 nodes”. 
#   Next select a group of “T1 nodes”, added between [T1, T1+ΔT], where 
#   ΔT << T1 and T1 > T0. When a T1 node joins the system we record the 
#   degree k of the T0 node to which the new node links. The histogram 
#   providing the number of links acquired by the T0 nodes with exactly 
#   k degree, after normalization, gives [...] the Π(k) preferential 
#   attachment function. As we are forced to use relatively short ΔT 
#   intervals, even for large networks with hundreds of thousands of nodes 
#   Π(k) has significant fluctuations, particularly for large k. To reduce 
#   the noise level, instead of Π(k) we study the cumulative function."
# My issue was the normalization bit. I experimented with two alternatives,
# and kept the one that gives results with the Barabasi-Albert which are
# consistent with the theory. The selected normalization was found in 
# Newman's paper.
# 
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

# load full graph to get filtered characters
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
filt.names <- V(g)$name[V(g)$Filtered]
if(length(filt.names)==0) stop("Empty list of filtered characters")

# load raw data
tlog(0,"Extract the sequence of scene-related cumulative graphs")
data <- read.raw.data()
# compute the sequence of scene-based graphs (possibly one for each scene)
gs.unf <- extract.static.graph.scenes(
	inter.df=data$inter.df,
	char.stats=data$char.stats, 
	volume.stats=data$volume.stats, 
	ret.seq=TRUE
)
gs.filt <- future_lapply(gs.unf, function(g) delete_vertices(g, v=intersect(filt.names,V(g)$name)))
# build the list for latter use
gsl <- list()
gsl[[1]] <- gs.unf
gsl[[2]] <- gs.filt



################################################################################
filts <- c(FALSE, TRUE)
modes <- c("all", "external", "internal")

# NLS thresholds (set manually)
thres <- matrix(0, nrow=length(modes), ncol=length(filts))
rownames(thres) <- modes
thres["all",1] <- 0.65
thres["all",2] <- 0.58
thres["external",1] <- 0.60
thres["external",2] <- 0.57
thres["internal",1] <- 0.50
thres["internal",2] <- 0.55

# axis labels
xlab <- c()
xlab["all"] <- "Degree $k$"
xlab["external"] <- "Degree $k$"
xlab["internal"] <- "Degree product $k_1 k_2$"
ylab <- c()
ylab["all"] <- "Attachment probability $\\kappa(k)$"
ylab["external"] <- "Attachment probability $\\kappa(k)$"
ylab["internal"] <- "Attachment probability $\\kappa(k_1 k_2)$"

tlog(2,"Looping over modes (all, external, internal")
for(mode in modes)
{	tlog(2,"Processing mode ",mode)
	
	tlog(2,"Looping over unfiltered/filtered graphs")
	for(f in 1:length(filts))
	{	gs <- gsl[[f]]
		filtered <- if(filts[f]) "filtered" else "unfiltered"
		tlog(4,"Processing the ",filtered," graph")
		
		# focus on the middle of the period
		t0 <- round(length(gs)*0.5)
		t1 <- t0+1 #round(length(gs)*0.40)
		dt <- round(length(gs)*0.2)
		t2 <- min(t1+dt, length(gs))
		
		# retrieve vertex sets
		v0 <- V(gs[[t0]])$name		# point of reference
		v1 <- V(gs[[t1-1]])$name	# moment right before the start of the interval
		v2 <- V(gs[[t2]])$name		# end of the interval
		# new vertices in the [t1,t1+dt] period
		dv <- setdiff(v2, v1)
		
		####################################
		# compute stats
		if(mode=="all")
		{	# remove old edges and get remaining ones
			dg <- difference(big=gs[[t2]], small=gs[[t1-1]])
			el <- as_edgelist(dg, names=TRUE)
			nei <- c(el)
			# keep only vertices existing at t0
			nei <- nei[nei %in% v0]
			idx <- match(nei, v0)
			norm <- length(idx)
			tlog(6,"Total number of new edges: ",norm)
			# compute vals
			deg0 <- igraph::degree(graph=gs[[t0]], mode="all")
			#if(min(deg0)==0) deg0 <- deg0 + 1
			tt <- table(deg0[idx])
			deg.vals <- as.integer(names(tt))
			# compute normalization term
			norms <- sapply(deg.vals, function(d) length(which(deg0==d)))
		}
		else if(mode=="external")
		{	# neighbors of the new vertices
			nei <- names(unlist(unname(adjacent_vertices(graph=gs[[t2]], v=dv, mode="all"))))
			# keep only vertices existing at t0
			nei <- nei[nei %in% v0]
			idx <- match(nei, v0)
			norm <- length(idx)
			tlog(6,"Number of new external edges: ",norm)
			# compute vals
			deg0 <- igraph::degree(graph=gs[[t0]], mode="all")
			#if(min(deg0)==0) deg0 <- deg0 + 1
			tt <- table(deg0[idx])
			deg.vals <- as.integer(names(tt))
			# compute normalization term
			norms <- sapply(deg.vals, function(d) length(which(deg0==d)))
			
			## compute degree changes (alt, but equivalent to above block)
			#tt <- table(idx)
			#idx <- as.integer(names(tt))
			## compute degree and merge same degree values
			#deg0 <- igraph::degree(graph=gs[[t0]], v=idx, mode="all")
			#deg.vals <- sort(unique(deg0))
			#vals <- sapply(deg.vals, function(d) sum(tt[which(deg0==d)]))
			## compute the cumulative values
			#cum.vals <- cumsum(vals/norm)
			
			## compute degree changes (alt 2, slightly different from above block, distinguish all vertices in the cumulative function, as Olesen et al. do)
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
		}
		else if(mode=="internal")
		{	# remove old edges
			dg <- difference(big=gs[[t2]], small=gs[[t1-1]])
			# remove new vertices
			dg <- delete_vertices(graph=dg, v=dv)
			# get the remaining edges
			el <- as_edgelist(dg, names=TRUE)
			idx <- cbind(match(el[,1], V(gs[[t0]])$name), match(el[,2], V(gs[[t0]])$name))
			norm <- nrow(idx)
			tlog(6,"Number of new internal edges: ",norm)
			# compute vals
			deg0 <- igraph::degree(graph=gs[[t0]], mode="all")
			#if(min(deg0)==0) deg0 <- deg0 + 1
			tt <- table(deg0[idx[,1]]*deg0[idx[,2]])
			deg.vals <- as.integer(names(tt))
			# compute normalization term
			pairs <- t(combn(gorder(gs[[t0]]),2))
			exist <- sapply(1:nrow(pairs), function(i) length(which(idx[,1]==pairs[i,1] & idx[,2]==pairs[i,2]))>0)
			pairs <- pairs[!exist,]
			ttn <- table(deg0[pairs[,1]]*deg0[pairs[,2]])
			norms <- ttn[names(tt)]
		}
		
		# remove zero degree
		norms <- norms[deg.vals>0]
		tt <- tt[deg.vals>0]
		deg.vals <- deg.vals[deg.vals>0]
		
		# compute cumulative distribution
		#vals <- tt/norm*gorder(gs[[t0]])/norms				# Newman's version
		#vals <- tt/norm									# Barabasi's version?
		vals <- tt/norms/sum(tt/norms)						# or is this Barabasi's version?
		cum.vals <- cumsum(vals)
		
		####################################
		# fit power law
		
		# try with various thresholds
		#thresholds <- quantile(deg.vals, probs=c(1,0.75,0.50,0.25))
#		threshold <- quantile(deg.vals, probs=0.55)
		threshold <- quantile(deg.vals, thres[mode,f]) #, probs=0.6)
		#		 unfiltered      all 1 + 0.98
		#		   filtered      all 1 + 0.72
		#		 unfiltered external 1 + 0.99
		#		   filtered external 1 + 0.93
		#		 unfiltered internal 1 + 0.43
		#		   filtered internal 1 + 0.45
				
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
			
		####################################
		# plot results

		# create/complete figure
		if(!filts[f])
		{	# open plot file
			plot.file <- get.path.comparison.plot(object="nodes", mode="scenes", meas.name=MEAS_DEGREE, filtered=FALSE, plot.type=paste0("pref_attach_both_",mode))
			tlog(2,"Plot in file ",plot.file)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
			# plot parameters
			par(
				mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
				fig=c(0,1,0,1),		# set coordinate space of the original plot
				mgp=c(3,0.8,0)		# distance between axis ticks and values
			)
			zm <- 1
			# labels
			xl <- TeX(xlab[mode])
			yl <- TeX(xlab[mode])
		}
		else
		{	# plot parameters
			if(mode=="internal")
			{	par(
					fig=c(0.08, 0.59, 0.47, 0.98), 
					new=TRUE,
					mgp=c(3,0.5,0)
				)
			}
			else
			{	par(
					fig=c(0.50, 0.98, 0.05, 0.57), 
					new=TRUE,
					mgp=c(3,0.5,0)
				)
			}
			zm <- 0.75
			# labels
			xl <- NA
			yl <- NA
		}
		# plot cumulative values
		plot(
			x=deg.vals, y=cum.vals,		# cumulative plot
#			x=deg.vals, y=vals,			# non-cumulative plot
			xlab=xl, ylab=yl,
			col=pal[f], 
			log="xy", 
			las=1,
			cex.lab=zm, cex.axis=zm, cex=zm
		)
		# plot fitted line
		threshold <- max(deg.vals)
		x <- seq(from=min(deg.vals), to=threshold, by=(threshold-min(deg.vals))/100)
		y <- predict(fit, list(cut.deg=x))
		idx <- which(y>0 & y<=1)
		lines(x[idx], y[idx], col="BLACK", lty=2)
		
		# close plot file
		if(filts[f])
			dev.off()
		
	}
}




###############################################################################
# end logging
tlog(0,"Process complete")
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




################################################################################
## try to plot the evolution of the exponent (very unstable, not sure why)
#
## loop over t0
#t0s <- round(seq(from=length(gs)*0.50, to=length(gs)*0.80, by=length(gs)*0.05))
#expos <- c()
#for(t0 in t0s)
#{	# set up periods
#	t1 <- t0+1 #round(length(gs)*0.40)
#	dt <- round(length(gs)*0.2)
#	t2 <- min(t1+dt, length(gs))
#	tlog(4,"Processing t0=",t0," vs. period [",t1,";",t2,"]")
#	
#	# retrieve vertex sets
#	v0 <- V(gs[[t0]])$name		# point of reference
#	v1 <- V(gs[[t1-1]])$name	# moment right before the start of the interval
#	v2 <- V(gs[[t2]])$name		# end of the interval
#	# new vertices in the [t1,t1+dt] period
#	dv <- setdiff(v2, v1)
#	# their neighbors
#	nei <- names(unlist(unname(adjacent_vertices(graph=gs[[t2]], v=dv, mode="all"))))
#	# keep only vertices existing at t0
#	nei <- nei[nei %in% v0]
#	idx <- match(nei, v0)
#	norm <- length(idx)
#	
#	# compute cumulative distribution
#	deg0 <- igraph::degree(graph=gs[[t0]], mode="all")
#	tt <- table(deg0[idx])
#	deg.vals <- as.integer(names(tt))
#	norms <- sapply(deg.vals, function(d) length(which(deg0==d)))
#	vals <- tt/norms/sum(tt/norms)						# alt Barabasi's version
#	cum.vals <- cumsum(vals)
#	
#	# make a few tries
#	thresholds <- quantile(deg.vals, probs=c(1,0.75,0.50,0.25))
#	best.exp <- NA
#	best.r2 <- 0
#	for(i in 1:length(thresholds))
#	{	threshold <- thresholds[i]
#		tlog(6,"Threshold=",thresholds[i]," (",i,"/",length(thresholds),")")
#		
#		# only keep the left tail
#		cut.cum <- cum.vals[deg.vals<=threshold]
#		cut.deg <- deg.vals[deg.vals<=threshold]
#		# build data frame
#		df <- data.frame(cut.deg, cut.cum)
#	
#		# init parameters using a linear regression
#		fit <- lm(log(cut.cum) ~ log(cut.deg))
#		#print(summary(fit))
#		params <- fit$coefficients
#		val1 <- exp(params[1]); names(val1) <- NULL
#		val2 <- params[2]; names(val2) <- NULL
#		val3 <- 0
#		
#		# perform NL regression
#		fit <- NA
#		iter <- 0
#		while(all(is.na(fit)) && iter<5)
#		{	fit <- tryCatch(
#					expr=nlsLM(cut.cum ~ c1*cut.deg^c2, 
#							start=list(c1=val1, c2=val2),
#							data=df,
#							control=list(maxiter=200)),
#					error=function(e) NA)
#			if(all(is.na(fit)))
#			{	iter <- iter + 1
#				val1 <- runif(1,-5,5)
#				val2 <- runif(1,-5,5)
#				val3 <- runif(1,-5,5)
#			}
#		}
#		
#		if(!all(is.na(fit)))
#		{	# retrieve estimated exponent and GoF
#			exponent <- summary(fit)$coefficients["c2","Estimate"]
#			r2 <- cor(cut.cum,predict(fit))^2 # according to https://stats.stackexchange.com/a/472444/26173
#			tlog(8,"exp=",exponent," -- pseudoR2=",r2)
#			
#			# keep best fit
#			if(r2>best.r2)
#			{	best.exp <- exponent
#				best.r2 <- r2
#			}
#		}
#	}
#	
#	# store PL exponent
#	tlog(5,"best: exp=",best.exp," -- pseudoR2=",best.r2)
#	expos <- c(expos, best.exp)
#}
## plot evolution of exponent
#plot(
#	x=t0s, y=expos-1,
#	ylim=c(0,3),
#	xlab="Scene", ylab=TeX("Exponent $\\alpha$"),
#	las=1, col=pal[1],
#	type="l", #type="o", pch=20
#)
