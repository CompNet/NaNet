#############################################################################################
# Functions used to handle the computating and plotting of various topological properties.
# 
# 11/2021 Vincent Labatut
#
# source("src/graphs/topomeas.R")
#############################################################################################




###############################################################################
# Estimates the exponent of the power law relation between the degree and the
# local transitivity.
#
# g: graph to compute.
# weights: whether to use weights (if any).
# filename: path and base name of the generated files.
# col: main color used in the plot.
#
# returns: result of the estimation and goodness of fit.
###############################################################################
transitivity.vs.degree <- function(g, weights=FALSE, filename, col=MAIN_COLOR)
{	# NOTE: not clear whether the transitivity values should be averaged by degree first
	# some papers seem to do it, some other do not...
	
	col.sec <- combine.colors(col, "WHITE", transparency=20)
	
	# compute the values
	tra.vals <- transitivity(graph=g, type="local", weights=NA, isolates="zero")
	if(weights)
	{	deg.vals <- strength(g, mode="all")
		xlab <- "Strength $s$"
		base.name <- paste0(filename,"_trans_vs_strength")
	}
	else
	{	deg.vals <- degree(g, mode="all")
		xlab <- "Degree $k$"
		base.name <- paste0(filename,"_trans_vs_degree")
	}
	
	# filter out zero transitivity
	filt.tra <- tra.vals[tra.vals>0]
	filt.deg <- deg.vals[tra.vals>0]
	avg.tra <- sapply(1:max(filt.deg), function(d) mean(filt.tra[filt.deg==d]))
	
	# init result table
	thresholds <- quantile(filt.deg, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
	cn <- c("Exponent","sigma","pseudoR2")
	tab <- matrix(NA, nrow=length(thresholds), ncol=length(cn))
	rownames(tab) <- c("100%","Top 75%","Top 50%","Top 25%", "Top 15%", "Top 10%", "Top 5%")
	colnames(tab) <- cn
	
	# make a few tries
	best.exp <- NA
	best.sigma <- NA
	best.r2 <- .Machine$integer.max
	for(i in 1:length(thresholds))
	{	threshold <- thresholds[i]
		
		# only keep the right tail?
		cut.tra <- filt.tra[filt.deg>=threshold]
		cut.deg <- filt.deg[filt.deg>=threshold]
		# build data frame
		df <- data.frame(cut.deg, cut.tra)
		
		# init parameters using a linear regression
		fit <- lm(log(cut.tra) ~ log(cut.deg))
		#summary(fit)
		params <- fit$coefficients
		val1 <- exp(params[1]); names(val1) <- NULL
		val2 <- params[2]; names(val2) <- NULL
		val3 <- 0
		
		# fit model
		fit <- NA
		iter <- 0
		while(all(is.na(fit)) && iter<5)
		{	fit <- tryCatch(
				expr=nlsLM(cut.tra ~ c1*cut.deg^c2,		# nlsLM(cut.tra ~ c1*cut.deg^c2 + c3, 
					start=list(c1=val1, c2=val2),		# start=list(c1=val1, c2=val2, c3=val3),
					data = df,
					control=list(maxiter=75)),
				error=function(e) NA)
			if(all(is.na(fit)))
			{	iter <- iter + 1
				val1 <- runif(1,-5,5)
				val2 <- runif(1,-5,5)
				val3 <- runif(1,-5,5)
			}
		}
		
		if(!all(is.na(fit)))
		{	# retrieve estimated exponent
			exponent <- tab[i,"Exponent"] <- summary(fit)$coefficients["c2","Estimate"]
			r2 <- tab[i,"pseudoR2"] <- cor(cut.tra,predict(fit))^2 # according to https://stats.stackexchange.com/a/472444/26173
			sigma <- tab[i,"sigma"] <- summary(fit)$sigma	# square root of the estimated variance of the random error
			#plot(fit) # plot residuals
			
			# plot for future visualization
			plot.file <- paste0(base.name,"_thre=",threshold)
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
				{	pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
					par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
					main.title <- NA
				}
				else if(fformat==PLOT_FORMAT_PNG)
				{	png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					main.title <- paste0("[",rownames(tab)[i],"] Exponent: ",exponent," -- pseudoR2: ",r2)
				}
					# data
					plot(
						x=filt.deg, y=filt.tra, 
						main=main.title,
						xlab=TeX(xlab),
						ylab=TeX("Local Transitivity $C(v)$"),
						log="xy",
						col=col.sec,
						xlim=c(1,max(deg.vals)*1.1)
						#ylim=c(0.0001,1)
					)
					# mean
					idx <- which(avg.tra>0)
					lines(
						x=idx, y=avg.tra[idx],
						col=col
					)
					# fitted line
					x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
					lines(x, predict(fit, list(cut.deg=x)), col="BLACK", lty=2)
					# legend
					legend(
						x="topright",
						lty=1:2, col=c(col,"BLACK"),
						legend=c("Mean","Fit")
					)
				dev.off()
			}
			
			# keep best fit
			if(r2>best.r2)
			{	best.exp <- exponent
				best.sigma <- sigma
				best.r2 <- r2
			}
		}
	}
	
	# record results
	tab.file <- paste0(base.name,".csv")
	write.csv(x=tab, file=tab.file, row.names=TRUE)#, col.names=TRUE)
	
	result <- list(exponent=best.exp, sigma=best.sigma, r2=best.r2)
	return(result)
}




###############################################################################
# Estimates the exponent of the power law relation between the degree and the
# neighbors' average degree.
#
# g: graph to compute.
# weights: whether to use weights (if any).
# filename: path and base name of the generated files.
# col: main color used in the plot.
#
# returns: result of the estimation and goodness of fit.
###############################################################################
neigh.degree.vs.degree <- function(g, weights=FALSE, filename, col=MAIN_COLOR)
{	col.sec <- combine.colors(col, "WHITE", transparency=20)
	
	# compute the values
	if(weights)
	{	deg.vals <- strength(graph=g, mode="all")
		tmp <- igraph::knn(graph=g)#, mode="all", neighbor.degree.mode="all")
		xlab <- "Strength $s$"
		ylab <- "Neighbors' average Strength $<s_{nn}>$"
		base.name <- paste0(filename,"_neideg_vs_strength")
	}
	else
	{	deg.vals <- degree(graph=g, mode="all")
		tmp <- igraph::knn(graph=g, weights=NULL)#, mode="all", neighbor.degree.mode="all")
		xlab <- "Degree $k$"
		ylab <- "Neighbors' average Degree $<k_{nn}>$"
		base.name <- paste0(filename,"_neideg_vs_degree")
	}
	
	# filter out zero degree and NaN
	idx <- which(!is.nan(tmp$knn) & tmp$knn>0)
	filt.nei <- tmp$knn[idx]
	filt.deg <- deg.vals[idx]
	
	# init result table
	thresholds <- quantile(filt.deg, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
	cn <- c("Exponent","sigma","pseudoR2")
	tab <- matrix(NA, nrow=length(thresholds), ncol=length(cn))
	rownames(tab) <- c("100%","Top 75%","Top 50%","Top 25%", "Top 15%", "Top 10%", "Top 5%")
	colnames(tab) <- cn
	
	# make a few tries
	best.exp <- NA
	best.sigma <- NA
	best.r2 <- .Machine$integer.max
	for(i in 1:length(thresholds))
	{	threshold <- thresholds[i]
		
		# only keep the right tail?
		cut.nei <- filt.nei[filt.deg>=threshold]
		cut.deg <- filt.deg[filt.deg>=threshold]
		# build data frame
		df <- data.frame(cut.deg, cut.nei)
		
		# init parameters using a linear regression
		fit <- lm(log(cut.nei) ~ log(cut.deg))
		#summary(fit)
		params <- fit$coefficients
		val1 <- exp(params[1]); names(val1) <- NULL
		val2 <- params[2]; names(val2) <- NULL
		val3 <- 0
		
		# fit model
		fit <- NA
		iter <- 0
		while(all(is.na(fit)) && iter<5)
		{	fit <- tryCatch(
				expr=nlsLM(cut.nei ~ c1*cut.deg^c2,	# expr=nlsLM(cut.nei ~ c1*cut.deg^c2 + c3, 
					start=list(c1=val1, c2=val2),	# start=list(c1=val1, c2=val2, c3=val3),
					data = df,
					control=list(maxiter=75)),
				error=function(e) NA)
			if(all(is.na(fit)))
			{	iter <- iter + 1
				val1 <- runif(1,-5,5)
				val2 <- runif(1,-5,5)
				val3 <- runif(1,-5,5)
			}
		}
		
		if(!all(is.na(fit)))
		{	# retrieve estimated exponent
			exponent <- tab[i,"Exponent"] <- summary(fit)$coefficients["c2","Estimate"]
			r2 <- tab[i,"pseudoR2"] <- cor(cut.nei,predict(fit))^2 # according to https://stats.stackexchange.com/a/472444/26173
			sigma <- tab[i,"sigma"] <- summary(fit)$sigma	# square root of the estimated variance of the random error
			#plot(fit) # plot residuals
		
			# plot for future visualization
			plot.file <- paste0(base.name,"_thre=",threshold)
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
				{	pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
					par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
					main.title <- NA
				}
				else if(fformat==PLOT_FORMAT_PNG)
				{	png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
					main.title <- paste0("[",rownames(tab)[i],"] Exponent: ",exponent," -- pseudoR2: ",r2)
				}
					# points
					plot(
						x=filt.deg, y=filt.nei, 
						main=main.title,
						xlab=TeX(xlab), 
						ylab=TeX(ylab),
						log="xy", 
						las=1, col=col.sec,
						xlim=c(1,max(deg.vals)*1.1)
					)
					# mean
					idx <- which(!is.nan(tmp$knnk) & tmp$knnk>0)
					lines(
						x=idx, y=tmp$knnk[idx],
						col=col
					)
					# fitted line
					x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
					lines(x, predict(fit, list(cut.deg=x)), col="BLACK", lty=2)
					# legend
					legend(
						x="topright",
						lty=1:2, col=c(col,"BLACK"),
						legend=c("Mean","Fit")
					)
				dev.off()
			}
			
			# keep best fit
			if(r2<best.r2)
			{	best.exp <- exponent
				best.sigma <- sigma
				best.r2 <- r2
			}
		}
	}
	
	# record results
	tab.file <- paste0(base.name,".csv")
	write.csv(x=tab, file=tab.file, row.names=TRUE)#, col.names=TRUE)
	
	result <- list(exponent=best.exp, sigma=best.sigma, r2=best.r2)
	return(result)
}




###############################################################################
# Creates the hop-plot of the specified graph, i.e. proportion of nodes
# within a distance as a function of this distance (for each node).
# For more details, see:
#		R. Pastor-Satorras and A. Vespignani, 
#		Evolution and structure of the Internet: a statistical physics approach. 
#		Cambridge University Press, 2004.
#
# Note: isolates are not plotted because of the logarithmic y-axis, but they are 
# taken into account in the average.
#
# g: graph to compute.
# weights: whether to use weights (if any) to compute distances.
# filename: path and base name of the generated files.
# col: main color used in the plot.
#
# returns: result of the estimation and goodness of fit.
###############################################################################
hop.plot <- function(g, weights=FALSE, filename, col=MAIN_COLOR)
{	base.file <- paste0(filename,"_neisize_vs_distance")
	col.sec <- combine.colors(col, "WHITE", transparency=20)
	
	# compute required distances
	n <- gorder(g)
	if(weights)
	{	dd <- distances(graph=g, mode="all", weights=E(g)$weight)
		xlab <- "Weighted Distance $d$"
	}
	else
	{	dd <- distances(graph=g, mode="all", weights=NULL)
		xlab <- "Distance $d$"
	}
	tmp <- c(dd)
	dist.vals <- sort(unique(tmp[!is.infinite(tmp) & tmp>0]))
	hp.vals <- sapply(dist.vals, function(d) 
				sapply(1:nrow(dd), function(u) length(which(dd[u,-u]<=d))/(n-1)))
	hp.avg <- apply(hp.vals, 2, function(vect) mean(vect, na.rm=TRUE))
	hp.vals[hp.vals==0] <- NA
	
	# axes ranges
	xl <- range(dist.vals, na.rm=TRUE)
	yl <- c(min(c(hp.vals), na.rm=TRUE), 1)
	
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(base.file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(base.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			# points
			plot(NULL,
				xlim=xl,
				ylim=yl,
				xlab=TeX(xlab), 
				ylab=TeX("Proportion of vertices"),
				log="y", yaxt="n",
				col=col.sec
			)
			for(u in 1:n)
				points(x=dist.vals, y=hp.vals[u,], col="PINK")
			# render the y-axis
			expmax <- floor(log(min(yl[1]),10))
			axis(
				side=2, 
				at=10^(expmax:0), 
				label=parse(text=paste("10^", expmax:0, sep="")), 
				las=1
			)
			# mean
			lines(
				x=dist.vals, y=hp.avg,
				col=col
			)
		dev.off()
	}
}
