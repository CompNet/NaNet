# Ad hoc plots regarding power-law relationship between:
# - neighbors' average degree vs. degree
# - cluster coefficient vs. degree
# 
# Vincent Labatut
# 01/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/pl_rel.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="PlRel")




# read the graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
# clean names
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)

# compute values
deg.vals <- degree(graph=g, mode="all")
tmp <- igraph::knn(graph=g, weights=NULL)#, mode="all", neighbor.degree.mode="all")

# filter out zero degree and NaN
idx <- which(!is.nan(tmp$knn) & tmp$knn>0)
filt.nei <- tmp$knn[idx]
filt.deg <- deg.vals[idx]

# init parameters using a linear regression
fit <- lm(log(filt.nei) ~ log(filt.deg))
summary(fit)
params <- fit$coefficients
val1 <- exp(params[1]); names(val1) <- NULL
val2 <- params[2]; names(val2) <- NULL
val3 <- 0

# perform NL regression
df <- data.frame(filt.deg, filt.nei)
fit <- nlsLM(filt.nei ~ c1*filt.deg^c2 + c3, 
		start=list(c1=val1, c2=val2, c3=val3),
		data = df,
		control=list(maxiter=75))
summary(fit)

# plot
xlab <- "Degree $k$"
ylab <- "Neighbors' average Degree $<k_{nn}>$"
exponent <- summary(fit)$coefficients["c2","Estimate"]
# points
plot(
	x=filt.deg, y=filt.nei, 
	main=paste0("[",rownames(tab)[i],"] Exponent: ",exponent),
	xlab=TeX(xlab), 
	ylab=TeX(ylab),
	log="xy", 
	las=1, col="PINK",
	xlim=c(1,max(deg.vals)*1.1)
)
# mean
idx <- which(!is.nan(tmp$knnk) & tmp$knnk>0)
lines(	
	x=idx, y=tmp$knnk[idx],
	col="RED"
)
# fitted line
threshold <- min(filt.deg)
x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
lines(x, predict(fit, list(filt.deg=x)), col="BLACK", lty=2)
lines(x, 1000*x^-0.5 + summary(fit)$coefficients["c3","Estimate"], col="BLACK", lty=2)
# legend
legend(
	x="topright",
	lty=1:2, col=c("RED","BLACK"),
	legend=c("Mean","Fit")
)


###############################################################################
# distribution plots
tlog(0,"Producing plots")

# loop params
meass <- c("degree","strength")

# process each measure
for(meas in meass)
{	tlog(2,"Dealing with measure ",meas)
	wts <- NA
	if(meas=="strength")
		wts <- c("duration","occurrences")
	
	for(wt in wts)
	{	if(!is.na(wt)) 
			tlog(4,"Dealing with weight ",wt)
		
		# load precomputed data
		data <- list()
		# unfiltered
		file <- get.path.stat.table(object="nodes", mode="scenes", weights=if(is.na(wt)) "occurrences" else wt, filtered=FALSE)
		tab <- as.matrix(read.csv(file, header=TRUE, check.names=FALSE, row.names=1))
		data[[1]] <- tab[,meas]
		data[[1]] <- data[[1]][data[[1]]>0]	# remove isolates
#		file <- get.path.comparison.plot(object="nodes", mode="scenes", meas.name=meas, weights=if(is.na(wt)) "none" else wt, filtered=FALSE, plot.type="disttest_noisolates")
#		test.disc.distr(data[[1]], xlab=paste0("Unfiltered ",ALL_MEASURES[[meas]]$cname," (no isolates)"), return_stats=FALSE, sims=100, plot.file=file)
		# filtered
		file <- get.path.stat.table(object="nodes", mode="scenes", weights=if(is.na(wt)) "occurrences" else wt, filtered=TRUE)
		tab <- as.matrix(read.csv(file, header=TRUE, check.names=FALSE, row.names=1))
		data[[2]] <- tab[,meas]
		data[[2]] <- data[[2]][data[[2]]>1]	# remove isolates
		names(data) <- c("Unfiltered","Filtered")
#		file <- get.path.comparison.plot(object="nodes", mode="scenes", meas.name=meas, weights=if(is.na(wt)) "none" else wt, filtered=TRUE, plot.type="disttest_noisolates")
#		test.disc.distr(data[[2]], xlab=paste0("Unfiltered ",ALL_MEASURES[[meas]]$cname," (no isolates)"), return_stats=FALSE, sims=100, plot.file=file)
		
		# set params
		file <- get.path.comparison.plot(object="nodes", mode="scenes", meas.name=meas, weights=if(is.na(wt)) "none" else wt, filtered=FALSE, plot.type="both_distrib")
		pal <- get.palette(length(data))
		ml <- paste0(ALL_MEASURES[[meas]]$cname, " distribution")
		if(!is.na(wt))
			ml <- paste0(ml," (",wt,")")
		xl <- paste0(ALL_MEASURES[[meas]]$cname)
		if(!is.na(wt))
			xl <- paste0(xl," (",wt,")")
	
		# check distribution
		pl <- list()
		for(i in 1:length(data))
		{	power.law <- displ$new(data[[i]])
			est <- estimate_xmin(power.law)
			tmp <- power.law$setXmin(est)
			if(laws[paste0(names(data)[i],"-",meas,if(!is.na(wt)) paste0("-",wt) else "")]=="truncated")
				pl[[i]] <- discpowerexp.fit(x=data[[i]],threshold=power.law$xmin)
			else if(laws[paste0(names(data)[i],"-",meas,if(!is.na(wt)) paste0("-",wt) else "")]=="good")
				pl[[i]] <- power.law
			else
				pl[[i]] <- NA
		}
		print(pl)
		
		# plot distributions
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
			par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
			plot.ccdf(data=data, main=NA, xlab=xl, log=TRUE, cols=pal, leg.title="Characters")
			for(i in 1:2)
			{	if(laws[paste0(names(data)[i],"-",meas,if(!is.na(wt)) paste0("-",wt) else "")]=="truncated")
				{	x <- seq(pl[[2]]$threshold,max(data[[2]]))
					y <- 1 - cumsum(ddiscpowerexp(x=x,exponent=pl[[2]]$exponent,rate=pl[[2]]$rate,threshold=pl[[2]]$threshold))
					lines(x, y, col="BLACK", lty=2)
				}
				else if(laws[paste0(names(data)[i],"-",meas,if(!is.na(wt)) paste0("-",wt) else "")]=="good")
					lines(pl[[i]], col="BLACK", lty=2)
			}
			dev.off()
		}
	}
}




###############################################################################
# end logging
end.rec.log()
