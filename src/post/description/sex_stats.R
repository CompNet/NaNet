# Additional plots regarding the sex attribute.
# 
# Vincent Labatut
# 02/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/sex_stats.R.R")
###############################################################################
source("src/common/include.R")
start.rec.log(text="SexStats")




###############################################################################
tlog(0,"Producing sex-separated nodal measure distribution plots")

# plot parameters
pal <- SEX_COLORS_4

# read the graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
# clean names
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
males <- which(V(g)$Sex=="Male" | V(g)$Sex=="Mixed")
females <- which(V(g)$Sex=="Female" | V(g)$Sex=="Mixed")

# measure names
meas.names <- c("degree", "betweenness", "closeness", "eigenvector")

# inlay position
inlay.coords <- matrix(nrow=length(meas.names), ncol=4)
rownames(inlay.coords) <- meas.names
inlay.coords["betweenness",] <- c(0.06, 0.71, 0.05, 0.70)
inlay.coords["closeness",] <- c(0.06, 0.71, 0.05, 0.70)
inlay.coords["degree",] <- c(0.06, 0.59, 0.05, 0.58)
inlay.coords["eigenvector",] <- c(0.06, 0.86, 0.05, 0.85)

# process each measure
for(m in 1:length(meas.names))
{	meas.name <- meas.names[m]
	tlog(2,"Dealing with measure ",meas.name," (",m,"/",length(meas.names),")")
	
	# load precomputed data for unfiltered net
	tlog(4,"Loading pre-computed unfiltered values")
	vals <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=meas.name, filtered=FALSE)
	data.unf <- list()
	data.unf[["Males"]] <- vals[males]
	data.unf[["Females"]] <- vals[females]
	tlog(6,"Average ",meas.name,": ",mean(data.unf[["Males"]],na.rm=TRUE)," (males) vs. ",mean(data.unf[["Females"]],na.rm=TRUE)," (females)")

	# load precomputed data for filtered net
	tlog(4,"Loading pre-computed filtered values")
	vals <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=meas.name, filtered=TRUE)
	data.flt <- list()
	data.flt[["Males"]] <- vals[males]
	data.flt[["Females"]] <- vals[females]
	tlog(6,"Average ",meas.name,": ",mean(data.unf[["Males"]],na.rm=TRUE)," (males) vs. ",mean(data.unf[["Females"]],na.rm=TRUE)," (females)")
	
	# set params
	plot.file <- get.path.topomeas.plot(object="nodes", mode="scenes", meas.name=paste0(meas.name,"_sex_both"), filtered=FALSE)
	#ml <- paste0(ALL_MEASURES[[meas.name]]$cname, " distribution")
	xl <- paste0(ALL_MEASURES[[meas.name]]$cname)
	
#		# check distribution
#		pl <- list()
#		for(i in 1:length(data))
#		{	power.law <- displ$new(data[[i]])
#			est <- estimate_xmin(power.law)
#			tmp <- power.law$setXmin(est)
#			if(laws[paste0(names(data)[i],"-",meas,if(!is.na(wt)) paste0("-",wt) else "")]=="truncated")
#				pl[[i]] <- discpowerexp.fit(x=data[[i]],threshold=power.law$xmin)
#			else if(laws[paste0(names(data)[i],"-",meas,if(!is.na(wt)) paste0("-",wt) else "")]=="good")
#				pl[[i]] <- power.law
#			else
#				pl[[i]] <- NA
#		}
#		print(pl)
		
	# plot distributions
	tlog(4, "Plotting in file ",plot.file)
	for(fformat in PLOT_FORMAT)
	{	if(fformat==PLOT_FORMAT_PDF)
			pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
		else if(fformat==PLOT_FORMAT_PNG)
			png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
		
		# plot unfiltered data
		par(
			mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
			fig=c(0,1,0,1),		# set coordinate space of the original plot
			mgp=c(3,1,0)		# distance between axis ticks and values
		)
		plot.ccdf(
				data=data.unf, 
				main=NA, xlab=xl, ylab="default", 
				log=TRUE, 
				cols=pal[1:2], 
				leg.title=if(meas.name=="eigenvector") NA else "Sex"
		)
#		for(i in 1:2)
#		{	if(laws[paste0(names(data)[i],"-",meas,if(!is.na(wt)) paste0("-",wt) else "")]=="truncated")
#			{	x <- seq(pl[[2]]$threshold,max(data[[2]]))
#				y <- 1 - cumsum(ddiscpowerexp(x=x,exponent=pl[[2]]$exponent,rate=pl[[2]]$rate,threshold=pl[[2]]$threshold))
#				lines(x, y, col="BLACK", lty=2)
#			}
#			else if(laws[paste0(names(data)[i],"-",meas,if(!is.na(wt)) paste0("-",wt) else "")]=="good")
#				lines(pl[[i]], col="BLACK", lty=2)
#		}
		
		# plot filtered data
		par(
			fig=inlay.coords[meas.name,], 
			new=TRUE,
			mgp=c(3,0.5,0)
		)
		plot.ccdf(
				data=data.flt, 
				main=NA, xlab=NA, ylab=NA, 
				log=TRUE, 
				cols=pal[1:2], 
				leg.title=if(meas.name=="eigenvector") "Sex" else NA, leg.pos="bottomleft",
				cex.lab=0.75, cex.axis=0.75, cex=0.75)
		
		# close plot file
		dev.off()
	}
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
