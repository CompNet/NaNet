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
tlog(0,"Load network")

# read the graph
graph.file <- get.path.graph.file(mode="scenes", ext=".graphml")
g <- read_graph(file=graph.file, format="graphml")
# clean names
V(g)$name <- fix.encoding(strings=V(g)$name)
V(g)$ShortName <- fix.encoding(strings=V(g)$ShortName)
kept <- which(!V(g)$Filtered)

# plot parameters
sexes <- c("Male","Female","Mixed","Unknown")
pal <- SEX_COLORS_4
pal[4] <- "DARKGRAY"




###############################################################################
tlog(0,"Basic stats regarding edges vs. sex")

# get edge list
tlog(2,"Edges by sex")
el <- as_edgelist(graph=g, names=FALSE)
# use sex instead of ids
el.sex <- cbind(V(g)[el[,1]]$Sex, V(g)[el[,2]]$Sex)
tlog(4,"In the unfiltered graph:")
#el.str <- apply(el.sex, 1, function(row) paste0(sort(row),collapse="--"))
#el.str[el.str=="Female--Mixed"] <- "Female--Male"
#el.str[el.str=="Male--Mixed"] <- "Female--Male"
#el.str[el.str=="Mixed--Mixed"] <- "Female--Male"
#table(el.str)
el.str <- t(apply(el.sex, 1, sort))
tlog(6,"Numbers of edges by sex")
print(table(el.str[,1], el.str[,2]))
tlog(6,"Same expressed as percents")
print(table(el.str[,1], el.str[,2])/(length(el.str)/2)*100)
# filtered graph
tlog(4,"In the filtered graph:")
idx <- which(el[,1] %in% kept | el[,1] %in% kept)
el.str <- t(apply(el.sex[idx,], 1, sort))
tlog(6,"Numbers of edges by sex")
print(table(el.str[,1], el.str[,2]))
tlog(6,"Same expressed as percents")
print(table(el.str[,1], el.str[,2])/(length(el.str)/2)*100)

# total degree by sex
tlog(2,"Total degree by sex")
tlog(4,"In the unfiltered graph:")
vals <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=MEAS_DEGREE, filtered=FALSE)
print(sapply(sexes, function(s) sum(vals[V(g)$Sex==s])))
tlog(4,"In the filtered graph:")
vals <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=MEAS_DEGREE, filtered=TRUE)
print(sapply(sexes, function(s) sum(vals[V(g)$Sex[kept]==s])))




###############################################################################
tlog(0,"Basic stats regarding scenes vs. sex")

# number of scenes by sex
sc <- data$char.scenes
sc.sexes <- lapply(sc, function(chars) V(g)[chars]$Sex)
sc.ov.sexes <- sapply(sc.sexes, function(sx)
		{	res <- NA
			sx <- sx[sx!="Unknown"]
			#print(sx)
			if(length(sx)>0)
			{	if(any(sx=="Mixed"))
					res <- "Mixed"
				else if(any(sx=="Male"))
				{	if(any(sx=="Female"))
						res <- "Mixed"
					else
						res <- "Male"
				}
				else
					res <- "Female"
			}
			return(res)
		})
tlog(2,"Numbers of scenes by sex (ignoring characters of unknown sex)")
print(table(sc.ov.sexes))
tlog(2,"Same expressed as percents")
print(table(sc.ov.sexes)/sum(table(sc.ov.sexes))*100)

# scenes with only women (more than one)
idx <- which(sc.ov.sexes=="Female" & sapply(sc.sexes, function(c) length(which(c=="Female"))>1))
tab <- cbind(data$stats.scenes[idx,], sapply(data$char.scenes[idx], function(c) paste(c,collapse=",")))
colnames(tab)[ncol(tab)] <- "Characters"
#write.csv(tab, file.path(STAT_CORPUS_FOLDER,"women_interactions.csv"))
tab2 <- read.csv2(file.path(STAT_CORPUS_FOLDER,"women_interactions.csv"))
tlog(2,"Bechdel test by scene:")
print(table(tab2[,"Annotation"], useNA="always"))




###############################################################################
tlog(0,"Producing sex-separated nodal measure distribution plots")

# measure names
meas.names <- c(MEAS_BETWEENNESS, MEAS_CLOSENESS, MEAS_DEGREE, MEAS_EIGENCNTR, paste0(MEAS_TRANSITIVITY,SFX_LOCAL))

# inlay position
inlay.coords <- matrix(nrow=length(meas.names), ncol=4)
rownames(inlay.coords) <- meas.names
inlay.coords[MEAS_BETWEENNESS,] <- c(0.06, 0.71, 0.05, 0.70)
inlay.coords[MEAS_CLOSENESS,] <- c(0.06, 0.57, 0.05, 0.56)
inlay.coords[MEAS_DEGREE,] <- c(0.06, 0.61, 0.05, 0.60)
inlay.coords[MEAS_EIGENCNTR,] <- c(0.06, 0.86, 0.05, 0.85)
inlay.coords[paste0(MEAS_TRANSITIVITY,SFX_LOCAL),] <- c(0.06, 0.59, 0.05, 0.58)
# log scale
log.scale <- c()
log.scale[MEAS_BETWEENNESS] <- TRUE
log.scale[MEAS_CLOSENESS] <- FALSE
log.scale[MEAS_DEGREE] <- TRUE
log.scale[MEAS_EIGENCNTR] <- TRUE
log.scale[paste0(MEAS_TRANSITIVITY,SFX_LOCAL)] <- FALSE

# process each measure
for(m in 1:length(meas.names))
{	meas.name <- meas.names[m]
	tlog(2,"Dealing with measure ",meas.name," (",m,"/",length(meas.names),")")
	
	# load precomputed data for unfiltered net
	tlog(4,"Loading pre-computed unfiltered values")
	vals <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=meas.name, filtered=FALSE)
	data.unf <- list()
	tlog(6,"Average values by sex:")
	for(sex in sexes)
	{	data.unf[[sex]] <- vals[V(g)$Sex==sex]
		tlog(8,sex,": ",mean(data.unf[[sex]],na.rm=TRUE))
	}
	data.unf[["Mixed"]] <- NULL; data.unf[["Unknown"]] <- NULL	# we don't care about these sexes
	# compare means
	tmp <- t.test(x=data.unf[["Male"]], y=data.unf[["Female"]], alternative="two.sided", paired=FALSE)
	tlog(6,"T-test Male vs. Female means: p=",tmp$p.value)
	tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (similar means) is rejected")
	# compare distributions
	if(meas.name %in% c(MEAS_DEGREE, MEAS_BETWEENNESS))
	{	#tmp <- chisq.test(x=data.unf[["Male"]], y=data.unf[["Female"]])
		tmp <- permTS(x=data.unf[["Male"]], y=data.unf[["Female"]], method="exact.mc", control=permControl(nmc=10^4-1))
		tlog(6,"Comparing Male vs. Female distributions with the permutation test: D=",tmp$statistic," p=",tmp$p.value)
		tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (distributions are similar) is rejected")
	}
	else
	{	tmp <- ks.test(data.unf[["Male"]],data.unf[["Female"]])
		tlog(6,"Comparing Male vs. Female distributions with the KS test: D=",tmp$statistic," p=",tmp$p.value)
		tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (distributions are similar) is rejected")
	}
	
	# load precomputed data for filtered net
	tlog(4,"Loading pre-computed filtered values")
	vals <- load.static.nodelink.stats.scenes(object="nodes", weights="occurrences", measure=meas.name, filtered=TRUE)
	data.flt <- list()
	for(sex in sexes)
	{	data.flt[[sex]] <- vals[V(g)$Sex[kept]==sex]
		tlog(8,sex,": ",mean(data.flt[[sex]],na.rm=TRUE))
	}
	data.flt[["Mixed"]] <- NULL; data.flt[["Unknown"]] <- NULL	# we don't care about these sexes 
	# compare means
	tmp <- t.test(x=data.flt[["Male"]], y=data.flt[["Female"]], alternative="two.sided", paired=FALSE)
	tlog(6,"T-test Male vs. Female means: p=",tmp$p.value)
	tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (similar means) is rejected")
	# compare distributions
	if(meas.name %in% c(MEAS_DEGREE, MEAS_BETWEENNESS))
	{	#tmp <- chisq.test(x=data.unf[["Male"]], y=data.unf[["Female"]])
		tmp <- permTS(x=data.flt[["Male"]], y=data.flt[["Female"]], method="exact.mc", control=permControl(nmc=10^4-1))
		tlog(6,"Comparing Male vs. Female distributions with the permutation test: D=",tmp$statistic," p=",tmp$p.value)
		tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (distributions are similar) is rejected")
	}
	else
	{	tmp <- ks.test(data.flt[["Male"]],data.flt[["Female"]])
		tlog(6,"Comparing Male vs. Female distributions with the KS test: D=",tmp$statistic," p=",tmp$p.value)
		tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (distributions are similar) is rejected")
	}
	
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
			log=log.scale[meas.name], 
			cols=pal, 
			leg.title=if(meas.name==MEAS_EIGENCNTR) NA else "Sex"
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
			log=log.scale[meas.name], 
			cols=pal, 
			leg.title=if(meas.name==MEAS_EIGENCNTR) "Sex" else NA, leg.pos="bottomleft",
			cex.lab=0.75, cex.axis=0.75, cex=0.75
		)
		
		# close plot file
		dev.off()
	}
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
