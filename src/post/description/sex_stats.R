# Additional plots regarding the sex attribute.
# 
# Vincent Labatut
# 02/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/sex_stats.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="SexStats")




###############################################################################
tlog(0,"Load data and network")

# read raw data
data <- read.corpus.data(char.det="implicit")

# read the graph
graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
g <- read.graphml.file(file=graph.file)
kept <- which(V(g)$Filter=="Keep")

# plot parameters
sexes <- c("Male","Female","Mixed","Unknown")
pal <- ATT_COLORS$Sex[sexes]
pal.sec <- sapply(pal, function(col) combine.colors(col, "WHITE", transparency=20))




###############################################################################
tlog(0,"Basic stats regarding characters vs. sex")
char.stats <- data$char.stats

# all characters
tlog(2, "All characters:")
cnt.m <- length(which(char.stats[,"Sex"]=="Male"))
cnt.f <- length(which(char.stats[,"Sex"]=="Female"))
tab <- c(cnt.m,cnt.f)
per.m <- cnt.m/nrow(char.stats)*100
per.f <- cnt.f/nrow(char.stats)*100
tab <- cbind(tab, c(per.m,per.f))
tlog(4, "Proportions of: males=",per.m,", females=",per.f)
per.m <- cnt.m/(cnt.m+cnt.f)*100
per.f <- cnt.f/(cnt.m+cnt.f)*100
tab <- cbind(tab, c(per.m,per.f))
tlog(4, "Normalized to consider only males and females: males=",per.m,", females=",per.f,", F/M ratio=",per.f/per.m)
df <- data.frame(tab)
df <- cbind(c("Male","Female"),df)
colnames(df) <- c("Sex","Count","OverallProportion","FM_Proportion")
rownames(df) <- NULL
tab.file <- get.path.stats.corpus(object="characters", char.det="implicit", subfold="unfiltered", att="Sex", pref="attr_distrib_onlyFM")
tlog(4, "Record in \"",tab.file,"\"")
write.csv(x=tab, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)

# filtered data
tlog(2, "Filtered data:")
cnt.m <- length(which(char.stats[,"Sex"]=="Male" & char.stats[,COL_FILTER]=="Keep"))
cnt.f <- length(which(char.stats[,"Sex"]=="Female" & char.stats[,COL_FILTER]=="Keep"))
tab <- c(cnt.m,cnt.f)
per.m <- cnt.m/length(which(char.stats[,COL_FILTER]=="Keep"))*100
per.f <- cnt.f/length(which(char.stats[,COL_FILTER]=="Keep"))*100
tab <- cbind(tab, c(per.m,per.f))
tlog(4, "Proportions of: males=",per.m,", females=",per.f)
per.m <- cnt.m/(cnt.m+cnt.f)*100
per.f <- cnt.f/(cnt.m+cnt.f)*100
tab <- cbind(tab, c(per.m,per.f))
tlog(4, "Normalized to consider only males and females: males=",per.m,", females=",per.f,", F/M ratio=",per.f/per.m)
df <- data.frame(tab)
df <- cbind(c("Male","Female"),df)
colnames(df) <- c("Sex","Count","OverallProportion","FM_Proportion")
rownames(df) <- NULL
tab.file <- get.path.stats.corpus(object="characters", char.det="implicit", subfold="filtered", att="Sex", pref="attr_distrib_onlyFM")
tlog(4, "Record in \"",tab.file,"\"")
write.csv(x=tab, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)

# only named characters
tlog(2, "Named characters:")
cnt.m <- length(which(char.stats[,"Sex"]=="Male" & !char.stats[,"Named"]))
cnt.f <- length(which(char.stats[,"Sex"]=="Female" & !char.stats[,"Named"]))
tab <- c(cnt.m,cnt.f)
per.m <- cnt.m/length(which(!char.stats[,"Named"]))*100
per.f <- cnt.f/length(which(!char.stats[,"Named"]))*100
tab <- cbind(tab, c(per.m,per.f))
tlog(4, "Proportions of: males=",per.m,", females=",per.f)
per.m <- cnt.m/(cnt.m+cnt.f)*100
per.f <- cnt.f/(cnt.m+cnt.f)*100
tab <- cbind(tab, c(per.m,per.f))
tlog(4, "Normalized to consider only males and females: males=",per.m,", females=",per.f,", F/M ratio=",per.f/per.m)
df <- data.frame(tab)
df <- cbind(c("Male","Female"),df)
colnames(df) <- c("Sex","Count","OverallProportion","FM_Proportion")
rownames(df) <- NULL
tab.file <- get.path.stats.corpus(object="characters", char.det="implicit", subfold="unfiltered", att="Sex", pref="attr_distrib_onlyFM_named")
tlog(4, "Record in \"",tab.file,"\"")
write.csv(x=tab, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)




###############################################################################
tlog(0,"Basic stats regarding edges vs. sex")

# get edge list
tlog(2,"Edges by sex")
el <- as_edgelist(graph=g, names=FALSE)
# use sex instead of ids
el.sex <- cbind(V(g)[el[,1]]$Sex, V(g)[el[,2]]$Sex)
tlog(4,"In the unfiltered graph:")
el.str <- t(apply(el.sex, 1, sort))
# filter unknown sex and convert mixed sex
idx.mxd <- which(el.str[,1]=="Female" & el.str[,2]=="Mixed")
el.str[idx.mxd,2] <- rep("Male",length(idx.mxd))
idx.mxd <- which(el.str[,1]=="Male" & el.str[,2]=="Mixed")
el.str[idx.mxd,1] <- rep("Female",length(idx.mxd))
el.str[idx.mxd,2] <- rep("Male",length(idx.mxd))
idx.mxd <- which(el.str[,1]=="Mixed" & el.str[,2]=="Mixed")
el.str[idx.mxd,1] <- rep("Female",length(idx.mxd))
el.str[idx.mxd,2] <- rep("Male",length(idx.mxd))
idx.ukn <- which(el.str[,1]=="Unknown" | el.str[,2]=="Unknown")
el.str.unf <- el.str[-idx.ukn,]
tlog(6,"Numbers of edges by sex")
tt <- table(el.str.unf[,1], el.str.unf[,2])
print(tt)
tab.unf <- c(t(tt)[c(1,2,4)],NA)
tlog(6,"Same expressed as percents")
tt <- tt/sum(tt)*100
print(tt)
tab.unf <- cbind(tab.unf, c(t(tt)[c(1,2,4)],NA))
ratio <- sum(tt["Female",])/sum(tt[,"Male"])
tlog(6,"F/M Ratio: ",ratio)
tab.unf <- cbind(tab.unf, c(NA,NA,NA,ratio))
# filtered graph
tlog(4,"In the filtered graph:")
idx.rmv <- which(!(el[,1] %in% kept) | !(el[,2] %in% kept))
el.str.flt <- el.str[-union(idx.ukn,idx.rmv),]
tlog(6,"Numbers of edges by sex")
tt <- table(el.str.flt[,1], el.str.flt[,2])
print(tt)
tab.flt <- c(t(tt)[c(1,2,4)],NA)
tlog(6,"Same expressed as percents")
tt <- tt/sum(tt)*100
print(tt)
tab.flt <- cbind(tab.flt, c(t(tt)[c(1,2,4)],NA))
ratio <- sum(tt["Female",])/sum(tt[,"Male"])
tlog(6,"F/M Ratio: ",ratio)
tab.flt <- cbind(tab.flt, c(NA,NA,NA,ratio))

# save edge stats
tab.unf <- data.frame(tab.unf)
tab.unf <- cbind(c("F--F","F--M","M--M","Overall"), tab.unf)
colnames(tab.unf) <- c("EdgeSex","EdgeNbr","EdgeProportion","FM_Ratio")
rownames(tab.unf) <- NULL
tab.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", filtered="unfiltered", subfold="graph", pref="_stats_edges")
tlog(4, "Record unfiltered stats in \"",tab.file,"\"")
write.csv(x=tab.unf, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
#
tab.flt <- data.frame(tab.flt)
tab.flt <- cbind(c("F--F","F--M","M--M","Overall"), tab.flt)
colnames(tab.flt) <- c("EdgeSex","EdgeNbr","EdgeProportion","FM_Ratio")
rownames(tab.flt) <- NULL
tab.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", filtered="filtered", subfold="graph", pref="_stats_edges")
tlog(4, "Record filtered stats in \"",tab.file,"\"")
write.csv(x=tab.flt, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)


# total degree by sex
tlog(2,"Total degree by sex")
tlog(4,"In the unfiltered graph:")
vals <- load.static.nodelink.stats.scenes(weights="none", measure=MEAS_DEGREE, filtered="unfiltered", compare=FALSE)
cnt <- sapply(sexes, function(s) sum(vals[V(g)$Sex==s]))
print(cnt)
tab.unf <- c(cnt,NA)
ratio <- cnt["Female"]/cnt["Male"]
tlog(4,"Gender Degree Ratio: ",ratio)
tab.unf <- cbind(tab.unf, c(NA,NA,NA,NA,ratio))
#
tlog(4,"In the filtered graph:")
vals <- load.static.nodelink.stats.scenes(weights="none", measure=MEAS_DEGREE, filtered="filtered", compare=FALSE)
cnt <- sapply(sexes, function(s) sum(vals[V(g)$Sex[kept]==s]))
print(cnt)
tab.flt <- c(cnt,NA)
ratio <- cnt["Female"]/cnt["Male"]
tlog(4,"Gender Degree Ratio: ",ratio)
tab.flt <- cbind(tab.flt, c(NA,NA,NA,NA,ratio))

# total strength by sex
tlog(2,"Total strength by sex")
for(w in c("duration","occurrences"))
{	tlog(4,"Using ",w," as weights")
	# unfiltered
	tlog(6,"In the unfiltered graph:")
	vals <- load.static.nodelink.stats.scenes(weights=w, measure=MEAS_STRENGTH, filtered="unfiltered", compare=FALSE)
	cnt <- sapply(sexes, function(s) sum(vals[V(g)$Sex==s]))
	print(cnt)
	tab.unf <- cbind(tab.unf, c(cnt,NA))
	ratio <- cnt["Female"]/cnt["Male"]
	tlog(6,"Gender Strength Ratio: ",ratio)
	tab.unf <- cbind(tab.unf, c(NA,NA,NA,NA,ratio))
	# filtered
	tlog(6,"In the filtered graph:")
	vals <- load.static.nodelink.stats.scenes(weights=w, measure=MEAS_STRENGTH, filtered="filtered", compare=FALSE)
	cnt <- sapply(sexes, function(s) sum(vals[V(g)$Sex[kept]==s]))
	print(cnt)
	tab.flt <- cbind(tab.flt, c(cnt,NA))
	ratio <- cnt["Female"]/cnt["Male"]
	tlog(6,"Gender Strength Ratio: ",ratio)
	tab.flt <- cbind(tab.flt, c(NA,NA,NA,NA,ratio))
}

# sex-specific density
tlog(2,"Sex-specific density")
# unfiltered data
tlog(4,"Unfiltered network:")
gm <- delete_vertices(g, V(g)$Sex!="Male")
dens.mal <- edge_density(gm)
gf <- delete_vertices(g, V(g)$Sex!="Female")
dens.fem <- edge_density(gf)
tlog(6,"Density: male=",dens.mal," -- female=",dens.fem)
tab.unf <- cbind(tab.unf, c(dens.mal,dens.fem,NA,NA,NA))
tlog(6,"n: male=",gorder(gm)," -- female=",gorder(gf))
tlog(6,"m: male=",gsize(gm)," (",gorder(gm)*(gorder(gm)-1)/2,") -- female=",gsize(gf)," (",gorder(gf)*(gorder(gf)-1)/2,")")
# filtered data
tlog(4,"Filtered network:")
gm <- delete_vertices(gm, V(gm)$Filter=="Discard")
dens.mal <- edge_density(gm)
gf <- delete_vertices(gf, V(gf)$Filter=="Discard")
dens.fem <- edge_density(gf)
tlog(6,"Density: male=",dens.mal," -- female=",dens.fem)
tab.flt <- cbind(tab.flt, c(dens.mal,dens.fem,NA,NA,NA))
tlog(6,"n: male=",gorder(gm)," -- female=",gorder(gf))
tlog(6,"m: male=",gsize(gm)," (",gorder(gm)*(gorder(gm)-1)/2,") -- female=",gsize(gf)," (",gorder(gf)*(gorder(gf)-1)/2,")")

# save misc stats
tab.unf <- data.frame(tab.unf)
tab.unf <- cbind(c("Male","Female","Mixed","Unknown","Overall"), tab.unf)
colnames(tab.unf) <- c("Sex","TotalDegree","GenderDegreeRatio","TotalStrength_Dur","GenderStrengthRatio_Dur","TotalStrength_Occ","GenderStrengthRatio_Occ","Density")
rownames(tab.unf) <- NULL
tab.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", filtered="unfiltered", subfold="graph", pref="_stats_graph")
tlog(4, "Record unfiltered stats in \"",tab.file,"\"")
write.csv(x=tab.unf, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
#
tab.flt <- data.frame(tab.flt)
tab.flt <- cbind(c("Male","Female","Mixed","Unknown","Overall"), tab.flt)
colnames(tab.flt) <- c("Sex","TotalDegree","GenderDegreeRatio","TotalStrength_Dur","GenderStrengthRatio_Dur","TotalStrength_Occ","GenderStrengthRatio_Occ","Density")
rownames(tab.flt) <- NULL
tab.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", filtered="filtered", subfold="graph", pref="_stats_graph")
tlog(4, "Record filtered stats in \"",tab.file,"\"")
write.csv(x=tab.flt, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)




###############################################################################
tlog(0,"Basic stats regarding scenes vs. sex")

tlog(2,"Considering the unfiltered characters")
# number of scenes by sex
sc <- data$scene.chars
sc.sexes <- lapply(sc, function(chars) if(length(chars)==0) c() else V(g)[chars]$Sex)
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
tlog(4,"Numbers of scenes by sex (ignoring characters of unknown sex)")
tt <- table(sc.ov.sexes)
print(tt)
tab <- c(tt)
tlog(4,"Same expressed as percents")
tt <- tt/sum(tt)*100
print(tt)
tab <- cbind(tab, tt)
tab <- data.frame(tab)
tab <- cbind(rownames(tab), tab)
colnames(tab) <- c("Sex","Count","FM_Proportion")
rownames(tab) <- NULL
tab.file <- get.path.stats.corpus(object="characters", char.det="implicit", subfold="unfiltered", att="Sex", pref="distrib_scenes-by-char_onlyFM")
tlog(4, "Record in \"",tab.file,"\"")
write.csv(x=tab, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)

# scenes with only women (more than one)
###### generate the file to store annotations
#idx <- which(sc.ov.sexes=="Female" & sapply(sc.sexes, function(c) length(which(c=="Female"))>1))
#tab <- cbind(data$scene.stats[idx,], sapply(data$scene.chars[idx], function(c) paste(c,collapse=",")))
#colnames(tab)[ncol(tab)] <- "Characters"
#write.csv(tab, file.path(STAT_CORPUS_FOLDER, "implicit", "women_interactions.csv"), fileEncoding="UTF-8")
#tlog(4,"In the annotated file, TRUE means 'the females talk about a male'; FALSE means 'the females do not talk about a male'; and 'NA' means 'the females do not talk at all'.")
###### read the annotated file
tab2 <- read.csv2(file.path(STAT_CORPUS_FOLDER,"implicit","women_interactions.csv"))
######
tlog(4,"Bechdel test by scene:")
tt <- table(tab2[,"Annotation"], useNA="always")
print(tt)
tab <- c(tt)
tlog(4,"Same expressed as percents:")
tt <- tt/sum(tt)*100
print(tt)
tab <- cbind(tab,c(tt))
tab <- data.frame(tab)
tab <- cbind(c("Success","Fail","NA"),tab)
colnames(tab) <- c("BechdelTest","Count","Proportion")
rownames(tab) <- NULL
tab.file <- get.path.stats.corpus(object="characters", char.det="implicit", subfold="unfiltered", att="Sex", pref="distrib_scenes-by-char_bechdel-test")
tlog(4, "Record in \"",tab.file,"\"")
write.csv(x=tab, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)

tlog(2,"Considering the filtered characters")
# number of scenes by sex
sc <- data$scene.chars
sc.sexes <- lapply(sc, function(chars) if(length(chars)==0) c() else V(g)[chars]$Sex[!V(g)[chars]$Filter=="Discard"])
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
tlog(4,"Numbers of scenes by sex (ignoring characters of unknown sex)")
tt <- table(sc.ov.sexes)
print(tt)
tab <- c(tt)
tlog(4,"Same expressed as percents")
tt <- tt/sum(tt)*100
print(tt)
tab <- cbind(tab, tt)
tab <- data.frame(tab)
tab <- cbind(rownames(tab), tab)
colnames(tab) <- c("Sex", "Count", "FM_Proportion")
rownames(tab) <- NULL
tab.file <- get.path.stats.corpus(object="characters", char.det="implicit", subfold="filtered", att="Sex", pref="distrib_scenes-by-char_onlyFM")
tlog(4, "Record in \"",tab.file,"\"")
write.csv(x=tab, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)

# NOTE: no sense in doing Bechdel for filtered chars, as it requires 
# ignoring certain characters actively taking part in the scene




###############################################################################
tlog(0,"Homophily by sex")
# unfiltered net
gs <- delete_vertices(g, V(g)$Sex!="Female" & V(g)$Sex!="Male")
ass.unf <- assortativity_nominal(graph=gs, types=factor(V(gs)$Sex), directed=FALSE)
tlog(0,"Unfiltered network: ",ass.unf)
tab.unf <- cbind(tab.unf, c(NA,NA,NA,NA,ass.unf))
colnames(tab.unf)[ncol(tab.unf)] <- "Homophily"
tab.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", filtered="unfiltered", subfold="graph", pref="_stats_graph")
tlog(4, "Record unfiltered stats in \"",tab.file,"\"")
write.csv(x=tab.unf, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)

# filtered net
gs <- delete_vertices(gs, V(gs)$Filter=="Discard")
ass.flt <- assortativity_nominal(graph=gs, types=factor(V(gs)$Sex), directed=FALSE)
tlog(0,"Filtered network: ",ass.flt)
tab.flt <- cbind(tab.flt, c(NA,NA,NA,NA,ass.flt))
colnames(tab.flt)[ncol(tab.flt)] <- "Homophily"
tab.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", filtered="filtered", subfold="graph", pref="_stats_graph")
tlog(4, "Record filtered stats in \"",tab.file,"\"")
write.csv(x=tab.flt, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)




###############################################################################
tlog(0,"Triangles by sex")
# unfiltered net
gs <- delete_vertices(g, V(g)$Sex!="Female" & V(g)$Sex!="Male")
tris <- triangles(graph=gs)$Sex
col.tris <- sapply(1:(length(tris)/3), function(t) paste(sort(tris[(t*3-2):(t*3)]),collapse="-"))
tlog(0,"Unfiltered network:")
tt <- table(col.tris)
print(tt)
tab <- c(tt)
# same as proportions
tt <- tt/sum(tt)*100
print(tt)
tab <- cbind(tab, c(tt))
# record
tab <- data.frame(tab)
tab <- cbind(rownames(tab), tab)
colnames(tab) <- c("Sex", "Count", "FM_Proportion")
rownames(tab) <- NULL
tab.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", filtered="unfiltered", subfold="graph", pref="_stats_triangles")
tlog(4, "Record in \"",tab.file,"\"")
write.csv(x=tab, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)


# filtered net
gs <- delete_vertices(gs, V(gs)$Filter=="Discard")
tris <- triangles(graph=gs)$Sex
col.tris <- sapply(1:(length(tris)/3), function(t) paste(sort(tris[(t*3-2):(t*3)]),collapse="-"))
tlog(0,"Unfiltered network:")
tt <- table(col.tris)
print(tt)
tab <- c(tt)
# same as proportions
tt <- tt/sum(tt)*100
print(tt)
tab <- cbind(tab, c(tt))
# record
tab <- data.frame(tab)
tab <- cbind(rownames(tab), tab)
colnames(tab) <- c("Sex", "Count", "FM_Proportion")
rownames(tab) <- NULL
tab.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", filtered="filtered", subfold="graph", pref="_stats_triangles")
tlog(4, "Record in \"",tab.file,"\"")
write.csv(x=tab, file=paste0(tab.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)




###############################################################################
tlog(0,"Producing sex-separated nodal measure distribution plots")

# measure names
meas.names <- c(
	MEAS_BETWEENNESS, 
	MEAS_CLOSENESS, 
	MEAS_DEGREE, 
	MEAS_EIGENCNTR, 
	paste0(MEAS_TRANSITIVITY,SFX_LOCAL)
)

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
	
	# init result matrix
	tab <- data.frame(matrix(NA,ncol=4,nrow=0))
	colnames(tab) <- c("Network","TestName","Stat","P-value")
	
	# load precomputed data for unfiltered net
	tlog(4,"Loading pre-computed unfiltered values")
	vals <- load.static.nodelink.stats.scenes(weights="none", measure=meas.name, filtered="unfiltered", compare=FALSE)
	data.unf <- list()
	tlog(6,"Statistics by sex:")
	for(sex in sexes)
	{	data.unf[[sex]] <- vals[V(g)$Sex==sex]
		tlog(8,sex,": avg=",mean(data.unf[[sex]],na.rm=TRUE)," sdev=",sd(data.unf[[sex]],na.rm=TRUE))
	}
	data.unf[["Mixed"]] <- NULL; data.unf[["Unknown"]] <- NULL	# we don't care about these sexes
	# compare means
	tmp <- t.test(x=data.unf[["Male"]], y=data.unf[["Female"]], alternative="two.sided", paired=FALSE)
	tlog(6,"T-test Male vs. Female means: p=",tmp$p.value)
	tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (similar means) is rejected")
	# compare distributions
	if(meas.name %in% c(MEAS_DEGREE, MEAS_BETWEENNESS))
	{	test.name <- "Permutation"
		#tmp <- chisq.test(x=data.unf[["Male"]], y=data.unf[["Female"]])
		tmp <- permTS(x=data.unf[["Male"]], y=data.unf[["Female"]], method="exact.mc", control=permControl(nmc=10^4-1))
		stat <- tmp$statistic; if(is.null(stat)) stat <- NA
		pval <- tmp$p.value
		tlog(6,"Comparing Male vs. Female distributions with the permutation test: D=",stat," p=",pval)
		tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (distributions are similar) is rejected")
	}
	else
	{	test.name <- "Kolmogorov-Smirnov"
		tmp <- ks.test(data.unf[["Male"]],data.unf[["Female"]])
		stat <- tmp$statistic
		pval <- tmp$p.value
		tlog(6,"Comparing Male vs. Female distributions with the KS test: D=",stat," p=",pval)
		tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (distributions are similar) is rejected")
	}
	df <- data.frame("Unfiltered",test.name,stat,pval);colnames(df) <- colnames(tab)
	tab <- rbind(tab, df)
	
	# load precomputed data for filtered net
	tlog(4,"Loading pre-computed filtered values")
	vals <- load.static.nodelink.stats.scenes(weights="none", measure=meas.name, filtered="filtered", compare=FALSE)
	data.flt <- list()
	tlog(6,"Statistics by sex:")
	for(sex in sexes)
	{	data.flt[[sex]] <- vals[V(g)$Sex[kept]==sex]
		tlog(8,sex,": avg=",mean(data.flt[[sex]],na.rm=TRUE)," sdev=",sd(data.flt[[sex]],na.rm=TRUE))
	}
	data.flt[["Mixed"]] <- NULL; data.flt[["Unknown"]] <- NULL	# we don't care about these sexes 
	# compare means
	tmp <- t.test(x=data.flt[["Male"]], y=data.flt[["Female"]], alternative="two.sided", paired=FALSE)
	tlog(6,"T-test Male vs. Female means: p=",tmp$p.value)
	tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (similar means) is rejected")
	# compare distributions
	if(meas.name %in% c(MEAS_DEGREE, MEAS_BETWEENNESS))
	{	test.name <- "Permutation"
		#tmp <- chisq.test(x=data.unf[["Male"]], y=data.unf[["Female"]])
		tmp <- permTS(x=data.flt[["Male"]], y=data.flt[["Female"]], method="exact.mc", control=permControl(nmc=10^4-1))
		stat <- tmp$statistic; if(is.null(stat)) stat <- NA
		pval <- tmp$p.value
		tlog(6,"Comparing Male vs. Female distributions with the permutation test: D=",stat," p=",pval)
		tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (distributions are similar) is rejected")
	}
	else
	{	test.name <- "Kolmogorov-Smirnov"
		tmp <- ks.test(data.flt[["Male"]],data.flt[["Female"]])
		stat <- tmp$statistic
		pval <- tmp$p.value
		tlog(6,"Comparing Male vs. Female distributions with the KS test: D=",stat," p=",pval)
		tlog(8,"Reminder: small p (eg. p<0.05) means that the null hypothesis (distributions are similar) is rejected")
	}
	df <- data.frame("Filtered",test.name,stat,pval);colnames(df) <- colnames(tab)
	tab <- rbind(tab, df)
	
	# record test results
	test.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", meas.name=meas.name, filtered="both", suf="comparison_test")
	write.csv(x=tab, file=paste0(test.file,".csv"), fileEncoding="UTF-8", row.names=FALSE)
	
	# set params
	plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", meas.name=meas.name, filtered="both", suf="ccdf")
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
			leg=meas.name!=MEAS_EIGENCNTR, leg.title="Sex"
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
			leg=meas.name==MEAS_EIGENCNTR, leg.title="Sex", leg.pos="bottomleft",
			cex.lab=0.75, cex.axis=0.75, cex=0.75
		)
		
		# close plot file
		dev.off()
	}
}




###############################################################################
# transitivity vs. degree plot
tlog(0,"Plotting transitivity vs. degree")

# load precomputed data for unfiltered net
tlog(2,"Loading pre-computed unfiltered values")
deg.vals <- load.static.nodelink.stats.scenes(weights="none", measure=MEAS_DEGREE, filtered="unfiltered", compare=FALSE)
tra.vals <- load.static.nodelink.stats.scenes(weights="none", measure=paste0(MEAS_TRANSITIVITY,SFX_LOCAL), filtered="unfiltered", compare=FALSE)

## keep tail
#thresholds <- quantile(deg.filt, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
#threshold <- thresholds[7]	# exp=0.42
#cut.tra <- tra.filt[deg.filt>=threshold]
#cut.deg <- deg.filt[deg.filt>=threshold]
#
## init parameters using a linear regression
#fit <- lm(log(cut.tra) ~ log(cut.deg))
#summary(fit)
#params <- fit$coefficients
#val1 <- exp(params[1]); names(val1) <- NULL
#val2 <- params[2]; names(val2) <- NULL
#val3 <- 0
#
## perform NL regression
#df <- data.frame(cut.deg, cut.tra)
#fit <- nlsLM(cut.tra ~ c1*cut.deg^c2, 
#		start=list(c1=val1, c2=val2),
#		data = df,
#		control=list(maxiter=200))
#summary(fit)

# set up series
deg.unf <- list()
tra.unf <- list()
tra.avg.unf <- list()
for(sex in sexes)
{	# get values for the considered sex
	deg.unf[[sex]] <- deg.vals[V(g)$Sex==sex]
	tra.unf[[sex]] <- tra.vals[V(g)$Sex==sex]
	
	# filter out zero degree and NaN
	idx <- which(!is.nan(tra.unf[[sex]]) & tra.unf[[sex]]>0)
	deg.unf[[sex]] <- deg.unf[[sex]][idx]
	tra.unf[[sex]] <- tra.unf[[sex]][idx]
	tra.avg.unf[[sex]] <- sapply(1:max(deg.unf[[sex]]), function(d) mean(tra.unf[[sex]][deg.unf[[sex]]==d]))
	
}
# we don't care about certain sexes
deg.unf[["Mixed"]] <- NULL; deg.unf[["Unknown"]] <- NULL	
tra.unf[["Mixed"]] <- NULL; tra.unf[["Unknown"]] <- NULL
tra.avg.unf[["Mixed"]] <- NULL; tra.avg.unf[["Unknown"]] <- NULL

# create plot
xlab <- "Degree $k$"
ylab <- "Local Transitivity $C(v)$"
#exponent <- summary(fit)$coefficients["c2","Estimate"]
plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", meas.name=MEAS_MULTI_NODES, filtered="both", suf="transitivity_vs_degree")
tlog(2,"Plotting in file ",plot.file)
pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
par(
	mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
	fig=c(0,1,0,1),		# set coordinate space of the original plot
	mgp=c(3,1,0)		# distance between axis ticks and values
)
# init
plot(
	NULL,
	xlab=TeX(xlab), ylab=TeX(ylab),
	log="xy", 
	las=1,
	xlim=range(unlist(deg.unf)), ylim=range(unlist(tra.unf))
)
# series
for(s in 1:length(deg.unf))
{	points(
		x=deg.unf[[s]], y=tra.unf[[s]],
		col=pal[s]	# col=pal.sec[s]
	)
#	# mean
#	idx <- which(!is.nan(tra.avg.unf[[s]]) & tra.avg.unf[[s]]>0)
#	lines(	
#		x=idx, tra.avg.unf[[s]][idx],
#		col=pal[s]
#	)
#	# fitted line
#	threshold <- min(cut.deg)
#	x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
#	lines(x, predict(fit, list(cut.deg=x)), col="BLACK", lty=2)
}
## legend
#legend(
#	x="topright",
#	lty=1:2, col=c(col,"BLACK"),
#	legend=c("Mean","Fit")
#)
legend(
	x="topright",
	fill=pal[1:length(deg.unf)],
	legend=sexes[1:length(deg.unf)]
)

####
# load precomputed data for unfiltered net
tlog(2,"Loading pre-computed filtered values")
deg.vals <- load.static.nodelink.stats.scenes(weights="none", measure=MEAS_DEGREE, filtered="filtered", compare=FALSE)
tra.vals <- load.static.nodelink.stats.scenes(weights="none", measure=paste0(MEAS_TRANSITIVITY,SFX_LOCAL), filtered="filtered", compare=FALSE)

## keep tail
#thresholds <- quantile(deg.filt, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
#threshold <- thresholds[7]	# exp=0.42
#cut.tra <- tra.filt[deg.filt>=threshold]
#cut.deg <- deg.filt[deg.filt>=threshold]
#
## init parameters using a linear regression
#fit <- lm(log(cut.tra) ~ log(cut.deg))
#summary(fit)
#params <- fit$coefficients
#val1 <- exp(params[1]); names(val1) <- NULL
#val2 <- params[2]; names(val2) <- NULL
#val3 <- 0
#
## perform NL regression
#df <- data.frame(cut.deg, cut.tra)
#fit <- nlsLM(cut.tra ~ c1*cut.deg^c2, 
#		start=list(c1=val1, c2=val2),
#		data = df,
#		control=list(maxiter=200))
#summary(fit)

# set up series
deg.flt <- list()
tra.flt <- list()
tra.avg.flt <- list()
for(sex in sexes)
{	# get values for the considered sex
	deg.flt[[sex]] <- deg.vals[V(g)$Sex[kept]==sex]
	tra.flt[[sex]] <- tra.vals[V(g)$Sex[kept]==sex]
	
	# filter out zero degree and NaN
	idx <- which(!is.nan(tra.flt[[sex]]) & tra.flt[[sex]]>0)
	deg.flt[[sex]] <- deg.flt[[sex]][idx]
	tra.flt[[sex]] <- tra.flt[[sex]][idx]
	tra.avg.flt[[sex]] <- sapply(1:max(deg.flt[[sex]]), function(d) mean(tra.flt[[sex]][deg.flt[[sex]]==d]))
	
}
# we don't care about certain sexes
deg.flt[["Mixed"]] <- NULL; deg.flt[["Unknown"]] <- NULL	
tra.flt[["Mixed"]] <- NULL; tra.flt[["Unknown"]] <- NULL
tra.avg.flt[["Mixed"]] <- NULL; tra.avg.flt[["Unknown"]] <- NULL

# update plot
par(
	fig=c(0.06,0.56, 0.05, 0.55), 
	new=TRUE,
	mgp=c(3,0.5,0)
)
# init
plot(
	NULL,
	xlab=NA, ylab=NA,
	log="xy", 
	las=1,
	xlim=range(unlist(deg.flt)), ylim=range(unlist(tra.flt)),
	cex.lab=0.75, cex.axis=0.75, cex=0.75
)
# series
for(s in 1:length(deg.flt))
{	points(
		x=deg.flt[[s]], y=tra.flt[[s]],
		col=pal[s],	# col=pal.sec[s]
		cex.lab=0.75, cex.axis=0.75, cex=0.75
	)
#	# mean
#	idx <- which(!is.nan(tra.avg.flt[[s]]) & tra.avg.flt[[s]]>0)
#	lines(	
#		x=idx, tra.avg.flt[[s]][idx],
#		col=pal[s]
#	)
#	# fitted line
#	threshold <- min(cut.deg)
#	x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
#	lines(x, predict(fit, list(cut.deg=x)), col="BLACK", lty=2)
}
## legend
#legend(
#	x="topright",
#	lty=1:2, col=c(col,"BLACK"),
#	legend=c("Mean","Fit")
#)

# close file
dev.off()




###############################################################################
# average neighbors' degree vs. degree plot
tlog(0,"Plotting average neighbors' degree vs. degree")

# compute unfiltered values
deg.vals <- degree(graph=g, mode="all")
nei.vals <- igraph::knn(graph=g, weights=NULL)$knn	#, mode="all", neighbor.degree.mode="all")

# set up series
deg.unf <- list()
nei.unf <- list()
nei.avg.unf <- list()
for(sex in sexes)
{	# get values for the considered sex
	deg.unf[[sex]] <- deg.vals[V(g)$Sex==sex]
	nei.unf[[sex]] <- nei.vals[V(g)$Sex==sex]
	
	# filter out zero degree and NaN
	idx <- which(!is.nan(nei.unf[[sex]]) & nei.unf[[sex]]>0)
	deg.unf[[sex]] <- deg.unf[[sex]][idx]
	nei.unf[[sex]] <- nei.unf[[sex]][idx]
	nei.avg.unf[[sex]] <- sapply(1:max(deg.unf[[sex]]), function(d) mean(nei.unf[[sex]][deg.unf[[sex]]==d]))
	
}
# we don't care about certain sexes
deg.unf[["Mixed"]] <- NULL; deg.unf[["Unknown"]] <- NULL	
nei.unf[["Mixed"]] <- NULL; nei.unf[["Unknown"]] <- NULL
tra.avg.unf[["Mixed"]] <- NULL; tra.avg.unf[["Unknown"]] <- NULL

# create plot
xlab <- "Degree $k$"
ylab <- "Neighbors' average Degree $<k_{nn}>$"
#exponent <- summary(fit)$coefficients["c2","Estimate"]
plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", att="Sex", weights="none", meas.name=MEAS_MULTI_NODES, filtered="both", suf="nei-degree_vs_degree")
tlog(2,"Plotting in file ",plot.file)
pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
par(
	mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
	fig=c(0,1,0,1),		# set coordinate space of the original plot
	mgp=c(3,1,0)		# distance between axis ticks and values
)
# init
plot(
	NULL,
	xlab=TeX(xlab), ylab=TeX(ylab),
	log="xy", 
	las=1,
	xlim=range(unlist(deg.unf)), ylim=range(unlist(nei.unf))
)
# series
for(s in 1:length(deg.unf))
{	points(
		x=deg.unf[[s]], y=nei.unf[[s]],
		col=pal[s]	# col=pal.sec[s]
	)
#	# mean
#	idx <- which(!is.nan(nei.avg.unf[[s]]) & nei.avg.unf[[s]]>0)
#	lines(	
#		x=idx, nei.avg.unf[[s]][idx],
#		col=pal[s]
#	)
}
legend(
	x="topright",
	fill=pal[1:length(deg.unf)],
	legend=sexes[1:length(deg.unf)]
)

####
# compute filtered values
gf <- delete_vertices(g, V(g)$Filter=="Discard")
deg.vals <- degree(graph=gf, mode="all")
nei.vals <- igraph::knn(graph=gf, weights=NULL)$knn	#, mode="all", neighbor.degree.mode="all")

# set up series
deg.flt <- list()
nei.flt <- list()
nei.avg.flt <- list()
for(sex in sexes)
{	# get values for the considered sex
	deg.flt[[sex]] <- deg.vals[V(gf)$Sex==sex]
	nei.flt[[sex]] <- nei.vals[V(gf)$Sex==sex]
	
	# filter out zero degree and NaN
	idx <- which(!is.nan(nei.flt[[sex]]) & nei.flt[[sex]]>0)
	deg.flt[[sex]] <- deg.flt[[sex]][idx]
	nei.flt[[sex]] <- nei.flt[[sex]][idx]
	nei.avg.flt[[sex]] <- sapply(1:max(deg.flt[[sex]]), function(d) mean(nei.flt[[sex]][deg.flt[[sex]]==d]))
	
}
# we don't care about certain sexes
deg.flt[["Mixed"]] <- NULL; deg.flt[["Unknown"]] <- NULL	
nei.flt[["Mixed"]] <- NULL; nei.flt[["Unknown"]] <- NULL
tra.avg.flt[["Mixed"]] <- NULL; tra.avg.flt[["Unknown"]] <- NULL

# update plot
par(
	fig=c(0.57,0.98, 0.05, 0.46), 
	new=TRUE,
	mgp=c(3,0.5,0)
)
# init
plot(
	NULL,
	xlab=NA, ylab=NA,
	log="xy", 
	las=1,
	xlim=range(unlist(deg.flt)), ylim=range(unlist(nei.flt)),
	cex.lab=0.75, cex.axis=0.75, cex=0.75
)
# series
for(s in 1:length(deg.flt))
{	points(
		x=deg.flt[[s]], y=nei.flt[[s]],
		col=pal[s],	# col=pal.sec[s]
		cex.lab=0.75, cex.axis=0.75, cex=0.75
		)
#	# mean
#	idx <- which(!is.nan(nei.avg.flt[[s]]) & nei.avg.flt[[s]]>0)
#	lines(	
#		x=idx, nei.avg.flt[[s]][idx],
#		col=pal[s]
#	)
}

# close file
dev.off()




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
