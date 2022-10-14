# Additional plots regarding the power-law relationship between the
# the average degree of the neighbors of a vertex, and the 
# degree of this vertex. This allows studying degree assortativity.
# 
# Vincent Labatut
# 01/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/nei_vs_deg.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="NeighVsDeg")




###############################################################################
# compute the unfiltered data

# read the graph
graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
tlog(0,"Read the graph file: \"",graph.file,"\"")
g <- read.graphml.file(file=graph.file)

# compute values
tlog(2,"Compute the graph degree")
deg.vals <- degree(graph=g, mode="all")
tmp <- igraph::knn(graph=g, weights=NULL)#, mode="all", neighbor.degree.mode="all")

# filter out zero degree and NaN
tlog(2,"Filter out zero degree nodes")
idx <- which(!is.nan(tmp$knn) & tmp$knn>0)
filt.nei <- tmp$knn[idx]
filt.deg <- deg.vals[idx]

# keep tail
tlog(2,"Keep tail")
thresholds <- quantile(filt.deg, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
threshold <- thresholds[4]	# exp=0.48
cut.nei <- filt.nei[filt.deg>=threshold]
cut.deg <- filt.deg[filt.deg>=threshold]

# init parameters using a linear regression
tlog(2,"Perform regression")
fit <- lm(log(cut.nei) ~ log(cut.deg))
summary(fit)
params <- fit$coefficients
val1 <- exp(params[1]); names(val1) <- NULL
val2 <- params[2]; names(val2) <- NULL
val3 <- 0

# perform NL regression
df <- data.frame(cut.deg, cut.nei)
fit <- nlsLM(cut.nei ~ c1*cut.deg^c2, 
	start=list(c1=val1, c2=val2),
	data = df,
	control=list(maxiter=200))
summary(fit)

# plot
pal <- ATT_COLORS_FILT[c("Discard","Keep")]
col <- pal["Discard"]
col.sec <- combine.colors(col, "WHITE", transparency=20)
xlab <- "Degree $k$"
ylab <- "Neighbors' average Degree $<k_{nn}>$"
exponent <- summary(fit)$coefficients["c2","Estimate"]
plot.file <- get.path.stats.topo(mode="scenes", char.det="implicit", net.type="static", weights="none", meas.name=MEAS_MULTI_NODES, filtered="both", suf="nei-degree_vs_degree")
tlog(2,"Plot in \"",plot.file,"\"")
pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
par(
	mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
	fig=c(0,1,0,1),		# set coordinate space of the original plot
	mgp=c(3,1,0)		# distance between axis ticks and values
)
# points
plot(
	x=filt.deg, y=filt.nei, 
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
threshold <- min(cut.deg)
x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
lines(x, predict(fit, list(cut.deg=x)), col="BLACK", lty=2)
#lines(x, 1000*x^-0.5 + summary(fit)$coefficients["c3","Estimate"], col="BLACK", lty=2)
# legend
legend(
	x="topright",
	lty=1:2, col=c(col,"BLACK"),
	legend=c("Mean","Fit")
)




###############################################################################
## one can compare this to a randomly rewired network

## rewire the network
#rand.g <- (rewire(g, with=keeping_degseq(niter=vcount(g)*100)))
#
## compute values
#rand.deg.vals <- degree(graph=rand.g, mode="all")
#rand.tmp <- igraph::knn(graph=rand.g, weights=NULL)#, mode="all", neighbor.degree.mode="all")
#
## filter out zero degree and NaN
#rand.idx <- which(!is.nan(rand.tmp$knn) & rand.tmp$knn>0)
#rand.filt.nei <- rand.tmp$knn[rand.idx]
#rand.filt.deg <- rand.deg.vals[rand.idx]
#
## add to plot
#points(
#	x=rand.filt.deg, y=rand.filt.nei, 
#	col="GRAY"
#)
## mean
#rand.idx <- which(!is.nan(rand.tmp$knnk) & rand.tmp$knnk>0)
#lines(	
#	x=rand.idx, y=rand.tmp$knnk[idx],
#	col="BLACK"
#)




###############################################################################
# add the plot for the filtered net, as an inset
tlog(2,"Same thing for filtered net")

# filter the characters
filt.names <- V(g)$name[V(g)$Filter=="Discard"]
if(length(filt.names)==0) stop("Empty list of filtered characters")
g <- delete_vertices(g, V(g)$Filter=="Discard")

# compute values
deg.vals <- degree(graph=g, mode="all")
tmp <- igraph::knn(graph=g, weights=NULL)#, mode="all", neighbor.degree.mode="all")

# filter out zero degree and NaN
idx <- which(!is.nan(tmp$knn) & tmp$knn>0)
filt.nei <- tmp$knn[idx]
filt.deg <- deg.vals[idx]

# keep tail
thresholds <- quantile(filt.deg, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
threshold <- thresholds[7]	# exp=0.70
cut.nei <- filt.nei[filt.deg>=threshold]
cut.deg <- filt.deg[filt.deg>=threshold]

# init parameters using a linear regression
fit <- lm(log(cut.nei) ~ log(cut.deg))
summary(fit)
params <- fit$coefficients
val1 <- exp(params[1]); names(val1) <- NULL
val2 <- params[2]; names(val2) <- NULL
val3 <- 0

# perform NL regression
df <- data.frame(cut.deg, cut.nei)
fit <- nlsLM(cut.nei ~ c1*cut.deg^c2, 
		start=list(c1=val1, c2=val2),
		data = df,
		control=list(maxiter=200))
summary(fit)

# plot as an inset
col <- pal["Keep"]
col.sec <- combine.colors(col, "WHITE", transparency=20)
par(
	fig=c(0.57,0.98, 0.05, 0.46), 
	new=TRUE,
	mgp=c(3,0.5,0)
)
# points
plot(
	x=filt.deg, y=filt.nei, 
	xlab=NA, 
	ylab=NA,
	log="xy", 
	las=1, col=col.sec,
	xlim=c(1,max(deg.vals)*1.1),
	cex.lab=0.75, cex.axis=0.75, cex=0.75
)
# mean
idx <- which(!is.nan(tmp$knnk) & tmp$knnk>0)
lines(	
	x=idx, y=tmp$knnk[idx],
	col=col
)
# fitted line
threshold <- min(cut.deg)
x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
lines(x, predict(fit, list(cut.deg=x)), col="BLACK", lty=2)
# legend
legend(
	x="topright",
	lty=1:2, col=c(col,"BLACK"),
	legend=c("Mean","Fit"),
	cex=0.75
)
dev.off()




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
