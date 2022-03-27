# Additional plots regarding the power-law relationship between
# local transitivity and the degree of a vertex. This allows
# studying the hierarchical structure of a network.
# 
# Vincent Labatut
# 01/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/trans_vs_deg.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/include.R")
start.rec.log(text="TransVsDeg")




###############################################################################
# compute the unfiltered data

# read the graph
graph.file <- get.path.data.graph(mode="scenes", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
g <- read.graphml.file(file=graph.file)

# compute values
deg.vals <- degree(graph=g, mode="all")
tra.vals <- transitivity(graph=g, type="local", weights=NA, isolates="zero")

# filter out zero degree and NaN
idx <- which(!is.nan(tra.vals) & tra.vals>0)
filt.tra <- tra.vals[idx]
filt.deg <- deg.vals[idx]
avg.tra <- sapply(1:max(filt.deg), function(d) mean(filt.tra[filt.deg==d]))

# keep tail
thresholds <- quantile(filt.deg, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
threshold <- thresholds[7]	# exp=0.42
cut.tra <- filt.tra[filt.deg>=threshold]
cut.deg <- filt.deg[filt.deg>=threshold]

# init parameters using a linear regression
fit <- lm(log(cut.tra) ~ log(cut.deg))
summary(fit)
params <- fit$coefficients
val1 <- exp(params[1]); names(val1) <- NULL
val2 <- params[2]; names(val2) <- NULL
val3 <- 0

# perform NL regression
df <- data.frame(cut.deg, cut.tra)
fit <- nlsLM(cut.tra ~ c1*cut.deg^c2, 
		start=list(c1=val1, c2=val2),
		data = df,
		control=list(maxiter=200))
summary(fit)

# plot
pal <- ATT_COLORS_FILT[c("Discard","Keep")]
col <- pal["Discard"]
col.sec <- combine.colors(col, "WHITE", transparency=20)
xlab <- "Degree $k$"
ylab <- "Local Transitivity $C(v)$"
#exponent <- summary(fit)$coefficients["c2","Estimate"]
plot.file <- get.path.stats.topo(net.type="static", mode="scenes", meas.name=MEAS_MULTI_NODES, filtered="both", suf="transitivity_vs_degree")
pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
par(
	mar=c(4,4,0,0)+0.1,	# remove the title space Bottom Left Top Right
	fig=c(0,1,0,1),		# set coordinate space of the original plot
	mgp=c(3,1,0)		# distance between axis ticks and values
)
# points
plot(
	x=filt.deg, y=filt.tra, 
	xlab=TeX(xlab), ylab=TeX(ylab),
	log="xy", 
	las=1, col=col.sec,
	xlim=c(1,max(deg.vals)*1.1)
)
# mean
idx <- which(!is.nan(avg.tra) & avg.tra>0)
lines(	
	x=idx, avg.tra[idx],
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
	legend=c("Mean","Fit")
)




################################################################################
## rewire network (to compare with a random network)
#rand.g <- (rewire(g, with=keeping_degseq(niter=vcount(g)*100)))
#
## compute values
#deg.vals <- degree(graph=rand.g, mode="all")
#tra.vals <- transitivity(graph=rand.g, type="local", weights=NA, isolates="zero")
#
## filter out zero degree and NaN
#idx <- which(!is.nan(tra.vals) & tra.vals>0)
#filt.tra <- tra.vals[idx]
#filt.deg <- deg.vals[idx]
#avg.tra <- sapply(1:max(filt.deg), function(d) mean(filt.tra[filt.deg==d]))
#
## add to plot
#points(
#	x=filt.deg, y=filt.tra, 
#	col="GRAY"
#)
## mean
#idx <- which(!is.nan(avg.tra) & avg.tra>0)
#lines(	
#	x=idx, y=avg.tra[idx],
#	col="BLACK"
#)




###############################################################################
# add the plot for the filtered net, as an inset

# filter the characters
filt.names <- V(g)$name[V(g)$Filter=="Discard"]
if(length(filt.names)==0) stop("Empty list of filtered characters")
g <- delete_vertices(g, V(g)$Filter=="Discard")

# compute values
deg.vals <- degree(graph=g, mode="all")
tra.vals <- transitivity(graph=g, type="local", weights=NA, isolates="zero")

# filter out zero degree and NaN
idx <- which(!is.nan(tra.vals) & tra.vals>0)
filt.tra <- tra.vals[idx]
filt.deg <- deg.vals[idx]
avg.tra <- sapply(1:max(filt.deg), function(d) mean(filt.tra[filt.deg==d]))

# keep tail
thresholds <- quantile(filt.deg, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
threshold <- thresholds[7]	# exp=1.53
cut.tra <- filt.tra[filt.deg>=threshold]
cut.deg <- filt.deg[filt.deg>=threshold]

# init parameters using a linear regression
fit <- lm(log(cut.tra) ~ log(cut.deg))
summary(fit)
params <- fit$coefficients
val1 <- exp(params[1]); names(val1) <- NULL
val2 <- params[2]; names(val2) <- NULL
val3 <- 0

# perform NL regression
df <- data.frame(cut.deg, cut.tra)
fit <- nlsLM(cut.tra ~ c1*cut.deg^c2, 
	start=list(c1=val1, c2=val2),
	data = df,
	control=list(maxiter=200))
summary(fit)

# plot as an inset
col <- pal["Keep"]
col.sec <- combine.colors(col, "WHITE", transparency=20)
par(
	fig=c(0.06,0.56, 0.05, 0.55), 
	new=TRUE,
	mgp=c(3,0.5,0)
)
# points
plot(
	x=filt.deg, y=filt.tra, 
	xlab=NA, ylab=NA,
	log="xy", 
	las=1, col=col.sec,
	xlim=c(1,max(deg.vals)*1.1),
	cex.lab=0.75, cex.axis=0.75, cex=0.75
)
# mean
idx <- which(!is.nan(avg.tra) & avg.tra>0)
lines(	
	x=idx, avg.tra[idx],
	col=col
)
# fitted line
threshold <- min(cut.deg)
x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
lines(x, predict(fit, list(cut.deg=x)), col="BLACK", lty=2)
# legend
legend(
	x="bottomleft",
	lty=1:2, col=c(col,"BLACK"),
	legend=c("Mean","Fit"),
	cex=0.75
)

# close file
dev.off()




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
