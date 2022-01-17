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

# keep tail
thresholds <- quantile(filt.deg, probs=c(0,0.25,0.50,0.75,0.85,0.90,0.95))
threshold <- thresholds[4]
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

# plot
xlab <- "Degree $k$"
ylab <- "Neighbors' average Degree $<k_{nn}>$"
exponent <- summary(fit)$coefficients["c2","Estimate"]
# points
plot(
	x=filt.deg, y=filt.nei, 
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
threshold <- min(cut.deg)
x <- seq(from=threshold, to=max(deg.vals), by=(max(deg.vals)-threshold)/100)
lines(x, predict(fit, list(cut.deg=x)), col="BLACK", lty=2)
#lines(x, 1000*x^-0.5 + summary(fit)$coefficients["c3","Estimate"], col="BLACK", lty=2)
# legend
legend(
	x="topright",
	lty=1:2, col=c("RED","BLACK"),
	legend=c("Mean","Fit")
)




###############################################################################
# rewire network
rand.g <- (rewire(g, with=keeping_degseq(niter=vcount(g)*100)))

# compute values
rand.deg.vals <- degree(graph=rand.g, mode="all")
rand.tmp <- igraph::knn(graph=rand.g, weights=NULL)#, mode="all", neighbor.degree.mode="all")

# filter out zero degree and NaN
rand.idx <- which(!is.nan(rand.tmp$knn) & rand.tmp$knn>0)
rand.filt.nei <- rand.tmp$knn[rand.idx]
rand.filt.deg <- rand.deg.vals[rand.idx]

# add to plot
points(
	x=rand.filt.deg, y=rand.filt.nei, 
	col="GRAY"
)
# mean
rand.idx <- which(!is.nan(rand.tmp$knnk) & rand.tmp$knnk>0)
lines(	
	x=rand.idx, y=rand.tmp$knnk[idx],
	col="BLACK"
)




###############################################################################
# end logging
end.rec.log()
