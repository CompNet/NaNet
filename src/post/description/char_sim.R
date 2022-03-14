# Additional plots regarding the similarity between certain characters.
# 
# Vincent Labatut
# 03/2022
#
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/char_sim.R")
###############################################################################
library("blockmodeling")
source("src/common/include.R")
start.rec.log(text="CharSim")



###############################################################################
tlog(0,"Load data and network")

# read raw data
tlog(2,"Reading previously computed corpus stats")
data <- read.corpus.data()
char.stats <- data$char.stats

# get filtered character names
filt.names <- data$char.stats[data$char.stats[,COL_FILTERED],COL_NAME]
if(length(filt.names)==0) stop("Empty list of filtered characters")

# read the graph
graph.file <- get.path.graph.file(mode="scenes", filtered=FALSE, desc="static", ext=".graphml")
tlog(2,"Reading file \"",graph.file,"\"")
g <- read.graphml.file(file=graph.file)
kept <- which(!V(g)$Filtered)

# compute the sequence of scene-based graphs (possibly one for each scene)
tlog(2,"Extracting the sequence of graphs")
gs <- list()
gs[["FALSE"]] <- extract.static.graph.scenes(
	inter.df=data$inter.df, 
	char.stats=char.stats, 
	volume.stats=data$volume.stats, 
	ret.seq=TRUE
)

# compute the filtered version
tlog(2,"Same thing for filtered graphs")
gs[["TRUE"]] <- future_lapply(gs[["FALSE"]], function(g) delete_vertices(g, v=intersect(filt.names,V(g)$name)))




###############################################################################
tlog(0,"Evolution of similarity between pairs of characters")

# similarity measures
sim.meas <- list()
sim.meas[["cosine"]] <- list(
	bounds=c(0,1),
	cname="Cosine Similarity",
	foo=function(a,i,j) {sum(a[i,]*a[j,])/sqrt(sum(a[i,]^2)*sum(a[j,]^2))}
)
sim.meas[["pearson"]] <- list(
	bounds=c(-1,1),
	cname="Pearson Coefficient",
	foo=function(a,i,j) {cor(x=a[i,], y=a[j,])}
)
sim.meas[["euclidean"]] <- list(
	bounds=c(0,NA),
	cname="Euclidean Distance",
	foo=function(a,i,j) {sqrt(sum((a[i,]-a[j,])^2))}
)
#sim.meas[["regulareq"]] <- list(
#	bounds=c(0,NA),
#	cname="Regular Equivalence",
#	foo=function(a,i,j) {REGE.for(M=a)$E[i,j]}
#)

# color parameters
pal <- get.palette(2)

# targeted pairs of characters
pairs <- matrix(c(
		"Thorgal", "Jolan", 
		"Thorgal", "Aaricia"),
		ncol=2, byrow=T)

tlog(2,"Looping over the pairs of vertices")
for(p in 1:nrow(pairs))
{	tlog(3,"Processing pair ",pairs[p,1],"--",pairs[p,2]," (",p,"/",nrow(pairs),")")
	
	# get full names
	idx <- c(which(char.stats[COL_NAME_SHORT]==pairs[p,1]), which(char.stats[COL_NAME_SHORT]==pairs[p,2]))
	fnames <- char.stats[idx,COL_NAME]
	
	# process unfiltered and filtered networks
	for(filt in c(FALSE,TRUE))
	{	tlog(4,"Processing the ",if(!filt) "un" else "","filtered network")
			
		# compute the similarity between both chars for each graph of the sequence
		tlog(5,"Looping over all graphs in the sequence")
		res <- t(sapply(gs[[as.character(filt)]], function(g) 
		{	vs <- match(fnames,  V(g)$name)
			if(any(is.na(vs)))
				res <- rep(NA,3)
			else
			{	a <- as_adjacency_matrix(graph=g, type="both", sparse=FALSE)
				res <- sapply(1:length(sim.meas), function(m) sim.meas[[m]]$foo(a, vs[1],vs[2]))
			}
			return(res)
		}))
		colnames(res) <- names(sim.meas)
	
		# plot the obtained values
		tlog(5,"Looping over all similarity measures")
		for(m in 1:length(sim.meas))
		{	plot.file <- get.path.topomeas.plot(object="nodepairs", mode="scenes", meas.name=paste0("comp_",names(sim.meas)[m]), filtered=filt, plot.type=paste0("pair=",paste0(pairs[p,],collapse="--")))
			tlog(6,"Creating file \"",plot.file,"\"")
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				# init plot with unfiltered results
				par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
				plot(
					x=1:nrow(res), y=res[,m], 
#					ylim=sim.meas[[m]]$bounds,
					xlab="Scene", #  "by publication order"
					ylab=names(sim.meas)[m],
					las=1, col=if(filt) pal[2] else pal[1],
					type="l"
				)
				dev.off()
			}
		}
	}
# TODO version with both filtered and unfiltered plots at once?
}




###############################################################################













###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
