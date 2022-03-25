# Additional plots regarding the similarity between certain characters.
# 
# Vincent Labatut
# 03/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/char_sim.R")
###############################################################################
SERIES <- "Thorgal"
library("blockmodeling")
source("src/common/include.R")
start.rec.log(text="CharSim")




################################################################################
# main parameters
wide <- TRUE				# wide plots showing volumes as rectangles
narr.smooth <- FALSE			# whether to use narrative smoothing or cumulative scene-based networks
weighted <- TRUE			# whether to use the graph weights
sc.lim <- NA				# limit on the considered scenes (NA for no limit)
pub.order <- TRUE			# whether to use the volume publication vs. story order




################################################################################
tlog(0,"Load data and network")

# read raw data
tlog(2,"Reading previously computed corpus stats")
data <- read.corpus.data()
char.stats <- data$char.stats
scene.stats <- data$scene.stats
scene.chars <- data$scene.chars
volume.stats <- data$volume.stats
scene.stats <- data$scene.stats

# get filtered character names
filt.names <- data$char.stats[data$char.stats[,COL_FILTER]=="Discard",COL_NAME]
if(length(filt.names)==0) stop("Empty list of filtered characters")

# read the graph
graph.file <- get.path.data.graph(mode="scenes", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
tlog(2,"Reading file \"",graph.file,"\"")
g <- read.graphml.file(file=graph.file)
kept <- which(V(g)$Filter=="Keep")

# compute the sequence of scene-based graphs (possibly one for each scene)
gs <- list()
w.name <- "none"
if(narr.smooth)
{	net.type <- "narr_smooth"
	gs[["unfiltered"]] <- ns.read.graph(filtered=FALSE, remove.isolates=TRUE, pub.order=pub.order)
	gs[["filtered"]] <- ns.read.graph(filtered=TRUE, remove.isolates=TRUE, pub.order=pub.order)
	if(weighted)
		w.name <- "normalized"
}else
{	net.type <- "cumulative"
	tlog(2,"Extracting the sequence of graphs")
	gs[["unfiltered"]] <- extract.static.graph.scenes(
		inter.df=data$inter.df, 
		char.stats=char.stats, 
		volume.stats=volume.stats, 
		scene.stats=scene.stats, scene.chars=scene.chars,
		ret.seq=TRUE, pub.order=pub.order
	)
	# possibly set weights
	if(weighted)
	{	gs[["unfiltered"]] <- future_lapply(gs[["unfiltered"]], function(g) {E(g)$weight <- E(g)$Occurrences;return(g)})
		w.name <- "occurrences"
	}
	
	# compute the filtered version
	tlog(2,"Same thing for filtered graphs")
	gs[["filtered"]] <- future_lapply(gs[["unfiltered"]], function(g) delete_vertices(g, v=intersect(filt.names,V(g)$name)))
}

# scene range
if(is.na(sc.lim))
{	sc.rg <- 1:nrow(scene.stats)
}else
	sc.rg <- 1:sc.lim

# subfolder
if(pub.order)
{	ord.fold <- "publication"
}else
{	ord.fold <- "story"
}




###############################################################################
tlog(0,"Evolution of similarity between pairs of characters")

# similarity measures
sim.meas <- list()
sim.meas[["sim-cosine"]] <- list(
	bounds=c(0,1),
	cname="Cosine Similarity",
	foo=function(a,idx) {sapply(1:nrow(idx), function(r) sum(a[idx[r,1],]*a[idx[r,2],])/sqrt(sum(a[idx[r,1],]^2)*sum(a[idx[r,2],]^2)))}
)
sim.meas[["sim-pearson"]] <- list(
	bounds=c(-1,1),
	cname="Pearson Coefficient",
	foo=function(a,idx) {sapply(1:nrow(idx), function(r) cor(x=a[idx[r,1],], y=a[idx[r,2],]))}
)
sim.meas[["sim-euclidean"]] <- list(
	bounds=c(0,NA),
	cname="Euclidean Distance",
	foo=function(a,idx) {sapply(1:nrow(idx), function(r) sqrt(sum((a[idx[r,1],]-a[idx[r,2],])^2)))}
)
#sim.meas[["sim-regular"]] <- list(					# BEWARE: this one is extremely slow
#	bounds=c(0,NA),
#	cname="Regular Equivalence",
#	foo=function(a,idx) {tmp <- REGE.for(M=a,E=0)$E; sapply(1:nrow(idx), function(r) tmp[idx[r,1],idx[r,2]])}
#)

# plot parameters
pal <- ATT_COLORS_FILT[c("Discard","Keep")]
if(wide)
{	pw.pdf <- 15; ph.pdf <- 5
	pw.png <- 2400; ph.png <- 800
}else
{	pw.pdf <- 7; ph.pdf <- 7
	pw.png <- 800; ph.png <- 800
}

# targeted pairs of characters
pairs <- matrix(c(
	"Thorgal", "Aaricia",
	"Thorgal", "Jolan"
),
ncol=2, byrow=T)
rownames(pairs) <- apply(pairs, 1, function(row) paste0(row,collapse="--"))

# get full names
idx <- cbind(match(pairs[,1],char.stats[,COL_NAME_SHORT]), match(pairs[,2],char.stats[,COL_NAME_SHORT]))
fnames <- cbind(char.stats[idx[,1],COL_NAME], char.stats[idx[,2],COL_NAME])

# compute and plot the similarity values
tlog(2,"Looping over similarity measures")
for(m in 1:length(sim.meas))
{	tlog(3,"Processing measure \"",names(sim.meas)[m],"\" (",m,"/",length(sim.meas),")")
	sim.vals <- list()
	
	# process unfiltered and filtered networks
	for(filt in c("unfiltered","filtered"))
	{	tlog(4,"Processing the ",filt," network")
		if(filt=="unfiltered")
			col <- pal["Discard"]
		else
			col <- pal["Keep"]
		
		#####
		# compute the similarity between all char pairs, for each graph of the sequence
		tlog(5,"Looping over all graphs in the sequence")
		sims <- t(sapply(sc.rg, function(s) 
		{	if(s==min(sc.rg) || s %% 500==0 || s==max(sc.rg))
				tlog(6,"Processing scene ",s,"/",max(sc.rg))
			sim <- rep(NA, nrow(pairs))
			g <- gs[[filt]][[s]]
			vs <- cbind(match(fnames[,1],V(g)$name), match(fnames[,2],V(g)$name))
			idx <- which(!apply(vs, 1, function(row) any(is.na(row))))
			if(length(idx)>0)
			{	if(weighted)
					a <- as_adjacency_matrix(graph=g, type="both", sparse=FALSE, att="weight")
				else
					a <- as_adjacency_matrix(graph=g, type="both", sparse=FALSE)
				sim[idx] <- sim.meas[[m]]$foo(a, vs[idx,,drop=FALSE])
			}
			return(sim)
		}))
		colnames(sims) <- rownames(pairs)
		
		# record results
		if(is.na(sc.lim))
		{	pt <- names(sim.meas)[m]
			file <- get.path.stats.topo(net.type=net.type, order=ord.fold, mode="scenes", weights=w.name, meas.name=MEAS_MULTI_NODEPAIRS, filtered=filt, suf=pt)
			tlog(6,"Recording results to file \"",file,"\"")
			write.csv(x=sims, file=paste0(file,".csv"), row.names=FALSE)
		}
		
		#####
		# plot the obtained values for each character pair
		tlog(5,"Looping over the pairs of vertices")
		for(p in 1:nrow(pairs))
		{	tlog(6,"Processing pair ",pairs[p,1],"--",pairs[p,2]," (",p,"/",nrow(pairs),")")
			
			# set file name
			pt <- paste0(names(sim.meas)[m],"_pair=", paste0(pairs[p,],collapse="--"), if(wide) "_wide" else "")
			plot.file <- get.path.stats.topo(net.type=net.type, order=ord.fold, mode="scenes", weights=w.name, meas.name=MEAS_MULTI_NODEPAIRS, filtered=filt, suf=pt)
			tlog(7,"Creating file \"",plot.file,"\"")
			
			# compute data ranges
			xlim <- range(sc.rg)
			ylim <- range(sims[,p], na.rm=TRUE)
			ylim[2] <- ylim[2]*1.1	# add some space for volume names
			
			# produce file
			for(fformat in PLOT_FORMAT)
			{	if(fformat==PLOT_FORMAT_PDF)
					pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), width=pw.pdf, height=ph.pdf, bg="white")
				else if(fformat==PLOT_FORMAT_PNG)
					png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=pw.png, height=ph.png, units="px", pointsize=20, bg="white")
				# init empty plot
				par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
				plot(
					NULL,
					xlim=xlim, ylim=ylim,				# ylim=sim.meas[[m]]$bounds,
					xlab="Scenes", ylab=sim.meas[[m]]$cname,
					las=1
				)
				# add volume representations
				if(wide)
					draw.volume.rects(ylim, volume.stats)
				# horizontal dotted line
				if(names(sim.meas)[m]=="pearson")
					abline(h=0, lty=2)
				# add line
				lines(
					x=sc.rg, y=sims[,p], 
					col=col
				)
				# close file
				dev.off()
	
			}
			sim.vals[[filt]] <- sims
		}
	}
	
	#####
	# plot unfiltered and filtered values on the same plot
	tlog(4,"Plotting both unfiltered and filtered networks at once")
	
	tlog(5,"Looping over the pairs of vertices")
	for(p in 1:nrow(pairs))
	{	tlog(6,"Processing pair ",pairs[p,1],"--",pairs[p,2]," (",p,"/",nrow(pairs),")")
		
		# set file name
		pt <- paste0(names(sim.meas)[m],"_pair=", paste0(pairs[p,],collapse="--"), if(wide) "_wide" else "")
		plot.file <- get.path.stats.topo(net.type=net.type, order=ord.fold, mode="scenes", weights=w.name, meas.name=MEAS_MULTI_NODEPAIRS, filtered="both", suf=pt)
		tlog(7,"Creating file \"",plot.file,"\"")
		
		# compute data ranges
		xlim <- range(sc.rg)
		ylim <- range(c(sim.vals[["unfiltered"]][,p], sim.vals[["filtered"]][,p]), na.rm=TRUE)
		ylim[2] <- ylim[2]*1.1	# add some space for volume names
		
		# produce file
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), width=pw.pdf, height=ph.pdf, bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=pw.png, height=ph.png, units="px", pointsize=20, bg="white")
			# init empty plot
			par(mar=c(4,4,0,0)+0.1)	# remove the title space Bottom Left Top Right
			plot(
				NULL,
				xlim=xlim, ylim=ylim,				# ylim=sim.meas[[m]]$bounds,
				xlab="Scenes", ylab=sim.meas[[m]]$cname,
				las=1
			)
			# add volume representations
			if(wide)
				draw.volume.rects(ylim, volume.stats)
			# add line
			for(filt in c("unfiltered","filtered"))
			{	if(filt=="unfiltered")
					col <- pal["Discard"]
				else
					col <- pal["Keep"]
				lines(
					x=sc.rg, y=sim.vals[[filt]][,p], 
					col=col
				)
			}
			# horizontal dotted line
			if(names(sim.meas)[m]=="pearson")
				abline(h=0, lty=2)
			# legend
			legend(
				title="Characters",
				x="bottomright",
				fill=pal[c("Discard","Keep")],
				legend=c("Unfiltered","Filtered")
			)
			# close file
			dev.off()
		}
	}
}




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
