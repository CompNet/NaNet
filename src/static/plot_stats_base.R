# This script contains functions related to the generation of plots representing
# the previously computed statistics.
# 
# Vincent Labatut
# 02/2019
#
# source("src/static/plot_stats_base.R")
###############################################################################




###############################################################################
# Loads a value corresponding to the specified parameters.
#
# weights: either "occurrences" or "duration".
# measure: name of the concerned topological measure.
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, ignored if arc is specified).
# filtered: whether to use the filter version of the graph.
# compare: whether to compute the regular stats or to compare with reference graphs.
#
# returns: the value corresponding to the specified parameters.
###############################################################################
load.static.graph.stats.scenes <- function(weights, measure, arc=NA, vol=NA, filtered=NA, compare=NA)
{	object <- ALL_MEASURES[[measure]]$object
	table.file <- get.path.stat.table(object=object, mode="scenes", char.det="implicit", net.type="static", weights=weights, arc=arc, vol=vol, filtered=filtered, compare=compare)
	tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	res <- tmp.tab[measure,1]
	return(res)
}




###############################################################################
# Loads a series corresponding to the scene-based graph.
#
# weights: either "occurrences" or "duration".
# measure: name of the concerned topological measure.
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, ignored if arc is specified).
# filtered: whether to use the filter version of the graph.
# compare: whether to compute the regular stats or to compare with reference graphs.
#
# returns: a vector representing the link/node values for the specified measure.
###############################################################################
load.static.nodelink.stats.scenes <- function(weights, measure, arc=NA, vol=NA, filtered=NA, compare=NA)
{	object <- ALL_MEASURES[[measure]]$object
	table.file <- get.path.stat.table(object=object, mode="scenes", char.det="implicit", net.type="static", weights=weights, arc=arc, vol=vol, filtered=filtered, compare=compare)
#print(table.file)
	tmp.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
#print(colnames(tmp.tab))
	res <- tmp.tab[,measure]
	return(res)
}




###############################################################################
# Generates the plots related to the topological measures obtained on the
# scene-based graph, for both types of weights (occurrences, durations).
# The stats must have been computed beforehand.
#
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether to use the filtered version of the graph.
###############################################################################
generate.static.plots.scene <- function(arc=NA, vol=NA, filtered=FALSE)
{	if(filtered)
	{	col <- ATT_COLORS_FILT["Keep"]
		filt.txt <- "filtered"
	}
	else
	{	col <- ATT_COLORS_FILT["Discard"]
		filt.txt <- "unfiltered"
	}
	tlog(3,"Generating plots for the ",filt.txt," scene-based graphs")
	mode <- "scenes"
	char.det <- "implicit"
	
	# list measures to plot
	nmn <- names(NODE_MEASURES)
	lmn <- names(LINK_MEASURES)
	npmn <- names(NODEPAIR_MEASURES)
	
	# plot each measure
	for(meas.name in c(nmn,lmn))
	{	tlog(4,"Generating plots for measure ",meas.name)
		
#		if(meas.name %in% nmn)
#			object <- "nodes"
#		else if(meas.name %in% lmn)
#			object <- "links"
#		else if(meas.name %in% npmn)
#			object <- "nodepairs"
		
		if(ALL_MEASURES[[meas.name]]$weighted)
			wmodes <- c("occurrences","duration")
		else
			wmodes <- c("none")
		
		# process each type of weight
		for(wmode in wmodes)
		{	tlog(4,"Dealing with weights=",wmode)
			
			# load pre-computed values (scene-based graph)
			vals <- load.static.nodelink.stats.scenes(weights=wmode, measure=meas.name, arc=arc, vol=vol, filtered=filt.txt, compare=FALSE)
			# remove possible NAs
			vals <- vals[!is.na(vals)]
			#vals <- vals[vals>0]	# remove the zeroes?
			disc <- all(vals%%1==0) 
			
			# plot histogram
			plot.file <- get.path.stats.topo(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, weights=wmode, arc=arc, vol=vol, filtered=filt.txt, suf="histo")
			ml <- paste0("weights=",wmode)
			xl <- ALL_MEASURES[[meas.name]]$cname
			test <- is.na(arc) && is.na(vol)	# test the type of distribution: very slow, doing it only for the whole graph
			if(disc)
			{	plot.disc.distribution(
					vals=vals, 
					xlab=xl, main=ml, 
					freq=FALSE,
					log=TRUE, cols=col, 
					#leg.title=NA, leg.pos="topright", las=1, 
					export=TRUE, file=plot.file, 
					histo=TRUE, ccdf=TRUE, test=test
				)
			}
			else
			{	plot.cont.distribution(
					vals=vals, 
					xlab=xl, main=ml, 
					breaks=20, 
					freq=FALSE, 
					log=TRUE, cols=col, 
					#leg.title=NA, leg.pos="topright", las=1, 
					export=FALSE, file=plot.file, 
					histo=TRUE, ccdf=TRUE, test=test
				)
			}
		}
	}
	
	# compute and plot additional stuff (not for volume- or arc-specific graphs)
	if(is.na(vol) && is.na(arc))
	{	# read the graph
		graph.file <- get.path.data.graph(mode="scenes", char.det="implicit", net.type="static", filtered=FALSE, pref="graph", ext=".graphml")
		g <- read.graphml.file(file=graph.file)
		if(filtered)
			g <- delete_vertices(graph=g, v=which(V(g)$Filter=="Discard"))
		g.dur <- g; E(g.dur)$weight <- E(g)$Duration
		g.occ <- g; E(g.occ)$weight <- E(g)$Occurrences
		
		# degree vs. neighbors' degree
		filename <- get.path.stats.topo(mode=mode, char.det=char.det, net.type="static", meas.name=MEAS_MULTI_NODES, weights="none", arc=arc, vol=vol, filtered=filt.txt)
		neigh.degree.vs.degree(g=g, weights=FALSE, filename=filename, col=col)
		for(wmode in c("occurrences","duration"))
		{	filename <- get.path.stats.topo(mode=mode, char.det=char.det, net.type="static", meas.name=MEAS_MULTI_NODES, weights=wmode, arc=arc, vol=vol, filtered=filt.txt)
			if(wmode=="duration")
				neigh.degree.vs.degree(g=g.dur, weights=TRUE, filename=filename, col=col)
			else if(wmode=="occurrences")
				neigh.degree.vs.degree(g=g.occ, weights=TRUE, filename=filename, col=col)
		}
		
		# degree vs. transitivity
		filename <- get.path.stats.topo(mode=mode, char.det=char.det, net.type="static", meas.name=MEAS_MULTI_NODES, weights="none", arc=arc, vol=vol, filtered=filt.txt)
		transitivity.vs.degree(g=g, weights=FALSE, filename=filename, col=col)
		for(wmode in c("occurrences","duration"))
		{	filename <- get.path.stats.topo(mode=mode, char.det=char.det, net.type="static", meas.name=MEAS_MULTI_NODES, weights=wmode, arc=arc, vol=vol, filtered=filt.txt)
			if(wmode=="duration")
				transitivity.vs.degree(g=g.dur, weights=TRUE, filename=filename, col=col)
			else if(wmode=="occurrences")
				transitivity.vs.degree(g=g.occ, weights=TRUE, filename=filename, col=col)
		}
		
		# hop plots
		filename <- get.path.stats.topo(mode=mode, char.det=char.det, net.type="static", meas.name=MEAS_MULTI_NODEPAIRS, weights="none", arc=arc, vol=vol, filtered=filt.txt)
		hop.plot(g=g, weights=FALSE, filename=filename, col=col)
		for(wmode in c("occurrences","duration"))
		{	filename <- get.path.stats.topo(mode=mode, char.det=char.det, net.type="static", meas.name=MEAS_MULTI_NODEPAIRS, weights=wmode, arc=arc, vol=vol, filtered=filt.txt)
			if(wmode=="duration")
				hop.plot(g=g.dur, weights=TRUE, filename=filename, col=col)
			else if(wmode=="occurrences")
				hop.plot(g=g.occ, weights=TRUE, filename=filename, col=col)
		}
	}
}




###############################################################################
# Generates the plots showing the evolution of measures over arcs or volumes.
#
# data: preprocessed data.
# arcs: TRUE to process arcs, FALSE to process volumes.
# filtered: whether to use the filter version of the graph.
###############################################################################
generate.static.plots.evol <- function(data, arcs, filtered)
{	filt.txt <- if(filtered) "filtered" else "unfiltered"
	# init arc/vol-dependent variables
	if(arcs)
	{	emode <- "arc"
		items <- unique(data$volume.stats[,COL_ARC])
	}
	else
	{	emode <- "volume"
		items <- paste(1:nrow(data$volume.stats),"_",data$volume.stats[,COL_VOLUME],sep="")
	}
	# whether x labels are long or not
	long <- max(nchar(items)) > 2
		
	
	# init other variables
	tlog(3,"Generating ",emode,"-based evolution plots for the ",filt.txt," scene-based graphs")
	mode <- "scenes"
	char.det <- "implicit"
	col <- ATT_COLORS_FILT[if(filtered) "Keep" else "Discard"]
	
	# list measures to plot
	gmn <- names(GRAPH_MEASURES)
	
	# plot each measure
	for(meas.name in gmn)
	{	tlog(4,"Generating ",emode,"-based evolution plots for measure ",meas.name)
		#object <- "graph"
		
		if(ALL_MEASURES[[meas.name]]$weighted)
			wmodes <- c("occurrences","duration")
		else
			wmodes <- c("none")
		
		# process each type of weight
		for(wmode in wmodes)
		{	tlog(4,"Dealing with weights=",wmode)
			
			# load pre-computed values (scene-based graph)
			vals <- rep(NA, length(items))
			for(i in 1:length(items))
			{	vals[i] <- load.static.graph.stats.scenes(weights=wmode, measure=meas.name, 
							arc=if(arcs) i else NA, vol=if(arcs) NA else items[i], 
							filtered=filt.txt, compare=FALSE)
			}
			
			# generate barplots
			if(all(is.na(vals)))
				tlog(6,"WARNING: nothing to plot, all values are NAs")
			else
			{	file <- get.path.stats.topo(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, weights=wmode, arc=if(arcs) TRUE else NA, vol=if(arcs) NA else TRUE, filtered=filt.txt, suf="evolution")
				tlog(4,"Generating file ",file)
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						barplot(
							height=vals,
							names.arg=if(arcs) 1:length(items) else items,
							ylab=ALL_MEASURES[[meas.name]]$cname,
							xlab=if(arcs) "Narrative Arcs" else "Volumes",
							main=paste0("Evolution of ",ALL_MEASURES[[meas.name]]$cname," by ",if(arcs) "arc" else "volume"),
							col=col,
							las=if(long) 2 else 3
						)
					dev.off()
				}
			}
		}
	}
}




###############################################################################
# Main function for the generation of plots describing static graphs.
# The statistics must have been previously computed.
#
# data: preprocessed data.
###############################################################################
generate.static.plots.base <- function(data)
{	tlog(1,"Generating plots for scene-based static graphs")
	
	# deal with scene-based graphs
	tlog(2,"Generating plots for static graphs with scene-based windows")
	for(filtered in c(FALSE, TRUE))
		generate.static.plots.scene(filtered=filtered)
	
	# same for each narrative arc
	arc.nbr <- nrow(data$arc.stats)
	for(arc in 1:arc.nbr)
	{	for(filtered in c(FALSE, TRUE))
			generate.static.plots.scene(arc=arc, filtered=filtered)
	}
	
	# same for each volume
	volume.nbr <- nrow(data$volume.stats)
	for(v in 1:volume.nbr)
	{	vol <- paste0(v, "_", data$volume.stats[v, COL_VOLUME])
		for(filtered in c(FALSE, TRUE))
			generate.static.plots.scene(vol=vol, filtered=filtered)
	}
	
	# evolution plots
	for(flag in c(TRUE,FALSE))
	{	for(filtered in c(FALSE, TRUE))
			generate.static.plots.evol(data=data, arcs=flag, filtered=filtered)
	}
	
	tlog(1,"Generation of plots for scene-based static graphs complete")	
}
