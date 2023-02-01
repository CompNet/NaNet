# This script contains functions related to the computation of descriptive
# statistics for the previously extracted window-based networks.
# 
# Vincent Labatut
# 02/2019
###############################################################################




###############################################################################
# Computes all preselected nodal comparison measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration".
# filtered: whether to use the filter version of the graph.
#
# returns: an nxk table containing all computed values, where n is the number of
#          nodes and k the number of measures.
###############################################################################
compute.static.nodecomp.statistics <- function(g, mode, char.det=char.det, window.size=NA, overlap=NA, weights=NA, filtered=NA)
{	filt.txt <- if(filtered) "filtered" else "unfiltered"
	object <- "nodecomp"
	table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, filtered=filt.txt, compare=TRUE)
	tlog(4,"Computing nodal comparison measures for \"",table.file,"\"")
	
	if(!is.na(weights))
	{	if(weights=="none")
			nodecomp.meas <- NODECOMP_MEASURES[sapply(NODECOMP_MEASURES, function(meas) !meas$weighted)]
		else
			nodecomp.meas <- NODECOMP_MEASURES[sapply(NODECOMP_MEASURES, function(meas) meas$weighted)]
	}
	else
		nodecomp.meas <- NODECOMP_MEASURES
	if(filtered)
		nodecomp.meas <- nodecomp.meas[sapply(names(nodecomp.meas), function(meas.name) grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
	else
		nodecomp.meas <- nodecomp.meas[sapply(names(nodecomp.meas), function(meas.name) !grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
		
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	reset.flag <- FALSE
	if(file.exists(table.file))
	{	res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
		reset.flag <- nrow(res.tab)!=gorder(g)
	}
	if(!file.exists(table.file) || reset.flag)
	{	res.tab <- matrix(NA, nrow=gorder(g), ncol=length(nodecomp.meas))
		colnames(res.tab) <- names(nodecomp.meas)
		rownames(res.tab) <- V(g)$name
	}
	
	# compute each measure
	tlog(5,"Computing each nodal measure")
	for(m in 1:length(nodecomp.meas))
	{	meas.name <- names(nodecomp.meas)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(nodecomp.meas),")")
		
		# possibly add column if missing
		if(!(meas.name %in% colnames(res.tab)))
		{	res.tab <- cbind(res.tab, rep(NA, nrow(res.tab)))
			colnames(res.tab)[ncol(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!all(is.na(res.tab[,meas.name])) && !FORCE)
		{	tlog(7,"Measure already computed, using the existing values")
			values <- res.tab[,meas.name]
		}
		else
		{	# compute values
			measure <- nodecomp.meas[[m]]
			values <- measure$foo(graph=g)
			if(length(values)==0)
				values <- rep(NA,gorder(g))
			tlog(7,"Number of values: ",length(values))
			
			# update table
			res.tab[,meas.name] <- values
			
			# update file
			tlog(7,"Updating stat file '",table.file,"'")
			write.csv(x=res.tab, file=table.file, fileEncoding="UTF-8", row.names=TRUE)#, col.names=TRUE)
		}
		
		# plot
		if(grepl(SFX_OCC, meas.name, fixed=TRUE))
			subf <- "occ"
		else if(grepl(SFX_DUR, meas.name, fixed=TRUE))
			subf <- "dur"
		plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, filtered=filt.txt, suf=paste0(subf,"_histo"))
		tlog(7,"Plotting in file '",plot.file,"'")
		for(fformat in PLOT_FORMAT)
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
			else if(fformat==PLOT_FORMAT_PNG)
				png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
				hist(
					values,
					col=MAIN_COLOR
				)
			dev.off()
		}
	}
	
	# plot the TP, FP and FN values for all characters / only the main ones
	if(!is.na(weights))
	{	if(weights=="none")
		{	if(!filtered)
			{	ms <- rbind(
					c(paste0(MEAS_TRUEPOS, SFX_DUR), paste0(MEAS_FALSENEG, SFX_DUR, paste0(MEAS_FALSEPOS, SFX_DUR))), 
					c(paste0(MEAS_TRUEPOS, SFX_OCC), paste0(MEAS_FALSENEG, SFX_OCC, paste0(MEAS_FALSEPOS, SFX_OCC)))
				)
			}
			else
			{	ms <- rbind(
					c(paste0(MEAS_TRUEPOS, SFX_FILTERED, SFX_DUR), paste0(MEAS_FALSENEG, SFX_FILTERED, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_FILTERED, SFX_DUR)), 
					c(paste0(MEAS_TRUEPOS, SFX_FILTERED, SFX_OCC), paste0(MEAS_FALSENEG, SFX_FILTERED, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_FILTERED, SFX_OCC))
				)
			}
		}
		else
		{	if(!filtered)
			{	ms <- rbind(
					c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_DUR)), 
					c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_NORM, SFX_DUR)), 
					c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_OCC)), 
					c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_NORM, SFX_OCC)) 
				)
			}
			else
			{	ms <- rbind(
					c(paste0(MEAS_TRUEPOS, SFX_FILTERED, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSENEG, SFX_FILTERED, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_FILTERED, SFX_WEIGHT, SFX_DUR)), 
					c(paste0(MEAS_TRUEPOS, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSENEG, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_DUR)), 
					c(paste0(MEAS_TRUEPOS, SFX_FILTERED, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSENEG, SFX_FILTERED, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_FILTERED, SFX_WEIGHT, SFX_OCC)), 
					c(paste0(MEAS_TRUEPOS, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSENEG, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_FILTERED, SFX_WEIGHT, SFX_NORM, SFX_OCC)) 
				)
			}
		}
	}
	
	idx.keep <- order(V(g)$Frequency, decreasing=TRUE)[1:min(gorder(g),10)]
	for(m in 1:nrow(ms))
	{	meas.name <- ms[m,1]
		tlog(6,"Plotting measure \"",meas.name,"\" (",m,"/",nrow(ms),")")
		
		mds1 <- c("freq", "prop")
		for(md1 in mds1)
		{	mds2 <- c("all","main")
			for(md2 in mds2)
			{	# all vs main
				if(md2=="main")
					idx <- idx.keep
				else
					idx <- 1:nrow(res.tab)
				
				# prop vs freq
				data <- t(res.tab[idx,ms[m,]])
				if(md1=="prop")
				{	data <- data/matrix(rep(colSums(data),3), ncol=ncol(data), byrow=TRUE)
					ylim <- c(0,1)
					ylab <- "Proportion"
				}
				else
				{	ylim <- c(0,max(colSums(data)))
					ylab <- "Frequency"
				}
				
				if(grepl(SFX_OCC, meas.name, fixed=TRUE))
					subf <- "occ"
				else if(grepl(SFX_DUR, meas.name, fixed=TRUE))
					subf <- "dur"
				if(grepl(SFX_NORM, meas.name, fixed=TRUE))
					subf <- paste0(subf,"_norm")
				else
					subf <- paste0(subf,"_raw")
				plot.file <- get.path.stats.comp(mode=mode, char.det=char.det, net.type="static", meas.name=MEAS_TFPN_NODES, window.size=window.size, overlap=overlap, weights=weights, filtered=filt.txt, suf=paste0(subf,"_",md2,"_",md1,"_barplot"))
				tlog(7,"Plotting in file '",plot.file,"'")
				for(fformat in PLOT_FORMAT)
				{	if(fformat==PLOT_FORMAT_PDF)
						pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
					else if(fformat==PLOT_FORMAT_PNG)
						png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						barplot(
							data,
							col=get.palette(3),
							ylim=ylim,
							xlab="Character",
							ylab=ylab,
							legend.text=c("TP","FN","FP"),
							names.arg=V(g)$ShortName[idx], las=2,
							main=paste0("mode=",mode," window.size=",window.size," overlap=",overlap)
						)
					dev.off()
				}
			}
		}
	}
	
	tlog(4,"Computation of nodal comparison measures complete")
	return(res.tab)
}




###############################################################################
# Computes all preselected graph comparison measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# filtered: whether to use the filter version of the graph.
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
compute.static.graphcomp.statistics <- function(g, mode, char.det=NA, window.size=NA, overlap=NA, weights=NA, filtered=NA)
{	filt.txt <- if(filtered) "filtered" else "unfiltered"
	object <- "graphcomp"
	table.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, filtered=filt.txt, compare=TRUE)
	tlog(4,"Computing graph comparison measures")
	
	if(!is.na(weights))
	{	if(weights=="none")
			graphcomp.meas <- GRAPHCOMP_MEASURES[sapply(GRAPHCOMP_MEASURES, function(meas) !meas$weighted)]
		else
			graphcomp.meas <- GRAPHCOMP_MEASURES[sapply(GRAPHCOMP_MEASURES, function(meas) meas$weighted)]
	}
	else
		graphcomp.meas <- GRAPHCOMP_MEASURES
	if(filtered)
		graphcomp.meas <- graphcomp.meas[sapply(names(graphcomp.meas), function(meas.name) grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
	else
		graphcomp.meas <- graphcomp.meas[sapply(names(graphcomp.meas), function(meas.name) !grepl(SFX_FILTERED, meas.name, fixed=TRUE))]
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA,nrow=length(names(graphcomp.meas)),ncol=1)
		rownames(res.tab) <- names(graphcomp.meas)
		colnames(res.tab) <- c("Value")
	}
	
	# compute each topological and comparison measure
	tlog(5,"Computing each graph comparison measure")
	for(m in 1:length(graphcomp.meas))
	{	meas.name <- names(graphcomp.meas)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(graphcomp.meas),")")
		
		# possibly add row if missing
		if(!(meas.name %in% rownames(res.tab)))
		{	res.tab <- rbind(res.tab, NA)
			rownames(res.tab)[nrow(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!is.na(res.tab[meas.name,1]) && !FORCE)
		{	tlog(7,"Measure already computed, using the existing values")
			value <- res.tab[meas.name,1]
		}
		else
		{	# compute value
			measure <- graphcomp.meas[[m]]
			value <- measure$foo(graph=g)
			tlog(7,"Value: ",value)
			
			# update table
			res.tab[meas.name,1] <- value
			
			# update file
			tlog(7,"Updating stat file '",table.file,"'")
			write.csv(x=res.tab, file=table.file, fileEncoding="UTF-8", row.names=TRUE)#, col.names=FALSE)
		}
	}
	
	tlog(4,"Computation of graph comparison measures complete")
	return(res.tab)
}




###############################################################################
# Computes the correlation between the previously computed topological measures 
# for the specified static graph.
#
# mode: either "scenes", "panel.window", or "page.window".
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether to use the filter version of the graph.
#
# returns: a kx4 table containing all computed values, where k is the number of measures,
#		   and the columns correspond to Spearman's correlation and the associated p-value,
#          relatively to the duration and occurrences scene-based networks.
###############################################################################
compute.static.correlations <- function(mode, char.det=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=NA)
{	filt.txt <- if(filtered) "filtered" else "unfiltered"
	table.file <- get.path.stat.table(object="correlation", mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filt.txt, compare=TRUE)
	tlog(4,"Computing rank correlation measures for \"",table.file,"\"")
	
	# select measures
	if(!is.na(weights))
	{	if(weights=="none")
		{	mn <- c(names(NODE_MEASURES)[sapply(NODE_MEASURES, function(meas) !meas$weighted)], names(NODEPAIR_MEASURES)[sapply(NODEPAIR_MEASURES, function(meas) !meas$weighted)])
			cn <- c(COL_NONE_SPEAR, COL_NONE_PVAL)
		}
		else
		{	mn <- c(names(NODE_MEASURES)[sapply(NODE_MEASURES, function(meas) meas$weighted)], names(NODEPAIR_MEASURES)[sapply(NODEPAIR_MEASURES, function(meas) meas$weighted)])
			cn <- c(COL_DUR_SPEAR, COL_DUR_PVAL, COL_OCC_SPEAR, COL_OCC_PVAL)
		}
		# no link-related measure, as their number can vary from one graph to the other
	}
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA,nrow=length(mn),ncol=length(cn))
		rownames(res.tab) <- mn
		colnames(res.tab) <- cn
	}
	
	# retrieve each measure
	tlog(5,"Computing each nodal/node-pair measure")
	for(m in 1:length(mn))
	{	meas.name <- mn[m]
		tlog(6,"Computing rank correlation for measure ",meas.name," (",m,"/",length(mn),")")
		
		# possibly add row if missing
		if(!(meas.name %in% rownames(res.tab)))
		{	res.tab <- rbind(res.tab, rep(NA, ncol(res.tab)))
			rownames(res.tab)[nrow(res.tab)] <- meas.name
		}
		
		# check if already computed
#		if(!all(is.na(res.tab[meas.name,])) && !FORCE)
#		{	tlog(7,"Measure already computed, using the existing values")
#			#value <- res.tab[meas.name,1]
#		}
#		else
		{	# get object
			if(meas.name %in% names(NODE_MEASURES))
				object <- "nodes"
			else if(meas.name %in% names(NODECOMP_MEASURES))
				object <- "nodecomp"
			else if(meas.name %in% names(NODEPAIR_MEASURES))
				object <- "nodepairs"
			else if(meas.name %in% names(LINK_MEASURES))
				object <- "links"
			
			# retrieve tested values
			tab.file <- get.path.stat.table(object=object, mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filt.txt, compare=FALSE)
			tmp.tab <- as.matrix(read.csv(tab.file, header=TRUE, check.names=FALSE, row.names=1))
			vals.cur <- tmp.tab[,meas.name]
			
			# distinguish the weighted and unweighted cases
			if(is.na(weights) || weights=="none")
			{	# retrieve reference values
				vals.none <- load.static.nodelink.stats.scenes(weights="none", measure=meas.name, arc=arc, vol=vol, filtered=filt.txt, compare=FALSE)
				
				# compute correlations
				#corr <- cor.test(x=vals.none, y=vals.cur, method="spearman")
				corr <- tryCatch(
						cor.test(x=vals.none, y=vals.cur, method="spearman", exact=FALSE),
#						warning=function(w) 
#						{	tlog(7,"WARNING: ",w)
#						},
						error=function(e)
						{	msg <- paste0("ERROR: problem when computing Spearman for measure ",meas.name," (duration)")
							tlog(7,msg)
							warning(msg)
							corr <- list(estimate=NA,p.value=NA)
							return(corr)
						}
				)
				if(meas.name %in% rownames(res.tab))
					res.tab[meas.name,c(COL_NONE_SPEAR,COL_NONE_PVAL)] <- c(corr$estimate, corr$p.value)
				else
				{	res.tab <- rbind(meas.name, c(corr$estimate, corr$p.value))
					rownames(res.tab)[nrow(res.tab)] <- meas.name
				}
			}
			else
			{	# retrieve reference values
				vals.dur <- load.static.nodelink.stats.scenes(weights="duration", measure=meas.name, arc=arc, vol=vol, filtered=filt.txt, compare=FALSE)
				vals.occ <- load.static.nodelink.stats.scenes(weights="occurrences", measure=meas.name, arc=arc, vol=vol, filtered=filt.txt, compare=FALSE)
				
				# compute correlations
				#corr <- cor.test(x=vals.dur, y=vals.cur, method="spearman")
				corr <- tryCatch(
						cor.test(x=vals.dur, y=vals.cur, method="spearman", exact=FALSE),
						error=function(e)
						{	msg <- paste0("ERROR: problem when computing Spearman for measure ",meas.name," (duration)")
							tlog(7,msg)
							warning(msg)
							corr <- list(estimate=NA,p.value=NA)
							return(corr)
						}
				)
				if(meas.name %in% rownames(res.tab))
					res.tab[meas.name,c(COL_DUR_SPEAR,COL_DUR_PVAL)] <- c(corr$estimate, corr$p.value)
				else
				{	res.tab <- rbind(meas.name, c(corr$estimate, corr$p.value))
					rownames(res.tab)[nrow(res.tab)] <- meas.name
				}
				#corr <- cor.test(x=vals.occ, y=vals.cur, method="spearman")
				corr <- tryCatch(
						cor.test(x=vals.occ, y=vals.cur, method="spearman", exact=FALSE),
						error=function(e) 
						{	msg <- paste0("ERROR: problem when computing Spearman for measure ",meas.name, "(occurrences)")
							tlog(7,msg)
							warning(msg)
							corr <- list(estimate=NA,p.value=NA)
							return(corr)
						}
				)		
				if(meas.name %in% rownames(res.tab))
					res.tab[meas.name,c(COL_OCC_SPEAR,COL_OCC_PVAL)] <- c(corr$estimate, corr$p.value)
				else
				{	res.tab <- rbind(meas.name, c(corr$estimate, corr$p.value))
					rownames(res.tab)[nrow(res.tab)] <- meas.name
				}
			}
			
			
			# update file
			write.csv(x=res.tab, file=table.file, fileEncoding="UTF-8", row.names=TRUE)#, col.names=TRUE)
		}
	}
	
	tlog(4,"Computation of rank correlation measures complete")
	return(res.tab)
}




###############################################################################
# Computes all rank correlation measures for the specified static graph.
#
# mode: either "scenes", "panel.window", or "page.window".
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
# arc: the narrative arc to plot (optional).
# vol: the volume to plot (optional, and ignored if arc is specified).
# filtered: whether to use the filter version of the graph.
###############################################################################
compute.all.static.corrs <- function(mode, char.det=NA, window.size=NA, overlap=NA, weights=NA, arc=NA, vol=NA, filtered=NA)
{	graph.file <- get.path.data.graph(mode=mode, char.det=char.det, net.type="static", window.size=window.size, overlap=overlap, arc=arc, vol=vol, filtered=filtered, pref="graph", ext=".graphml")
	tlog(3,"Computing all rank correlation measures for \"",graph.file,"\"")
	
#	# read the graph file
#	tlog(4,"Loading graph")
#	g <- read.graphml.file(file=graph.file)
#	if(!is.na(weights))
#	{	if(weights=="occurrences")
#			E(g)$weight <- E(g)$Occurrences
#		else if(weights=="duration")
#			E(g)$weight <- E(g)$Duration
#	}
# not needed!
	
	# init cache
	cache <<- list()
	
	# compute its stats
	compute.static.correlations(mode=mode, char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, arc=arc, vol=vol, filtered=filtered)
	
	tlog(3,"Computation of all rank correlation measures complete")
}




###############################################################################
# Main function for the computation of statistics describing static graphs.
# The graphs must have been previously extracted.
#
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# panel.params: panel-related parameters.
# page.params: page-related parameters.
###############################################################################
compute.static.statistics.window <- function(char.det=NA, panel.params, page.params)
{	tlog(1,"Computing statistics for window-based static graphs")
	
	# retrieve parameters
	panel.window.sizes <- panel.params$window.sizes
	panel.overlaps <- panel.params$overlaps
	page.window.sizes <- page.params$window.sizes
	page.overlaps <- page.params$overlaps
	
	# statistics for the panel window-based static graphs
	tlog(2,"Dealing with panel-based windows")
	#future_sapply(1:length(panel.window.sizes), function(i) >> cache interaction pb
	for(i in 1:length(panel.window.sizes))
	{	window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
		{	for(weights in c("none","occurrences"))
			{	for(filtered in c(FALSE,TRUE))
					compute.static.all.statistics(mode="panel.window", char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=FALSE)
			}
		}
	}#)
	
	# statistics for the page window-based static graphs
	tlog(2,"Dealing with page-based windows")
	#future_sapply(1:length(page.window.sizes), function(i) >> cache interaction pb
	for(i in 1:length(page.window.sizes))
	{	window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
		{	for(weights in c("none","occurrences"))
			{	for(filtered in c(FALSE,TRUE))
					compute.static.all.statistics(mode="page.window", char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=FALSE)
			}
		}
	}#)
	
	tlog(1,"Computation of statistics for window-based static graphs complete")	
}




###############################################################################
# Main function for the comparison of graphs. Statistics must have been computed
# beforehand.
#
# data: preprocessed data.
# char.det: character detection mode ("implicit" or "explicit", NA if not relevant).
# panel.params: panel-related parameters.
# page.params: page-related parameters.
###############################################################################
compute.static.statistics.comparison <- function(data, char.det=NA, panel.params, page.params)
{	tlog(1,"Computing comparisons for static graphs")
	
	# retrieve parameters
	panel.window.sizes <- panel.params$window.sizes
	panel.overlaps <- panel.params$overlaps
	page.window.sizes <- page.params$window.sizes
	page.overlaps <- page.params$overlaps
	
	#### scene-based graphs
	if(char.det=="implicit")
	{	tlog(1,"Dealing with scene-based graphs")
		# loop over unfiltered/filtered networks
		for(filtered in c(FALSE,TRUE))
		{	tlog(2,"Processing filtered=",filtered)
			
			# comparison for the scene-based graphs
			for(weights in c("none","occurrences","duration"))
			{	compute.static.all.statistics(mode="scenes", char.det=char.det, weights=weights, filtered=filtered, compare=TRUE)
				compute.all.static.corrs(mode="scenes", char.det=char.det, weights=weights, filtered=filtered)
			}
			
			# correlations only for each narrative arc
			arc.nbr <- nrow(data$arc.stats)
			for(arc in 1:arc.nbr)
			{	for(weights in c("none","occurrences","duration"))
					compute.all.static.corrs(mode="scenes", char.det=char.det, weights=weights, arc=arc, filtered=filtered)
			}
			# correlations only for each volume
			volume.nbr <- nrow(data$volume.stats)
			for(v in 1:volume.nbr)
			{	vol <- paste0(v,"_",data$volume.stats[v, COL_VOLUME])
				for(weights in c("none","occurrences","duration"))
					compute.all.static.corrs(mode="scenes", char.det=char.det, weights=weights, vol=vol, filtered=filtered)
			}
		}
	}
			
	#### window-based graphs
	tlog(1,"Dealing with window-based graphs")
	# loop over weight types
	for(weights in c("none","occurrences"))
	{	# loop over unfiltered/filtered networks
		for(filtered in c(FALSE,TRUE))
		{	tlog(2,"Processing weights=",weights," filtered=",filtered)
			
			# statistics for the panel window-based static graphs
			#future_sapply(1:length(panel.window.sizes), function(i) >> cache interaction pb
			for(i in 1:length(panel.window.sizes))
			{	window.size <- panel.window.sizes[i]
				for(overlap in panel.overlaps[[i]])
				{	compute.static.all.statistics(mode="panel.window", char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=TRUE)
					compute.all.static.corrs(mode="panel.window", char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, filtered=filtered)
				}
			}#)
			
			# statistics for the page window-based static graphs
			#future_sapply(1:length(page.window.sizes), function(i) >> cache interaction pb
			for(i in 1:length(page.window.sizes))
			{	window.size <- page.window.sizes[i]
				for(overlap in page.overlaps[[i]])
				{	compute.static.all.statistics(mode="page.window", char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, filtered=filtered, compare=TRUE)
					compute.all.static.corrs(mode="page.window", char.det=char.det, window.size=window.size, overlap=overlap, weights=weights, filtered=filtered)
				}
			}#)
		}
	}
	
	tlog(1,"Computation of comparisons for static graphs complete")	
}
