# This script contains functions related to the computation of descriptive
# statistics for the previously extracted networks.
# 
# Vincent Labatut
# 02/2019
###############################################################################

# list used to cache certain (costly to compute) intermediary results
cache <- list()



###############################################################################
# Computes all preselected nodal topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: an nxk table containing all computed values, where n is the number of
#          nodes and k the number of measures.
###############################################################################
compute.static.node.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	object <- "nodes"
	table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing nodal topological measures for \"",table.file,"\"")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
	else
	{	res.tab <- matrix(NA, nrow=gorder(g), ncol=length(NODE_MEASURES))
		colnames(res.tab) <- names(NODE_MEASURES)
	}
	
	# compute each measure
	tlog(5,"Computing each nodal measure")
	for(m in 1:length(NODE_MEASURES))
	{	meas.name <- names(NODE_MEASURES)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(NODE_MEASURES),")")
		
		# possibly add column if missing
		if(!(meas.name %in% colnames(res.tab)))
		{	res.tab <- cbind(res.tab, rep(NA, nrow(res.tab)))
			colnames(res.tab)[ncol(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!all(is.na(res.tab[,meas.name])))
		{	tlog(7,"Measure already computed, using the existing values")
			values <- res.tab[,meas.name]
		}
		else
		{	# compute values
			measure <- NODE_MEASURES[[m]]
			values <- measure$foo(graph=g)
			if(length(values)==0)
				values <- rep(NA,gorder(g))
			tlog(7,"Number of values: ",length(values))
			
			# update table
			res.tab[,meas.name] <- values
			
			# update file
			write.csv(x=res.tab, file=table.file, row.names=FALSE)#, col.names=TRUE)
		}
		
		# plot
		plot.file <- get.path.topomeas.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, plot.type="histo")
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
	
	tlog(4,"Computation of nodal topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected nodal comparison measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: an nxk table containing all computed values, where n is the number of
#          nodes and k the number of measures.
###############################################################################
compute.static.nodecomp.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	object <- "nodescomp"
	table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing nodal comparison measures for \"",table.file,"\"")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
	else
	{	res.tab <- matrix(NA, nrow=gorder(g), ncol=length(NODECOMP_MEASURES))
		colnames(res.tab) <- names(NODECOMP_MEASURES)
	}
	
	# get reduced graph
	idx.red <- which(V(g)$Frequency>2)
	g.red <- induced_subgraph(g, v=idx.red)
	
	# compute each measure
	tlog(5,"Computing each nodal measure")
	for(m in 1:length(NODECOMP_MEASURES))
	{	meas.name <- names(NODECOMP_MEASURES)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(NODECOMP_MEASURES),")")
		
		# possibly add column if missing
		if(!(meas.name %in% colnames(res.tab)))
		{	res.tab <- cbind(res.tab, rep(NA, nrow(res.tab)))
			colnames(res.tab)[ncol(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!all(is.na(res.tab[,meas.name])))
		{	tlog(7,"Measure already computed, using the existing values")
			values <- res.tab[,meas.name]
		}
		else
		{	# compute values
			measure <- NODECOMP_MEASURES[[m]]
			if(grepl(SFX_REDUCED, meas.name, fixed=TRUE))
			{	values <- rep(NA,gorder(g))
				values[idx.red] <- measure$foo(graph=g.red)
			}
			else
				values <- measure$foo(graph=g)
			if(length(values)==0)
				values <- rep(NA,gorder(g))
			tlog(7,"Number of values: ",length(values))
			
			# update table
			res.tab[,meas.name] <- values
			
			# update file
			write.csv(x=res.tab, file=table.file, row.names=FALSE)#, col.names=TRUE)
		}
		
		# plot
		plot.file <- get.path.topomeas.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, plot.type="histo")
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
	
	# plot the TP, FP and FN values for the main characters
	ms <- rbind(
		c(paste0(MEAS_TRUEPOS, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_DUR), paste0(MEAS_FALSENEG, SFX_DUR)), 
		c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_DUR), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_DUR)), 
		c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_DUR)), 
		c(paste0(MEAS_TRUEPOS, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_OCC), paste0(MEAS_FALSENEG, SFX_OCC)), 
		c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_OCC), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_OCC)), 
		c(paste0(MEAS_TRUEPOS, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSENEG, SFX_WEIGHT, SFX_NORM, SFX_OCC)) 
	)
	rownames(ms) <- c(
		paste0("tfpn", SFX_DUR),
		paste0("tfpn", SFX_WEIGHT, SFX_DUR),
		paste0("tfpn", SFX_WEIGHT, SFX_NORM, SFX_DUR),
		paste0("tfpn", SFX_OCC),
		paste0("tfpn", SFX_WEIGHT, SFX_OCC),
		paste0("tfpn", SFX_WEIGHT, SFX_NORM, SFX_OCC)
	)
	
	idx.filtr <- order(V(g)$Frequency, decreasing=TRUE)[1:min(gorder(g),10)]
	for(m in 1:nrow(ms))
	{	meas.name <- rownames(ms)[m]
		tlog(6,"Plotting measure \"",meas.name,"\"")
		
		mds1 <- c("freq", "prop")
		for(md1 in mds1)
		{	mds2 <- c("all","main")
			for(md2 in mds2)
			{	# all vs main
				if(md2=="main")
					idx <- idx.filtr
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
				
				plot.file <- get.path.comparison.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, plot.type=paste0(md2,"_",md1,"_barplot"))
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
							legend.text=c("TP","FP","FN"),
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
# Computes all preselected node-pair topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: an n(n-1)/2 x k table containing all computed values, where n(n-1)/2 is the 
# 	       number of pairs of nodes and k the number of measures.
###############################################################################
compute.static.nodepair.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	object <- "nodepairs"
	table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing node-pair topological measures for \"",table.file,"\"")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	n <- gorder(g)
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
	else
	{	res.tab <- matrix(NA, nrow=n*(n-1)/2, ncol=length(NODEPAIR_MEASURES))
		colnames(res.tab) <- names(NODEPAIR_MEASURES)
	}
	
	# compute each measure
	tlog(5,"Computing each node-pair measure")
	for(m in 1:length(NODEPAIR_MEASURES))
	{	meas.name <- names(NODEPAIR_MEASURES)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(NODEPAIR_MEASURES),")")
		
		# possibly add column if missing
		if(!(meas.name %in% colnames(res.tab)))
		{	res.tab <- cbind(res.tab, rep(NA, nrow(res.tab)))
			colnames(res.tab)[ncol(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!all(is.na(res.tab[,meas.name])))
		{	tlog(7,"Measure already computed, using the existing values")
			values <- res.tab[,meas.name]
		}
		else
		{	# compute values
			measure <- NODEPAIR_MEASURES[[m]]
			values <- measure$foo(graph=g)
			if(length(values)==0)
				values <- rep(NA,n*(n-1)/2)
			tlog(7,"Number of values: ",length(values))
			
			# update table
			res.tab[,meas.name] <- values
			
			# update file
			write.csv(x=res.tab, file=table.file, row.names=FALSE)#, col.names=TRUE)
		}
		
		# plot
		if(!all(is.na(values)))
		{	plot.file <- get.path.topomeas.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, plot.type="histo")
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
	}
	
	tlog(4,"Computation of node-pair topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected link topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: an mxk table containing all computed values, where m is the number of
#          links and k the number of measures.
###############################################################################
compute.static.link.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	object <- "links"
	table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing link topological measures for \"",table.file,"\"")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE))
	else
	{	res.tab <- matrix(NA, nrow=gsize(g), ncol=length(LINK_MEASURES))
		colnames(res.tab) <- names(LINK_MEASURES)
	}
	
	# compute each measure
	tlog(5,"Computing each link measure")
	for(m in 1:length(LINK_MEASURES))
	{	meas.name <- names(LINK_MEASURES)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(LINK_MEASURES),")")
		
		# possibly add column if missing
		if(!(meas.name %in% colnames(res.tab)))
		{	res.tab <- cbind(res.tab, rep(NA, nrow(res.tab)))
			colnames(res.tab)[ncol(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!all(is.na(res.tab[,meas.name])))
		{	tlog(7,"Measure already computed, using the existing values")
			values <- res.tab[,meas.name]
		}
		else
		{	# compute values
			measure <- LINK_MEASURES[[m]]
			values <- measure$foo(graph=g)
			if(length(values)==0)
				values <- rep(NA,gsize(g))
			tlog(7,"Number of values: ",length(values))
			
			# update table
			res.tab[,meas.name] <- values
			
			# update file
			write.csv(x=res.tab, file=table.file, row.names=FALSE)#, col.names=TRUE)
		}
		
		# plot
		plot.file <- get.path.topomeas.plot(object=object, mode=mode, meas.name=meas.name, window.size=window.size, overlap=overlap, weights=weights, plot.type="histo")
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
	
	tlog(4,"Computation of link topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected graph topological measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
compute.static.graph.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	object <- "graph"
	table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing graph topological measures")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA, nrow=length(names(GRAPH_MEASURES)), ncol=1)
		rownames(res.tab) <- names(GRAPH_MEASURES)
		colnames(res.tab) <- c("Value")
	}
	
	# compute each topological and comparison measure
	tlog(5,"Computing each graph topological measure")
	measures <- GRAPH_MEASURES
	for(m in 1:length(measures))
	{	meas.name <- names(measures)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(measures),")")
		
		# possibly add row if missing
		if(!(meas.name %in% rownames(res.tab)))
		{	res.tab <- rbind(res.tab, NA)
			rownames(res.tab)[nrow(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!is.na(res.tab[meas.name,1]))
		{	tlog(7,"Measure already computed, using the existing values")
			value <- res.tab[meas.name,1]
		}
		else
		{	# compute value
			measure <- measures[[m]]
			value <- measure$foo(graph=g)
			tlog(7,"Value: ",value)
			
			# update table
			res.tab[meas.name,1] <- value
			
			# update file
			write.csv(x=res.tab, file=table.file, row.names=TRUE)#, col.names=FALSE)
		}
	}
	
	tlog(4,"Computation of graph topological measures complete")
	return(res.tab)
}



###############################################################################
# Computes all preselected graph comparison measures for the specified static graph.
#
# g: graph whose statistics must be computed.
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: a kx1 table containing all computed values, where k is the number of measures.
###############################################################################
compute.static.graphcomp.statistics <- function(g, mode, window.size=NA, overlap=NA, weights=NA)
{	object <- "graphcomp"
	table.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing graph comparison measures")
	
	# read or create the table containing the computed values
	tlog(5,"Getting/creating file \"",table.file,"\"")
	if(file.exists(table.file))
		res.tab <- as.matrix(read.csv(table.file, header=TRUE, check.names=FALSE, row.names=1))
	else
	{	res.tab <- matrix(NA,nrow=length(names(GRAPHCOMP_MEASURES)),ncol=1)
		rownames(res.tab) <- names(GRAPHCOMP_MEASURES)
		colnames(res.tab) <- c("Value")
	}
	
	# get reduced graph
	idx.red <- which(V(g)$Frequency>2)
	g.red <- induced_subgraph(g, v=idx.red)
	
	# compute each topological and comparison measure
	tlog(5,"Computing each graph comparison measure")
	measures <- GRAPHCOMP_MEASURES
	for(m in 1:length(measures))
	{	meas.name <- names(measures)[m]
		tlog(6,"Computing measure ",meas.name," (",m,"/",length(measures),")")
		
		# possibly add row if missing
		if(!(meas.name %in% rownames(res.tab)))
		{	res.tab <- rbind(res.tab, NA)
			rownames(res.tab)[nrow(res.tab)] <- meas.name
		}
		
		# check if already computed
		if(!is.na(res.tab[meas.name,1]))
		{	tlog(7,"Measure already computed, using the existing values")
			value <- res.tab[meas.name,1]
		}
		else
		{	# compute value
			measure <- measures[[m]]
			if(grepl(SFX_REDUCED, meas.name, fixed=TRUE))
				value <- measure$foo(graph=g.red)
			else
				value <- measure$foo(graph=g)
			tlog(7,"Value: ",value)
			
			# update table
			res.tab[meas.name,1] <- value
			
			# update file
			write.csv(x=res.tab, file=table.file, row.names=TRUE)#, col.names=FALSE)
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
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
#
# returns: a kx4 table containing all computed values, where k is the number of measures,
#		   and the columns correspond to Spearman's correlation and the associated p-value,
#          relatively to the duration and occurrences scene-based networks.
###############################################################################
compute.static.correlations <- function(mode, window.size=NA, overlap=NA, weights=NA)
{	table.file <- get.path.stat.table(object="corr", mode=mode, window.size=window.size, overlap=overlap, weights=weights)
	tlog(4,"Computing rank correlation measures for \"",table.file,"\"")
	
	mn <- c(names(NODE_MEASURES), names(NODEPAIR_MEASURES), names(NODECOMP_MEASURES)) # not links, as their number can vary from one graph to the other
	cn <- c(COL_SPEAR_DUR, COL_PVAL_DUR, COL_SPEAR_OCC, COL_PVAL_OCC) 
	
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
		if(!all(is.na(res.tab[meas.name,])))
		{	tlog(7,"Measure already computed, using the existing values")
			#value <- res.tab[meas.name,1]
		}
		else
		{	# get object
			if(meas.name %in% names(NODE_MEASURES))
				object <- "nodes"
			else if(meas.name %in% names(NODECOMP_MEASURES))
				object <- "nodescomp"
			else if(meas.name %in% names(NODEPAIR_MEASURES))
				object <- "nodepairs"
			else if(meas.name %in% names(LINK_MEASURES))
				object <- "links"
			
			# retrieve reference values
			vals.dur <- load.static.nodelink.stats.scenes(object=object, measure=meas.name, weights="duration")
			vals.occ <- load.static.nodelink.stats.scenes(object=object, measure=meas.name, weights="occurrences")
			
			# retrieve tested values
			tab.file <- get.path.stat.table(object=object, mode=mode, window.size=window.size, overlap=overlap, weights=weights)
			tmp.tab <- as.matrix(read.csv(tab.file, header=TRUE, check.names=FALSE))
			vals.cur <- tmp.tab[,meas.name]
			
			# compute correlations
			#corr <- cor.test(x=vals.dur, y=vals.cur, method="spearman")
			corr <- tryCatch(
				cor.test(x=vals.dur, y=vals.cur, method="spearman", exact=FALSE),
#				warning=function(w) 
#				{	tlog(7,"WARNING: ",w)
#				},
				error=function(e)
				{	msg <- paste0("ERROR: problem when computing Spearman for measure ",meas.name)
					tlog(7,msg)
					warning(msg)
					corr <- list(estimate=NA,p.value=NA)
					return(corr)
				}
			)
			if(meas.name %in% rownames(res.tab))
				res.tab[meas.name,c(COL_SPEAR_DUR,COL_PVAL_DUR)] <- c(corr$estimate, corr$p.value)
			else
			{	res.tab <- rbind(meas.name, c(corr$estimate, corr$p.value))
				rownames(res.tab)[nrow(res.tab)] <- meas.name
			}
			#corr <- cor.test(x=vals.occ, y=vals.cur, method="spearman")
			corr <- tryCatch(
				cor.test(x=vals.occ, y=vals.cur, method="spearman", exact=FALSE),
				error=function(e) 
				{	msg <- paste0("ERROR: problem when computing Spearman for measure ",meas.name)
					tlog(7,msg)
					warning(msg)
					corr <- list(estimate=NA,p.value=NA)
					return(corr)
				}
			)		
			if(meas.name %in% rownames(res.tab))
				res.tab[meas.name,c(COL_SPEAR_OCC,COL_PVAL_OCC)] <- c(corr$estimate, corr$p.value)
			else
			{	res.tab <- rbind(meas.name, c(corr$estimate, corr$p.value))
				rownames(res.tab)[nrow(res.tab)] <- meas.name
			}
			
			# update file
			write.csv(x=res.tab, file=table.file, row.names=TRUE)#, col.names=TRUE)
		}
	}
	
	tlog(4,"Computation of rank correlation measures complete")
	return(res.tab)
}



###############################################################################
# Computes all rank correlation measures for the specified static graph.
#
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
###############################################################################
compute.all.static.corrs <- function(mode, window.size=NA, overlap=NA, weights=NA)
{	graph.file <- get.path.graph.file(mode, window.size, overlap)
	tlog(3,"Computing all rank correlation measures for \"",graph.file,"\"")
	
	# read the graph file
	tlog(4,"Loading graph")
	g <- read.graph(file=graph.file, format="graphml")
	if(!is.na(weights))
	{	if(weights=="occurrences")
			E(g)$weight <- E(g)$Occurrences
		else if(weights=="duration")
			E(g)$weight <- E(g)$Duration
	}
	else
		E(g)$weight <- E(g)$Occurrences
	
	# init cache
	cache <<- list()
	
	# compute its stats
	compute.static.correlations(mode, window.size, overlap, weights)
	
	tlog(3,"Computation of all rank correlation measures complete")
}



###############################################################################
# Computes all preselected topological measures for the specified static graph.
#
# mode: either "scenes", "panel.window", or "page.window".
# window.size: value for this parameter (ignored for mode="scenes").
# overlap: value for this parameter, specified for of the above parameter value.
#          (also ignored for mode="scenes").
# weights: either "occurrences" or "duration" (ignored for mode="window.xxx").
###############################################################################
compute.all.static.statistics <- function(mode, window.size=NA, overlap=NA, weights=NA)
{	graph.file <- get.path.graph.file(mode, window.size, overlap)
	tlog(3,"Computing all topological measures for \"",graph.file,"\"")
	
	# read the graph file
	tlog(4,"Loading graph")
	g <- read.graph(file=graph.file, format="graphml")
	if(!is.na(weights))
	{	if(weights=="occurrences")
			E(g)$weight <- E(g)$Occurrences
		else if(weights=="duration")
			E(g)$weight <- E(g)$Duration
	}
	else
		E(g)$weight <- E(g)$Occurrences
	
	# init cache
	cache <<- list()
	
	# compute its stats
	compute.static.node.statistics(g, mode, window.size, overlap, weights)
	compute.static.nodecomp.statistics(g, mode, window.size, overlap, weights)
	compute.static.nodepair.statistics(g, mode, window.size, overlap, weights)
	compute.static.link.statistics(g, mode, window.size, overlap, weights)
	compute.static.graph.statistics(g, mode, window.size, overlap, weights)
	compute.static.graphcomp.statistics(g, mode, window.size, overlap, weights)
	
	tlog(3,"Computation of all topological measures complete")
}



###############################################################################
# Main function for the computation of statistics describing static graphs.
# The graphs must have been previously extracted.
#
# panel.window.sizes: values for this parameter.
# panel.overlaps: values for this parameter, specified for of the above parameter values.
# page.window.sizes: same for page-based windows instead of panel-based.
# page.overlaps: same.
###############################################################################
compute.static.statistics <- function(panel.window.sizes, panel.overlaps, page.window.sizes, page.overlaps)
{	tlog(1,"Computing statistics for static graphs")
	
	# statistics for the scene-based graph
	for(weights in c("occurrences","duration"))
		compute.all.static.statistics(mode="scenes", weights=weights)
	for(weights in c("occurrences","duration"))
		compute.all.static.corrs(mode="scenes", weights=weights)
	
	# statistics for the panel window-based static graphs
	#future_sapply(1:length(panel.window.sizes), function(i) >> cache interaction pb
	for(i in 1:length(panel.window.sizes))
	{	window.size <- panel.window.sizes[i]
		for(overlap in panel.overlaps[[i]])
		{	compute.all.static.statistics(mode="panel.window", window.size, overlap)
			compute.all.static.corrs(mode="panel.window", window.size, overlap)
		}
	}#)
	
	# statistics for the page window-based static graphs
	#future_sapply(1:length(page.window.sizes), function(i) >> cache interaction pb
	for(i in 1:length(page.window.sizes))
	{	window.size <- page.window.sizes[i]
		for(overlap in page.overlaps[[i]])
		{	compute.all.static.statistics(mode="page.window", window.size, overlap)
			compute.all.static.corrs(mode="page.window", window.size, overlap)
		}
	}#)
	
	tlog(1,"Computation of statistics for static graphs complete")	
}
