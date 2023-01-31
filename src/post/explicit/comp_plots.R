# Additional plots aiming at producing plot grids in the paper's appendix.
# 
# Vincent Labatut
# 01/2023
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/explicit/comp_plots.R")
###############################################################################
SERIES <- "Thorgal"
source("src/common/_include.R")
start.rec.log(text="CompPlots")




###############################################################################
# comparison plots
tlog(0,"Producing clean comparison plots")

# set output folder
out.folder <- file.path(COMP_FOLDER, "clean_plots")

# set windows parameters
panel.params <- list(
	window.sizes=1:30,
	overlaps=lapply(1:30, function(size) 0:(size-1))	# 465 networks
)
page.params <- list(
	window.sizes=1:20,
	overlaps=lapply(1:20, function(size) 0:(size-1))	# 210 networks
)

# set misc parameters
filt.txt <- "unfiltered"
char.dets <- c("implicit", "explicit")
modes <- c("panel.window", "page.window")

# filename parameters
fn.modes <- c()
	fn.modes["panel.window"] <- "panels"
	fn.modes["page.window"] <- "pages"
fn.ws <- c()
	fn.ws["none"] <- ""
	fn.ws["duration"] <- "Wdu"
	fn.ws["occurrences"] <- "Woc"

# plot text
txt.modes <- c()
	txt.modes["panel.window"] <- "Panels"
	txt.modes["page.window"] <- "Pages"
txt.ws <- c()
	txt.ws["none"] <- "Edges"
	txt.ws["duration"] <- "Duration"
	txt.ws["occurrences"] <- "Occurrences"




###############################################################################
# set measure parameters
meass <- c(paste0(MEAS_TRUEPOS,SFX_TOTAL), paste0(MEAS_FALSEPOS,SFX_TOTAL), paste0(MEAS_FALSENEG,SFX_TOTAL), MEAS_PRECISION, MEAS_RECALL, MEAS_FMEASURE, paste0(MEAS_LINKWEIGHT,SFX_MAX))

# file names parameters
fn.meass <- c()
	fn.meass[paste0(MEAS_TRUEPOS,SFX_TOTAL)] <- "tp"
	fn.meass[paste0(MEAS_FALSEPOS,SFX_TOTAL)] <- "fp"
	fn.meass[paste0(MEAS_FALSENEG,SFX_TOTAL)] <- "fn"
	fn.meass[MEAS_PRECISION] <- "pre"
	fn.meass[MEAS_RECALL] <- "rec"
	fn.meass[MEAS_FMEASURE] <- "fmeas"
	fn.meass[paste0(MEAS_LINKWEIGHT,SFX_MAX)] <- "maxwght"

# plot text
txt.meass <- c()
	txt.meass[paste0(MEAS_TRUEPOS,SFX_TOTAL)] <- "True Positives"
	txt.meass[paste0(MEAS_FALSEPOS,SFX_TOTAL)] <- "False Positives"
	txt.meass[paste0(MEAS_FALSENEG,SFX_TOTAL)] <- "False Negatives"
	txt.meass[MEAS_PRECISION] <- "Precision"
	txt.meass[MEAS_RECALL] <- "Recall"
	txt.meass[MEAS_FMEASURE] <- "F-measure"
	txt.meass[paste0(MEAS_LINKWEIGHT,SFX_MAX)] <- "Maximal Edge Weight"
	
# loop over character annotation types
for(char.det in char.dets)
{	tlog(2,"Processing character annotation \"",char.det,"\"")
	
	# loop over measures
	for(meas.name in meass)
	{	tlog(4,"Processing measure \"",meas.name,"\"")
		compare <- meas.name != paste0(MEAS_LINKWEIGHT,SFX_MAX)
		
		# loop over window modes
		for(mode in modes)
		{	tlog(6,"Processing mode \"",mode,"\"")
			
			if(mode=="page.window")
			{	window.sizes <- page.params$window.sizes
				overlaps <- page.params$overlaps
			}
			else
			{	window.sizes <- panel.params$window.sizes
				overlaps <- panel.params$overlaps
			}
			common.overlaps <- sort(unique(unlist(overlaps)))
			
			# loop over weighting schemes
			if(compare)
				ws <- c("none","duration","occurrences")
			else
				ws <- c("occurrences")
			for(weights in ws)	
			{	tlog(8,"Processing weighting scheme \"",weights,"\"")
				
				if(weights=="none")
				{	ww <- "none"
					if(compare)
						meas.name.alt <- paste0(meas.name, SFX_DUR)
					else
						meas.name.alt <- meas.name
				}
				else 
				{	ww <- "occurrences"
					if(compare)
					{	if(weights=="duration")
							meas.name.alt <- paste0(meas.name, SFX_WEIGHT, SFX_NORM, SFX_DUR)
						else if(weights=="occurrences")
							meas.name.alt <- paste0(meas.name, SFX_WEIGHT, SFX_NORM, SFX_OCC)
					}
					else
						meas.name.alt <- meas.name
				}
				
				# load the reference values (scene-based graph)
				if(weights=="none")
				{	seg.none.vals <- load.static.graph.stats.scenes(weights="none", measure=meas.name.alt, filtered=filt.txt, compare=compare)
					seg.vals <- list()
					seg.vals[[1]] <- seg.none.vals
					snames <- c("Scenes")
					ltys <- c(2)
				}
				else
				{	seg.occ.vals <- load.static.graph.stats.scenes(weights="occurrences", measure=meas.name.alt, filtered=filt.txt, compare=compare)
					seg.dur.vals <- load.static.graph.stats.scenes(weights="duration", measure=meas.name.alt, filtered=filt.txt, compare=compare)
					seg.vals <- list()
					seg.vals[[1]] <- seg.occ.vals
					seg.vals[[2]] <- seg.dur.vals
					snames <- c("Occurrences","Duration")
					ltys <- c(2,3)
				}
				
				# retrieve the window.size data series
				tlog(10,"Gathering and plotting data by window.size")
				data <- list()
				for(i in 1:length(window.sizes))
				{	# the series corresponds to the values of the overlap
					window.size <- window.sizes[i]
					data[[i]] <- load.static.graph.stats.by.window(mode=mode, char.det=char.det, window.size=window.size, overlaps=overlaps[[i]], measure=meas.name.alt, weights=ww, filtered=filt.txt, compare=compare)
					data[[i]][is.infinite(data[[i]])] <- NA
				}
				# generate a plot containing each window size value as a series
				if(char.det=="implicit")
					cols <- viridis(length(data))
				else
					cols <- plasma(length(data))
				tmp.folder <- file.path(out.folder, char.det, fn.modes[mode])
				dir.create(path=tmp.folder, showWarnings=FALSE, recursive=TRUE)
				plot.file <- file.path(tmp.folder, paste0(fn.meass[meas.name], if(compare) fn.ws[weights] else "", "_", fn.modes[mode], "_foverlap"))
				tlog(5,"Plotting file \"",plot.file,"\"")
				if(all(is.na(unlist(data))))
				{	msg <- paste0("WARNING: All values are NA for ", plot.file)
					tlog(6,msg)
					#warning(msg)
				}
				else
				{	for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						par(mar=c(3.5,3.2,0.7,0.7)+0.1)		# plot margins: Bottom=5 Left=4 Top=4 Right=2
						par(mgp=c(2.25,1,0))				# label space: axis names=3, tick names=1, tick marks=0
						# init plot
						if(compare)
							lim.vals <- c(unlist(data))
						else
							lim.vals <- c(unlist(data),unlist(seg.vals))
						plot(NULL, 
							xlim=c(min(common.overlaps,na.rm=TRUE),max(common.overlaps,na.rm=TRUE)),
							ylim=c(if(is.na(ALL_MEASURES[[meas.name.alt]]$bounds[1]))
												min(lim.vals,na.rm=TRUE)
											else
												ALL_MEASURES[[meas.name.alt]]$bounds[1],
									if(is.na(ALL_MEASURES[[meas.name.alt]]$bounds[2]))
												max(lim.vals,na.rm=TRUE)
											else
												ALL_MEASURES[[meas.name.alt]]$bounds[2]),
							xlab=paste0("Overlap (",txt.modes[mode],")"),
							ylab=paste0(if(weights=="none" || !compare) "" else "Weighted ", txt.meass[meas.name], " (",txt.ws[weights],")"),
							main=NA
						)
						# draw reference lines
						if(!compare)
						{	for(s in 1:length(seg.vals))
								abline(h=seg.vals[[s]], lty=ltys[s])
						}
						# draw series
						for(d in 1:length(data))
						{	lines(x=overlaps[[d]],y=data[[d]],
								col=cols[d], lwd=2
							)
						}
						# add color legend
						if(char.det=="implicit")
							lcols <- viridis(100)
						else
							lcols <- plasma(100)
						gradientLegend(
							range(window.sizes), 
							color=lcols,	#,direction=-1), 
							inside=TRUE
						)
						# add line legend (if two reference values)
						if(!compare && length(seg.vals)>1)
						{	legend(
								x="bottomright",
								lty=ltys,
								legend=snames,
								title="Scene-Based"
							)
						}
						dev.off()
					}
				}
				
				# retrieve the overlap data series
				tlog(5,"Gathering and plotting data by overlap")
				data <- list()
				axis <- list()
				for(i in 1:length(common.overlaps))
				{	# the series corresponds to the values of the window sizes
					overlap <- common.overlaps[i]
					idx <- sapply(overlaps, function(vect) overlap %in% vect)
					data[[i]] <- load.static.graph.stats.by.overlap(mode=mode, char.det=char.det, window.sizes=window.sizes[idx], overlap=overlap, measure=meas.name.alt, weights=ww, filtered=filt.txt, compare=compare)
					data[[i]][is.infinite(data[[i]])] <- NA
					axis[[i]] <- window.sizes[idx]
				}
				# generate a plot representing each overlap value as a series
				if(char.det=="implicit")
					cols <- viridis(length(data))
				else
					cols <- plasma(length(data))
				plot.file <- file.path(out.folder, char.det, fn.modes[mode], paste0(fn.meass[meas.name], if(compare) fn.ws[weights] else "", "_", fn.modes[mode], "_fsize"))
				tlog(5,"Plotting file \"",plot.file,"\"")
				if(all(is.na(unlist(data))))
				{	msg <- paste0("WARNING: All values are NA for ", plot.file)
					tlog(6,msg)
					#warning(msg)
				}
				else
				{	for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						par(mar=c(3.5,3.2,0.7,0.7)+0.1)		# plot margins: Bottom=5 Left=4 Top=4 Right=2
						par(mgp=c(2.25,1,0))				# label space: axis names=3, tick names=1, tick marks=0
						# init plot
						if(compare)
							lim.vals <- c(unlist(data))
						else
							lim.vals <- c(unlist(data),unlist(seg.vals))
						plot(NULL, 
							xlim=c(min(window.sizes,na.rm=TRUE),max(window.sizes,na.rm=TRUE)),
							ylim=c(if(is.na(ALL_MEASURES[[meas.name.alt]]$bounds[1]))
												min(lim.vals,na.rm=TRUE)
											else
												ALL_MEASURES[[meas.name.alt]]$bounds[1],
									if(is.na(ALL_MEASURES[[meas.name.alt]]$bounds[2]))
												max(lim.vals,na.rm=TRUE)
											else
												ALL_MEASURES[[meas.name.alt]]$bounds[2]),
							xlab=paste0("Window Size (",txt.modes[mode],")"),
							ylab=paste0(if(weights=="none") "" else "Weighted ", txt.meass[meas.name], " (",txt.ws[weights],")"),
							main=NA
						)
						# draw reference lines
						if(!compare)
						{	for(s in 1:length(seg.vals))
								abline(h=seg.vals[[s]], lty=ltys[s])
						}
						# draw series
						for(d in 1:length(data))
						{	lines(x=axis[[d]],y=data[[d]],
								col=cols[d], lwd=2
							)
						}
						# add color legend
						if(char.det=="implicit")
							lcols <- viridis(100)
						else
							lcols <- plasma(100)
						gradientLegend(
							range(common.overlaps), 
							color=lcols, #,direction=-1), 
							inside=TRUE
						)
						# add line legend
						if(!compare && length(seg.vals)>1)
						{	legend(
								x="bottomright",
								lty=ltys,
								legend=snames,
								title="Scene-Based"
							)
						}
						dev.off()
					}
				}
			}
		}
	}
}




###############################################################################
# TFPN plots
for(char.det in char.dets)
{	tlog(2,"Processing character annotation \"",char.det,"\"")
	
	# color palette
	if(char.det=="implicit")
		cols <- viridis(3)
	else
		cols <- plasma(3)
	
	# loop over window modes
	for(mode in modes)
	{	tlog(6,"Processing mode \"",mode,"\"")
		
		if(mode=="page.window")
		{	window.sizes <- page.params$window.sizes
			overlaps <- page.params$overlaps
		}
		else
		{	window.sizes <- panel.params$window.sizes
			overlaps <- panel.params$overlaps
		}
		#common.overlaps <- sort(unique(unlist(overlaps)))
		common.overlaps <- c(0, 5)	# fixed overlaps of interest
		
		# loop over weighting schemes
		for(weights in ws)	
		{	tlog(8,"Processing weighting scheme \"",weights,"\"")
						
			# setup measure name lists
			if(weights=="none")
			{	ww <- "none"
				ms <- rbind(
					c(paste0(MEAS_TRUEPOS, SFX_TOTAL, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_TOTAL, SFX_DUR), paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_DUR))
				)
				rownames(ms) <- c(
					paste0("tfpn", SFX_TOTAL, SFX_DUR)
				)
				
			}
			else
			{	ww <- "occurrences"
				ms <- rbind(
					c(paste0(MEAS_TRUEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR), paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR)), 
					c(paste0(MEAS_TRUEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSEPOS, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC), paste0(MEAS_FALSENEG, SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC))
				)
				rownames(ms) <- c(
					paste0("tfpn", SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_DUR),
					paste0("tfpn", SFX_TOTAL, SFX_WEIGHT, SFX_NORM, SFX_OCC)
				)
			}
			
			for(m in 1:nrow(ms))
			{	meas.name <- rownames(ms)[m]
				tlog(10,"Computing measure \"",meas.name,"\"")
				
				# generate a plot for each overlap value appearing at least twice
				for(overlap in common.overlaps)
				{	tlog(12,"Dealing with overlap=",overlap)
					
					# the series corresponds to the values of the window sizes
					idx <- sapply(overlaps, function(vect) overlap %in% vect)
					data <- sapply(1:ncol(ms), function(j) load.static.graph.stats.by.overlap(mode=mode, char.det=char.det, window.sizes=window.sizes[idx], overlap=overlap, measure=ms[m,j], weights=ww, filtered=filt.txt, compare=TRUE))
					if(length(window.sizes[idx])>1)
						data <- t(data)
					
					# produce plot files
					plot.file <- file.path(out.folder, char.det, fn.modes[mode], paste0("tfpn", fn.ws[weights], "_", fn.modes[mode], "_ol",overlap))
					tlog(14,"Plotting file \"",plot.file,"\"")
					for(fformat in PLOT_FORMAT)
					{	if(fformat==PLOT_FORMAT_PDF)
							pdf(file=paste0(plot.file,PLOT_FORMAT_PDF), bg="white")
						else if(fformat==PLOT_FORMAT_PNG)
							png(filename=paste0(plot.file,PLOT_FORMAT_PNG), width=800, height=800, units="px", pointsize=20, bg="white")
						par(mar=c(2.8,3.3,0.8,0.0))			# plot margins: Bottom=5 Left=4 Top=4 Right=2
						par(mgp=c(2.25,1,0))				# label space: axis names=3, tick names=1, tick marks=0
						x <- barplot(
							data,
							col=cols,
							ylim=c(0,max(colSums(data))),
							#xlab=paste0("Window Size (",txt.modes[mode],")"),
							ylab=paste0(if(weights=="none") "" else "Weighted ", "Frequency (",txt.ws[weights],")"),
							legend.text=c("TP","FP","FN"),
							args.legend=list(x="topleft"),
							names.arg=window.sizes[idx],
							main=NA,
							xaxt="n"
						)
						axis(1, x, window.sizes[idx], tick=FALSE, mgp=c(1,0.05,0), cex.axis=if(overlap==0) 0.6 else 0.7)
						title(xlab=paste0("Window Size (",txt.modes[mode],")"), line=1.2)
						dev.off()
					}
				}
			}
		}
	}
}




# TODO
# est-ce qu'on prend bien les versions normalisées quand il y a des poids ? (à chaque plot)
# vérifier que si le même plot en dur vs occ est identique (normal ça ?)
# rajouter dans le papier les nouvelle pages correspondant à explicit
# mettre à jour les stats de référence dans les légendes de ces pages-figures




###############################################################################
# end logging
tlog(0,"Process complete")
end.rec.log()
