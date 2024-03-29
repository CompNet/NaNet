#############################################################################################
# Functions used to plot graphs and figures.
# 
# 09/2019 Vincent Labatut
#
# source("src/common/graphs/plot_graphs.R")
#############################################################################################
LAYOUT <- NA	# graph layout




#############################################################
# Setups the layout of the specified graph and records it at 
# the specified path. If the file already exists, then just
# loads it.
#
# g: graph concerned by the layout.
# folder: where to record the layout.
#############################################################
setup.graph.layout <- function(g, folder)
{	tmp <- LAYOUT
	reset.flag <- FALSE
	
	# try to read the layout if the file exists
	lay.file <- file.path(folder,"all_layout.txt")
	if(file.exists(lay.file))
	{	tlog(0,"Loading layout file \"",lay.file,"\"")
		tmp <- as.matrix(read.table(file=lay.file, header=TRUE, row.names=1))
		
		# clean rownames
		rn <- rownames(tmp)
		rn <- fix.encoding(strings=rn)
		rownames(tmp) <- rn
		
		# check that the layout includes all characters
		map <- match(V(g)$name,rownames(tmp))
		if(any(is.na(map)))
		{	idx <- which(is.na(map))
			tlog(2, "Could not find the position of the following characters in the existing layout file:")
			for(i in idx)
				tlog(4,V(g)$name[i])
			reset.flat <- TRUE		
			#######
			# or: manually complete the layout with the few missing nodes
			#######
			tmp2 <- cbind(rep(min(tmp[,1]),length(idx))-runif(n=length(idx),0.01,1)*0.1*abs(max(tmp[,1])-min(tmp[,1])),
						  rep(min(tmp[,2]),length(idx))-runif(n=length(idx),0.01,1)*0.1*abs(max(tmp[,2])-min(tmp[,2])))
			rownames(tmp2) <- V(g)$name[idx]
			colnames(tmp2) <- c("x", "y")
			tmp <- rbind(tmp, tmp2)  
			map <- match(V(g)$name,rownames(tmp))
			tmp <- tmp[map,]
		}
		# possibly reorder the characters in the layout, in order to match the graph
		else
			tmp <- tmp[map,]
	}
	
	# otherwise, compute the layout
	if(!file.exists(lay.file) || reset.flag)
	{	tlog(0,"Layout file \"",lay.file,"\" not found or obsolete: computing and recording it")
		
		# use a  predefined layout
#		LAYOUT <<- layout_with_dh(g)		# very long
#		LAYOUT <<- layout_with_fr(g)		# most nodes overlap
#		LAYOUT <<- layout_with_gem(g)		# very long
#		LAYOUT <<- layout_with_graphopt(g, charge = 0.04)
#		LAYOUT <<- layout_with_kk(g)		# possibly fine
#		LAYOUT <<- layout_with_lgl(g)		# massive node overlap
#		LAYOUT <<- layout_with_mds(g)		# possibly fine
		# using library qgraph
		el <- get.edgelist(g, names=FALSE)
#		if("weight" %in% edge_attr_names(g))
#			ww <- E(g)$weight
#		else
			ww <- rep(1, gsize(g))
		tmp <- qgraph.layout.fruchtermanreingold(
				edgelist=el, 
				vcount=gorder(g), 
				weight=ww, 
				area=8*(gorder(g)^2),repulse.rad=(gorder(g)^3.1)
		)
		
		colnames(tmp) <- c("x", "y")
		if("name" %in% vertex_attr_names(g))
			rownames(tmp) <- V(g)$name
		
		# old code used to manually refine the layout
#		tkplot(g, layout=LAYOUT)
#		LAYOUT <<- tk_coords(3)
		
		dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
		write.table(x=tmp, file=lay.file, fileEncoding="UTF-8", row.names=TRUE, col.names=TRUE)
	}
	
	LAYOUT <<- tmp
}




#############################################################
# Displays the specified graph in an appropraite way, taking
# into account the previously set link and node attributes.
#
# g: graph to plot.
# paths: (optional) paths to highlight while plotting. This parameter
# 		 is either a list of integer vectors (node sequences), or
# 		 an integer vector if there is only one path to plot.
# col.att: (optional) name of a vertex attribute, used to determine node color.
#          It is also possible to pass just the beginning of the name, common
#		   to several binary attributes, in which case the plot will use piecharts 
#		   to represent nodes.
# cat.att: (optional) if there is a vertex attribute, indicates whether
#		   it is categorical or not.
# size.att: (optional) name of a vertex attribute used to determine
#		    node size. It has to be numerical, cannot be categorical. 
# v.hl: vertices to highlight (these are represented as squares).
# e.hl: edges to highlight (these are represented as thick lines).
# color.isolates: force isolates to be colored (by default they are not)
# file: (optional) file name, to record the plot.
#############################################################
custom.gplot <- function(g, paths, col.att, size.att, cat.att=FALSE, v.hl, e.hl, color.isolates=FALSE, file)
{	if(any(is.na(LAYOUT)))
		setup.graph.layout(g, folder=NET_FOLDER)
	pie.values <- NA
	lgd.col <- NA
	
	# vertex shapes
	vshapes <- rep("circle",gorder(g))
	if(hasArg(v.hl))
		vshapes[v.hl] <- "csquare"
	# vertex outline color
	outline.cols <- rep("BLACK",gorder(g))
	
	# set edge colors
	nature <- edge_attr(g,ATT_EDGE_NAT)
	only.signed <- length(nature)==0
	if(!only.signed)
	{	ecols <- rep("BLACK", gsize(g))
		ecols[nature==ATT_VAL_FRIEND] <- "#1A8F39"		# green
		ecols[nature==ATT_VAL_FAMILY] <- "#9C1699"		# purple
		ecols[nature==ATT_VAL_PRO] <- "#C27604"			# orange
		ecols[nature==ATT_VAL_UNK] <- "#222222"			# dark grey
 		# set edge style
		polarity <- edge_attr(g,ATT_EDGE_POL)
		elty <- rep(1,gsize(g))							# positive=solid
		elty[!is.na(polarity) 
				& polarity==ATT_VAL_NEGATIVE] <- 3		# negative=dotted
		elty[is.na(polarity)] <- 5						# unknown=long-dashed
	}
	else
	{	signs <- edge_attr(g,ATT_EDGE_SIGN)
		ecols <- rep("#1A8F39", gsize(g))				# positive=green
		ecols[signs<0] <- "#E41A1C"						# negative=red
		elty <- rep(1,gsize(g))							# only solid line
	}
	# set edge width
	if(is.null(E(g)$weight))							# if no weight:
		E(g)$weight <- rep(1,gsize(g))					# same edge width
	ewidth <- E(g)$weight
	# set edge transparency
#	idx <- as.integer(E(g)[from(1)])	# edges attached to Trajan
#	if(length(idx)>0)
#	{	ecols[idx] <- sapply(ecols[idx],function(color) 
#		{	vals <- col2rgb(color)
#			rgb(vals[1],vals[2],vals[3],alpha=50,maxColorValue=255)
#		})
#	}
	
	# possibly change the color of the highlighted path
	if(hasArg(paths))
	{	if(!is.list(paths))
			paths <- list(paths)
		for(path in paths)
		{	v <- NA
			for(n in path)
			{	if(is.na(v))
				{	v <- n
					outline.cols[v] <- "RED"
					vshapes[v] <- "csquare"
				}
				else
				{	u <- v
					v <- n
					outline.cols[v] <- "RED"
					idx <- as.integer(E(g)[u %--% v])
					ecols[idx] <- "RED"
					ewidth[idx] <- 2*E(g)$weight[idx]
				}
			}
			outline.cols[v] <- "RED"
			vshapes[v] <- "csquare"
		}
	}
	
	# possibly highlight certain links
	if(hasArg(e.hl))
	{	if(length(e.hl)>0)
			ewidth[e.hl] <- E(g)$weight[e.hl]*3
	}
	
	# vertex color
	if(hasArg(col.att))
	{	# isolates have no color
		vcols <- rep("WHITE",gorder(g))
		if(color.isolates)
			connected <- rep(TRUE, gorder(g))
		else
			connected <- igraph::degree(g)>0
		
		if(!all(!connected))
		{	# get the attribute values
			vvals <- get.vertex.attribute(graph=g, name=col.att)
			
			# just one attribute
			if(length(vvals)>0)
			{	# categorical attribute
				if(cat.att)
				{	tmp <- factor(vvals[connected])
					vcols[connected] <- CAT_COLORS[(as.integer(tmp)-1) %% length(CAT_COLORS) + 1]
					lgd.txt <- levels(tmp)
					lgd.col <- CAT_COLORS[(1:length(lgd.txt)-1) %% length(CAT_COLORS) + 1]
				}
				# numerical attribute
				else
				{	fine <- 500 									# granularity of the color gradient
					pal <- colorRampPalette(c("YELLOW","RED"))	# extreme colors of the gradient
					finite <- !is.infinite(vvals)
					vcols[connected & finite] <- pal(fine)[as.numeric(cut(vvals[connected & finite],breaks=fine))]
					vcols[connected & !finite] <- "#575757"		# infinite values are grey
					# see https://stackoverflow.com/questions/27004167/coloring-vertexes-according-to-their-centrality
				}
			}
			# several attributes, supposedly binary ones
			else
			{	cat.att <- TRUE
				att.list <- list.vertex.attributes(g)							# list of all vertex attributes
				atts <- att.list[grepl(att.list,pattern=col.att)]				# look for the ones starting appropriately
				m <- sapply(atts, function(att) vertex_attr(g, att))			# get attribute values as a matrix
				are.nas <- apply(m,1,function(r) all(is.na(r)))					# detect individuals with only NAs
				are.pie <- apply(m,1,function(r) length(r[!is.na(r)])>1)		# detect individuals with several non-NA values
				uvals <- sort(unique(c(m)))										# get unique attribute values
				pie.matrix <- NA
				for(uval in uvals)												# build a column for each of them
				{	vals <- as.integer(apply(m, 1, function(v) uval %in% v[!is.na(v)]))
					if(all(is.na(pie.matrix)))
						pie.matrix <- as.matrix(vals, ncol=1)
					else
						pie.matrix <- cbind(pie.matrix, vals)
					colnames(pie.matrix)[ncol(pie.matrix)] <- uval
				}
				lgd.txt <- colnames(pie.matrix)
				if(length(lgd.txt)<=length(CAT_COLORS))
					lgd.col <- CAT_COLORS[(1:length(lgd.txt)-1) %% length(CAT_COLORS) + 1]
				else
					lgd.col <- CAT_COLORS_18[(1:length(lgd.txt)-1) %% length(CAT_COLORS_18) + 1]
				pie.values <- unlist(apply(pie.matrix, 1, function(v) list(v)), recursive=FALSE)
				pie.values[!are.pie | !connected] <- NA
				vshapes[are.pie & connected] <- rep("pie",length(which(are.pie)))
				vcols[are.pie & connected] <- NA
				vcols[!are.nas & !are.pie & connected] <- apply(pie.matrix[!are.nas & !are.pie & connected,,drop=FALSE], 1, 
						function(v) lgd.col[which(v>0)])
			}
		}
	}
	else
		vcols <- rep("GREY",gorder(g))
	
	# vertex size
	if(hasArg(size.att))
	{	# get the attribute values
		vvals <- get.vertex.attribute(graph=g, name=size.att)
		
		# init limit sizes
		vsizes <- rep(NA, gorder(g))
		min.size <- if(min(vvals,na.rm=TRUE)==0) 0 else 2
		max.size <- 20
		cut.nbr <- 4
		
		# define cuts
		must.round <- all(vvals%%1==0)	# check if values are integers
		step <- (max(vvals,na.rm=TRUE)-min(vvals,na.rm=TRUE))/cut.nbr
		if(must.round)
			step <- ceiling(step)
		cuts <- seq(from=step, to=cut.nbr*step, by=step)
		
		# NA, NaN, Inf are set to a zero size
		nosize <- is.infinite(vvals) | is.nan(vvals) | is.na(vvals)
		vsizes[nosize] <- 0
		
		# TODO how to deal with isolates?
		
		# regular values
		tmp <- vvals[!nosize]
		vsizes[!nosize] <- (tmp-min(tmp))/(max(tmp)-min(tmp))*(max.size-min.size)+min.size
		cuts.scale <- (cuts-min(tmp))/(max(tmp)-min(tmp))*(max.size-min.size)+min.size
	}
	else
		vsizes <- 5
	
	# main plot
	for(fformat in PLOT_FORMAT)
	{	if(hasArg(file))
		{	if(fformat==PLOT_FORMAT_PDF)
				pdf(paste0(file,fformat), width=15, height=15)
			else if(fformat==PLOT_FORMAT_PNG)
				png(paste0(file,fformat), width=1024, height=1024)
		}
		plot(g,										# graph to plot
			#axes=TRUE,								# whether to draw axes or not
			layout=LAYOUT,							# layout
			vertex.size=vsizes,						# node size
			vertex.color=vcols,						# node color
			vertex.pie=pie.values,					# node pie proportions
			vertex.pie.color=list(lgd.col),			# node pie colors
			vertex.shape=vshapes,					# node shape
			vertex.frame.color=outline.cols,		# node border color
			#vertex.label=V(g)$label0,				# node short labels
			vertex.label=V(g)$label,				# node long labels
			vertex.label.cex=1.2,					# label size
			vertex.label.family="sans",				# font type
			vertex.label.font=2,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
			vertex.label.label.dist=0,				# label distance to node center (0=center)
			vertex.label.color="BLACK",				# label color
			edge.color=ecols,						# link color
			edge.lty=elty,							# link type
			edge.width=ewidth						# link thickness
		)
		if(!only.signed)
		{	legend(
				title="Nature de la relation",					# title of the legend box
				x="topright",									# position
				legend=c(ATT_VAL_FRIEND,ATT_VAL_FAMILY,			# text of the legend
						ATT_VAL_PRO,ATT_VAL_UNK),
				col=c("#1A8F39","#9C1699","#C27604","#222222"),	# color of the lines
				lty=1,											# type of lines
				lwd=4,											# line thickness
				bty="n",										# no box around the legend
				cex=0.8
			)
 			legend(
				title="Polarite de la relation",				# title of the legend box
				x="bottomright",								# position
				legend=c(ATT_VAL_POSITIVE,ATT_VAL_NEGATIVE,		# text of the legend
						ATT_VAL_UNK),
				col="BLACK",									# color of the lines
				lty=c(1,3,5),									# type of lines
				lwd=2,											# line thickness
				bty="n",										# no box around the legend
				cex=0.8,										# size of the text in the legend
				seg.len=3										# length of the line in the legend
			)
		}
		else
		{	legend(
				title="Polarite de la relation",				# title of the legend box
				x="bottomright",								# position
				legend=c(ATT_VAL_POSITIVE,ATT_VAL_NEGATIVE),	# text of the legend
				col=c("#1A8F39","#E41A1C"),						# color of the lines
				lty=c(1,1),										# type of lines
				lwd=4,											# line thickness
				bty="n",										# no box around the legend
				cex=0.8,										# size of the text in the legend
				seg.len=3										# length of the line in the legend
			)
		}
		if(hasArg(col.att))
		{	if(!all(!connected))
			{	# categorical attributes
				if(cat.att)
				{	legend(
						title=LONG_NAME[col.att],				# title of the legend box
						x="bottomleft",							# position
						legend=lgd.txt,							# text of the legend
						fill=lgd.col,							# color of the nodes
						bty="n",								# no box around the legend
						cex=0.8									# size of the text in the legend
					)
				}
				# numerical attributes
				else
				{	width <- 0.05
					height <- 0.3
					x1 <- -1
					x2 <- x1 + width
					y2 <- -1
					y1 <- y2 + height
					leg.loc <- cbind(x=c(x1, x2, x2, x1), y=c(y1, y1, y2, y2))
					legend.gradient(
						pnts=leg.loc,
						cols=pal(25),
						#limits=format(range(vvals[connected],na.rm=TRUE), digits=2, nsmall=2),	# pb: uses scientific notation when numbers too small
						limits=sprintf("%.2f", range(vvals[connected & finite],na.rm=TRUE)),
						title=LONG_NAME[col.att], 
						cex=0.8
					)
				}
			}
		}
		# legend for vertex sizes, if required: 
		# https://stackoverflow.com/questions/38451431/add-legend-in-igraph-to-annotate-difference-vertices-size
		if(hasArg(size.att))
		{	legend.bubble(
				x="topleft",					# position of the legend
				title=LONG_NAME[size.att],		# title of the legend box
				z=max(cuts),					# largest size
				maxradius=max(cuts.scale/200),	# scaled radius of the largest bubble
				n=cut.nbr,						# number of bubbles
				round=if(must.round) 0 else 2,	# number of decimal places
				bty="n",						# box (o=default, n=none)
				mab=1.2,						# margin between largest bubble and box
				bg=NULL, 						# background color of the box
				inset=0, 						# inset distance from margin
				pch=21, 						# symbol used to plot
				pt.bg=NULL, 					# symbol background color
				txt.cex=0.5, 					# text size
				txt.col=NULL, 					# text color
				font = NULL						# text font
			)
		}
		if(hasArg(file))
			dev.off()
	}
}




#############################################################################################
# Plots a legend representing the meaning of vertex sizes.
# 
# Function taken from
# 	https://rdrr.io/github/AtlanticR/bio.utilities/src/R/legend.bubble.r
#############################################################################################
legend.bubble  <- function (x, y = NULL, title, z, maxradius = 1, n = 3, round = 0, bty = "o", 
		mab = 1.2, bg = NULL, inset = 0, pch = 21, pt.bg = NULL, 
		txt.cex = 1, txt.col = NULL, font = NULL, ...) 
{
	if (length(z) == 1) 
		legend <- round((seq(0, sqrt(z), length.out = n + 1)^2)[-1], 
				round)
	else legend <- round(sort(z), round)
	radius <- maxradius * sqrt(legend)/sqrt(max(legend))
	cex <- 2 * radius/par("cxy")[2]/0.375
	box <- legend.box(x, y, maxradius, mab, inset)
	if (bty == "o") 
		rect(box[1], box[2], box[3], box[4], col = bg)
	x <- (box[1] + box[3])/2
	if(hasArg(title))
		text(x=x, y=box[2]+0.03, labels=title, col=txt.col, font=font)
	y <- box[2] - mab * maxradius + maxradius
	for (i in length(radius):1) {
		ri <- radius[i]
		cex <- 2 * ri/par("cxy")[2]/0.375
		points(x, y - ri, cex = cex, pch = pch, bg = pt.bg, ...)
		text(x, y - ri * 2, legend[i], adj = c(0.5, -0.5), cex = txt.cex, 
				col = txt.col, font = font)
	}
}




#############################################################################################
# Function used by legend.bubble.
# 
# Function taken from
# 	https://rdrr.io/github/AtlanticR/bio.utilities/src/R/legend.box.r
#############################################################################################
legend.box <- function (x, y = NULL, maxradius, mab = 1.2, inset = 0, double = F) 
{
	auto <- if (is.character(x)) 
				match.arg(x, c("bottomright", "bottom", "bottomleft", 
								"left", "topleft", "top", "topright", "right", "center"))
			else NA
	asp <- get.asp()
	h <- mab * 2 * maxradius
	w <- h * asp
	if (double) 
		h <- h * 2
	usr <- par("usr")
	inset <- rep(inset, length.out = 2)
	if (!is.na(auto)) {
		insetx <- inset[1L] * (usr[2L] - usr[1L])
		left <- switch(auto, bottomright = , topright = , right = usr[2L] - 
						w - insetx, bottomleft = , left = , topleft = usr[1L] + 
						insetx, bottom = , top = , center = (usr[1L] + usr[2L] - 
							w)/2)
		insety <- inset[2L] * (usr[4L] - usr[3L])
		top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
						h + insety, topleft = , top = , topright = usr[4L] - 
						insety, left = , right = , center = (usr[3L] + usr[4L] + 
							h)/2)
	}
	else {
		left <- x - 1.2 * asp * maxradius
		top <- y + 1.2 * maxradius
	}
	return(c(left, top, left + w, top - h))
}




#############################################################################################
# Returns current asp option. Used by function legend.box.
# 
# Function taken from
# 	https://rdrr.io/github/AtlanticR/bio.utilities/src/R/get.asp.r
#############################################################################################
get.asp <- function() 
{	pin <- par("pin")
	usr <- par("usr")
	asp <- (pin[2]/(usr[4] - usr[3]))/(pin[1]/(usr[2] - usr[1]))
	return(asp)
}




#############################################################################################
# Takes a vector of values between 0 and 1 and performs a non-linear transformation using
# Lamé's method (also resulting in values between 0 and 1). The exponent parameter controls 
# the slope of the curve. 
# 	- <1: less contrast between small values, more contrast between large ones.
# 	-  1: linear (no change).
#	- >1: more contrast between small values, less contrast between large ones.
#	-  2: circle-like.
#############################################################################################
lame.normalize <- function(vals, exp=2)
{	(1-(1-vals)^exp)^(1/exp)
}




#############################################################################################
# Adds the triangle vertex shape to igraph plotting abilities.
# Taken from:
# 	https://rdrr.io/github/b2slab/FELLA/man/mytriangle.html
#############################################################################################
#' Add triangular shape to igraph plot
#' 
#' This function enables the usage of triangles as shape in 
#' the function \code{\link[igraph]{plot.igraph}}. 
#' 
#' @param coords,v,params clipping arguments, see 
#' \code{\link[igraph]{shapes}}
#' 
#' @return Plot symbols
#' 
#' @examples 
#' ## This function is internal
#' library(igraph)
#' 
#' add.vertex.shape(
#' "triangle", clip = shapes("circle")$clip,
#' plot = FELLA:::mytriangle)
#' 
#' g <- barabasi.game(10)
#' plot(
#' g, vertex.shape = "triangle", 
#' vertex.color = rainbow(vcount(g)),
#' vertex.size = seq(10, 20, length = vcount(g)))
#' 
#' @importFrom graphics symbols
#' 
#' @keywords internal
FELLAmytriangle <- function(coords, v=NULL, params) {
	vertex.color <- params("vertex", "color")
	if (length(vertex.color) != 1 && !is.null(v)) {
		vertex.color <- vertex.color[v]
	}
	vertex.size <- 1/200 * params("vertex", "size")
	if (length(vertex.size) != 1 && !is.null(v)) {
		vertex.size <- vertex.size[v]
	}
	
	graphics::symbols(
			x = coords[, 1], y = coords[, 2], bg = vertex.color,
			stars = cbind(vertex.size, vertex.size, vertex.size),
			add = TRUE, inches = FALSE)
}
add.vertex.shape("triangle", clip=shapes("circle")$clip, plot=FELLAmytriangle)
