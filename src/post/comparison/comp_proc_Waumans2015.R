# Computes the networks from article
#		M. C. Waumans, T. Nicodème, and H. Bersini, 
#		“Topology Analysis of Social Networks Extracted from Literature”, 
#		PLoS ONE 10(6):e0126470, 2015.
#		DOI: 10.1371/journal.pone.0126470
#
# Vincent Labatut
# 08/2021
###############################################################################
source("src/post/comparison/comp_proc.R")




###############################################################################
# data folder
folder <- file.path(CHARNETS_FOLDER, "Waumans2015") 
tlog(0,"Computing networks in folder ",folder)

# file names
files <- c(
	"ASOIAF_1",
	"ASOIAF_2",
	"ASOIAF_3",
	"ASOIAF_4",
	"ASOIAF_5",
	"Harry_Potter_1",
	"Harry_Potter_2",
	"Harry_Potter_3",
	"Harry_Potter_4",
	"Harry_Potter_5",
	"Harry_Potter_6",
	"Harry_Potter_7",
	"His_Dark_Materials_1",
	"His_Dark_Materials_2",
	"His_Dark_Materials_3",
	"TLC_1",
	"TLC_2",
	"TLC_3",
	"TLT_1",
	"TLT_2",
	"TLT_3",
	"TMI_1",
	"TMI_2",
	"TMI_3",
	"TMI_4",
	"TMI_5",
	"TMI_6",
	"TRWC_1",
	"TRWC_2",
	"TRWC_3",
	"TRWC_4",
	"TWOT_00",
	"TWOT_01",
	"TWOT_02",
	"TWOT_03",
	"TWOT_04",
	"TWOT_05",
	"TWOT_06",
	"TWOT_07",
	"TWOT_08",
	"TWOT_09",
	"TWOT_10",
	"TWOT_11",
	"TWOT_12",
	"TWOT_13",
	"TWOT_14",
	"Les_Miserables_Gutenberg_pg135"
)

###############################################################################
# init stat tables
prop.tab <- data.frame(matrix(vector(), 0, length(measnames), dimnames=list(c(), measnames)),
				stringsAsFactors=FALSE)
distr.tab <- matrix(vector(), 0, length(C_DISTR), dimnames=list(c(), C_DISTR))

# compute all stats for all networks
for(ff in 1:length(files))
{	# read and convert the graph file
	file <- files[ff]
	base.filename <- file.path(folder,file)
	f <- paste0(base.filename,".gexf")
	tlog(2, "Processing file ",f," (",ff,"/",length(files),")")
	gexf.g <- read.gexf(f)
	g <- gexf.to.igraph(gexf.g)
	g$name <- file
	# remove negative links
	#g <- delete_edges(graph=g, edges=which(E(g)$weight<0))
	#print(table(degree(g)))
	
	# clean the network
	g <- charnet.clean(g)
	
	# export graph as graphml for possible later use
	f <- paste0(base.filename,".graphml")
	write.graph(graph=g, file=f, format="graphml")
	
	# compute topological measures
	prop.row <- charnet.prop(g)
	prop.tab <- rbind(prop.tab, prop.row)
	rownames(prop.tab)[nrow(prop.tab)] <- file
	
	# process degree distribution
	deg <- degree(g, mode="all")
	distr.row <- test.disc.distr(data=deg, return_stats=TRUE, sims=100, plot.file=paste0(base.filename,"_deg_distrib"))
	distr.tab <- rbind(distr.tab, rep(NA, ncol(distr.tab)))
	distr.tab[nrow(distr.tab),names(distr.row)] <- distr.row
	rownames(distr.tab)[nrow(distr.tab)] <- file
	prop.tab[nrow(distr.tab),"Degr distrib"] <- distr.row[C_DECISION]
}

# record stats tables
tab.file <- file.path(folder,"_measures.csv")
write.table(prop.tab, file=tab.file, sep=",", row.names=T, col.names=T)
tab.file <- file.path(folder,"_degree_dist.csv")
write.table(distr.tab, file=tab.file, sep=",", row.names=T, col.names=T)
