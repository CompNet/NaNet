# data folder
folder <- "C:/Users/Vincent/Documents/Travail/Ecrits/Networks Analysis/Bost, Xavier/09. NANet/1. SNAM 2021/00 Other nets/Waumans2015_1373869/graphs"

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

# names of the stats
measnames <- c(
	"n",
	"m",
	"Density",
	"Avg k",
	"Avg dist",
	"Rand avg dist",
	"Norm avg dist",
	"Avg trans",
	"Rand avg trans",
	"Norm avg trans",
	"Smallworldness",
	"Max k",
	"Exp deg power law",
	"Degr ass",
	"Exp trans-deg rel"
)

# init stat table
tab <- matrix(NA, nrow=length(files), ncol=length(measnames))
rownames(tab) <- files
colnames(tab) <- measnames

# alt function to test PL
test.pl <- function(data)
{	if(0 %in% data)
		data <- data + 1
	
	# estimate parameters
	power.law <- fit_power_law(data, implementation="plfit") # "plfit" "R.mle"
	# display results
	tlog(4,"Parameters: x_min=",power.law$xmin," exp=",power.law$alpha)
	tlog(4,"p-value for power law: ",power.law$KS.p)
	
	return(power.law$alpha)
}

# compute all stats for all networks
for(file in files)
{	# read and convert graph file
	f <- file.path(folder,paste0(file,".gexf"))
	cat("Processing file",f,"\n")
	gexf.g <- read.gexf(f)
	g <- gexf.to.igraph(gexf.g)
	# remove negative links
	#g <- delete_edges(graph=g, edges=which(E(g)$weight<0))
	
	# fill stats table
	tab[file,"n"] <- gorder(g)
	tab[file,"m"] <- gsize(g)
	tab[file,"Density"] <- graph.density(g)
	tab[file,"Avg k"] <- mean(degree(g))
	tab[file,"Avg dist"] <- mean_distance(graph=g)
	tab[file,"Rand avg dist"] <- log(tab[file,"n"]) / log(tab[file,"Avg k"])
	tab[file,"Norm avg dist"] <- tab[file,"Avg dist"] / tab[file,"Rand avg dist"]
	tab[file,"Avg trans"] <- transitivity(graph=g, type="localaverage", weights=NA, isolates="zero")
	tab[file,"Rand avg trans"] <- tab[file,"Avg k"] / tab[file,"n"]
	tab[file,"Norm avg trans"] <- tab[file,"Avg trans"] / tab[file,"Rand avg trans"]
	tab[file,"Smallworldness"] <- tab[file,"Norm avg trans"] / tab[file,"Norm avg dist"]
	tab[file,"Max k"] <- max(degree(g))
	tab[file,"Exp deg power law"] <- test.cont.distr(if(any(degree(g)==0)) degree(g)+1 else degree(g))
	tab[file,"Exp deg power law"] <- test.pl(degree(g))
	tab[file,"Degr ass"] <- assortativity_degree(graph=g, directed=FALSE)
tab[file,"Exp trans-deg rel"] <- NA
	
	# export graph as graphml for possible later use
	f <- file.path(folder,paste0(file,".graphml"))
	write.graph(graph=g, file=f, format="graphml")
}

# record stats table
tab.file <- file.path(folder,"stats.csv")
write.table(tab, file=tab.file, sep=",", row.names=T, col.names=T)




# try with python code
#library("reticulate")
#env <- import("powerlaw")
#res <- powerlaw$Fit(deg)
#res$alpha
#res$D
#res$distribution_compare('power_law', 'lognormal')

#tab <- read.table("C:/Users/Vincent/Downloads/words.txt")[,1]
#test.cont.distr(tab)
