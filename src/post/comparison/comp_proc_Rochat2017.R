# Computes the networks from article
#		Y. Rochat and M. Triclot, 
#		“Les réseaux de personnages de science-fiction : échantillons de lectures intermédiaires,” 
#		ReS Futurae 10, 2017.
#		DOI: 10.4000/resf.1183
#
# Vincent Labatut
# 11/2021
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/comparison/comp_proc_Rochat2017.R")
###############################################################################
source("src/post/comparison/comp_proc.R")




###############################################################################
# data folder
folder <- file.path(CHARNETS_FOLDER, "Rochat2017") 
tlog(0,"Computing networks in folder ",folder)

# file names
files <- c(
	"1818.Frankenstein-adj",
	"1875.IleMysterieuse-adj",
	"1878.500millions_Begum-adj",
	"1886.Jekyll_Hyde-adj",
	"1896.Moreau-adj",
	"1917.Mars-adj",
	"1943.Ravage-adj",
	"1946.Espadon-adj",
	"1949.1984-adj",
	"1950.Asimov.Rob1-adj",
	"1950.Bradbury.1-adj",
	"1950.Bradbury.2-adj",
	"1950.Bradbury.3-adj",
	"1950.Bradbury.4-adj",
	"1950.Bradbury.123-adj",
	"1951.Fondation1-adj",
	"1952.Fondation2-adj",
	"1953.Fahrenheit451-adj",
	"1953.fondation3-adj",
	"1954.Godzilla-adj",
	"1959.Letempsdesarticule-sans-gum-adj",
	"1960.Les_croisés_du_cosmos-adj",
	"1962.Lemaitreduhautchateau-adj",
	"1964.Asimov.Rob2-adj",
	"1964.Cequedisentlesmorts-adj",
	"1965.Bloodmoney-adj",
	"1965.Ledieuvenuducentaure-adj",
	"1966.Les androïdes rêvent-ils de moutons électriques-adj",
	"1966.Who.29.The_Tenth_Planet_1-adj",
	"1966.Who.30.The_power_of_the_daleks-adj",
	"1969.Ubik-adj",
	"1971.OrangeMeca-adj",
	"1972.Stalker-adj",
	"1974.Who.74.spidermn-adj",
	"1974.Who.75.robotsc-adj",
	"1975.Who.78.Genesis of the Daleks-adj",
	"1977.Ender-adj",
	"1977.StarWars1.noJedi-adj",
	"1977.StarWars1-adj",
	"1980.CompagnieDesGlaces-adj",
	"1982.Akira1-adj",
	"1982.Akira2-adj",
	"1982.Akira3-adj",
	"1982.blade-runner-adj",
	"1984.Dune-adj",
	"1984.Neuromancien-adj",
	"1984.Who.135.Androzanisc-adj",
	"1984.Who.136.twinsc-adj",
	"1990.Gunnm-adj",
	"1992-2003.Meta-Baron-adj_corrected",
	"1994.Betelgeuse-adj",
	"1994.la_cite_des_permutants-adj",
	"1995.GhostInTheShell-adj",
	"1997.Bienvenue_a_Gattaca-adj",
	"1997.FF7-adj",
	"1998.Godzilla-adj",
	"1999.HommeBicentenaire.SansAndrew-adj",
	"1999.The_Matrix-adj",
	"1999.ZoneDehors-adj",
	"2000.Ombre_de_l_hegemon-adj",
	"2001.AI-adj",
	"2002.Equilibrium-adj",
	"2002.MinorityReport-adj",
	"2004.Steamboy-adj",
	"2005.Metro2033-adj",
	"2005.Who.166.badwolfsc-adj",
	"2005.Who.167.xmasinvasionsc-adj",
	"2006.FilsDeLhomme-adj",
	"2007.MassEffect-adj",
	"2008.3Body-adj",
	"2009.FullMetalAlchemistBrotherhood-adj",
	"2009.Moon-adj",
	"2010.Inception-adj",
	"2011.Steinsgate-adj",
	"2011.Time_Out-adj",
	"2012.PsychoPass-adj",
	"2012.Shinsekai_Yori-adj",
	"2013.Albator-adj",
	"2013.cloud_atlas-adj",
	"2013.Snowpiercer-adj",
	"2013.Who.240.Day of the Doctor-adj",
	"2014.Divergente-adj",
	"2014.the100-adj",
	"2015.Seul_sur_Mars-adj"
)

###############################################################################
# init stat tables
tabs <- charnet.init.tables(files)

# compute all stats for all networks
for(ff in 1:length(files))
{	# read and convert the graph file
	file <- files[ff]
	base.filename <- file.path(folder,file)
	f <- paste0(base.filename,".csv")
	tlog(2, "Processing file ",f," (",ff,"/",length(files),")")
	tt <- read.csv(file=f, header=TRUE, row.names=1)
	g <- make_empty_graph(n=nrow(tt), directed=FALSE)
	g$name <- file
	V(g)$name <- rownames(tt)
	for(c in 1:ncol(tt))
	{	idx <- which(tt[,c]==1)
		if(length(idx)>1)
		{	el <- t(combn(idx, 2))
			for(r in 1:nrow(el))
			{	if(are.connected(g, el[r,1], el[r,2]))
					E(g)[el[r,1] %--% el[r,2]]$weight <- E(g)[el[r,1] %--% el[r,2]]$weight + 1
				else
				{	g <- add_edges(graph=g, edges=el[r,])
					E(g)[gsize(g)]$weight <- 1
				}
			}
		}
	}
	
	# clean the network
	g <- charnet.clean(g)
	
	# export graph as graphml for possible later use
	f <- paste0(base.filename,".graphml")
	write.graph(graph=g, file=f, format="graphml")
	
	# compute topological measures
	tabs <- charnet.prop(g, tabs)
}
