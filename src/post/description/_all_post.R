# Launches all the post analysis scripts.
# 
# Vincent Labatut
# 03/2022
#
# setwd("~/eclipse/workspaces/Networks/NaNet")
# setwd("C:/Users/Vincent/Eclipse/workspaces/Networks/NaNet")
# source("src/post/description/_all_post.R")
###############################################################################




source("src/post/description/avgdist_evol.R")
source("src/post/description/centr_clusters.R")
source("src/post/description/centr_vs_centr.R")
source("src/post/description/centr_vs_occ.R")
source("src/post/description/char_distr.R")
source("src/post/description/char_sim.R")
#source("src/post/description/char_tj_comp.R")
source("src/post/description/comp_vs_edges.R")
source("src/post/description/comp_vs_vertices.R")
source("src/post/description/deg_plots.R")
#source("src/post/description/diameters.R")
#source("src/post/description/nei_vs_deg.R")
#source("src/post/description/partial_extr.R")
#source("src/post/description/pref_attach.R")
#source("src/post/description/robustness.R")
#source("src/post/description/sex_stats.R")
#source("src/post/description/trans_vs_deg.R")
#source("src/post/description/weight_plots.R")
