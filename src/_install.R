#############################################################################################
# Install all the packages necessary to these scripts.
# 
# See also src/common/stats/distr_test.R to compile the C programs required for power law fitting.
# 
# 09/2019 Vincent Labatut
#
# source("src/_install.R")
#############################################################################################




# graph computing
install.packages("igraph")
# harmonic closeness
install.packages("CINNA")
# block modeling
install.packages("blockmodeling")

# parallel computation
#install.packages("foreach")
install.packages("future.apply")
install.packages("doParallel")

# various plots
install.packages("ggplot2")
install.packages("ggExtra")
install.packages("sfsmisc")
install.packages("plotfunctions")
# violin plots
install.packages("vioplot")
# colors
install.packages("viridis")
install.packages("SDMTools")

# statistical tests
install.packages("ercv")
install.packages("poweRlaw")
install.packages("perm")	# permutation test
# nonlinear regression
install.packages("minpack.lm")

# strings
install.packages("stringr")
install.packages("latex2exp")	# insert latex code in plot texts

# polynomials
install.packages("polynom")

# various fast functions (including binary search)
#install.packages("Rfast") # not used
install.packages("data.table")

# clustering
install.packages("cluster")

