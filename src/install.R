#############################################################################################
# Install all the packages necessary to these scripts.
# 
# See also src/common/distr_test.R to compile the C programs required for power law fitting.
# 
# 09/2019 Vincent Labatut
# source("src/install.R")
#############################################################################################




# graph computing
install.packages("igraph")
# harmonic closeness
install.packages("CINNA")

# parallel computation
#install.packages("foreach")
install.packages("future.apply")
install.packages("doParallel")

# various plots
install.packages("ggplot2")
install.packages("ggExtra")
# violin plots
install.packages("vioplot")
# palette
install.packages("viridis")

# statistical tests
install.packages("ercv")
install.packages("poweRlaw")

# strings
install.packages("stringr")

# various fast functions (including binary search)
#install.packages("Rfast") # not used
install.packages("data.table")
