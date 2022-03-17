NaNet
=======
*Extraction and analysis of character networks from bandes dessinées, comics, mangas, and such*

* Copyright 2018-2022 Vincent Labatut 

NaNet is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/NaNet
* Data: https://doi.org/... **(not available yet)**
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>

-----------------------------------------------------------------------


# Description
This set of R scripts aims at extracting and analyzing character networks extracted from graphic novels. It actually works on manually constituted CSV files, so in theory the work of fiction could be anything besides graphic novels, provided the input format is enforced.

The script does the following:
1. Extracts various networks based on some tabular data containing individual and relational information.
2. Computes a number of statistics and generates the corresponding plots.
3. Performs additional analysis of the networks.


# Data
The raw dataset was manually for bande dessinée [Thorgal](https://en.wikipedia.org/wiki/Thorgal). The files produced by the scripts (graphs, plots, tables...) can be obtained by running these scripts, but they are also directly available on [Figshare](https://doi.org/...) **(not available yet)**.


# Organization
Here are the folders composing the project:
* Folder `data`: contains the data used by the R scripts, as well as produced by them. Each subfolder corresponds to a different series, and has the same structure:
  * File `characters.csv`: list of characters, see example in folder `Test`.
  * File `interactions.csv`: list of scenes with the involved characters.
  * File `pages.csv`: list of pages with their number of panels.
  * File `volumes.csv`: list of volumes (issues) in the series.
  * Folder `networks`: all the networks extracted from the above tables.
  * Folder `stats`: CSV and plot files containing the statistics computed for the corpus and for these networks.
* Folder `log`: logs produced when running the scripts.
* Folder `res`: resources used by the `R` scripts.
* Folder `src`: contains the `R` source code.


# Installation
You first need to install `R` and the required packages:

1. Install the [`R` language](https://www.r-project.org/)
2. Download this project from GitHub and unzip.
3. Install the required packages: 
   1. Open the `R` console.
   2. Set the unzipped directory as the working directory, using `setwd("<my directory>")`.
   3. Run the install script `src/install.R` (that may take a while).

A part of the analysis requires to compile some C code. The main instructions are in `src\pli\README.txt`, then follow the instructions in the following files (look for the TODOs):
1. `src\pli\zeta.R/`: concerns the files in folder `src\pli\zeta-function`.
2. `src\pli\powerexp.R`: concerns the files in folder `src\pli\exponential-integral`.
3. `src\pli\discpowerexp.R`: concerns the file in folder `\src\pli\discpowerexp`.


# Use
In order to extract the networks from the raw data, compute the statistics, and generate the plots:

1. Open the `R` console.
2. Set the current directory as the working directory, using `setwd("<my directory>")`.
3. Run the main script `src/main.R`.

The scripts will produce a number of files in the subfolders of folder `nets`. They are grouped in subsubfolders, each one corresponding to a specific topological measure (degree, closeness, etc.). 

The script `src/Labatut2022.R` reproduces the computations described in article [L'18] (note that this will take a while).


# Dependencies
Tested with `R` version 4.0.5, with the following packages:
* [`igraph`](http://igraph.org/r/) package: version 1.2.6.
* [`CINNA`](https://cran.r-project.org/web/packages/CINNA/): version 1.1.54.
* [`blockmodeling`](https://cran.r-project.org/web/packages/blockmodeling/): version 1.0.5.
* [`foreach`](https://cran.r-project.org/web/packages/foreach/): version 1.5.0.
* [`future.apply`](https://cran.r-project.org/web/packages/future.apply/): version 1.6.0.
* [`doParallel`](https://cran.r-project.org/web/packages/doParallel/): version 1.0.16.
* [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/): version 3.3.3.
* [`ggExtra`](https://cran.r-project.org/web/packages/ggExtra/): version 0.9.
* [`sfsmisc`](https://cran.r-project.org/web/packages/sfsmisc/): version 1.1.12.
* [`vioplot`](https://cran.r-project.org/web/packages/vioplot/): version 0.3.6.
* [`viridis`](https://cran.r-project.org/web/packages/viridis/): version 0.6.0.
* [`SDMTools`](https://cran.rstudio.com/web/packages/SDMTools): version 1.1.221.
* [`ercv`](https://cran.r-project.org/web/packages/ercv/): version 1.0.1.
* [`poweRlaw`](https://cran.r-project.org/web/packages/poweRlaw/): version 0.70.6.
* [`perm`](https://cran.r-project.org/web/packages/perm/): version 1.0.0.2.
* [`minpack.lm`](https://cran.r-project.org/web/packages/minpack.lm/): version 1.2.1.
* [`stringr`](https://cran.r-project.org/web/packages/stringr/): version 1.4.0.
* [`latex2exp`](https://cran.r-project.org/web/packages/latex2exp/): version 0.4.0.
* [`polynom`](https://cran.r-project.org/web/packages/polynom/): version 1.4.0.
* [`data.table`](https://cran.r-project.org/web/packages/data.table/): version 1.13.0.
* [`cluster`](https://cran.rstudio.com/web/packages/cluster): version 2.1.0.


# To-do List
* ...


# References
* **[V'22]** Labatut, V. *Complex Network Analysis of a Graphic Novel: The Case of the Bande Dessinée Thorgal*, submitted.
