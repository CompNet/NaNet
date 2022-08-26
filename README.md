NaNet
=======
*Extraction and analysis of character networks from bandes dessinées, comics, mangas, and such*

* Copyright 2018-2022 Vincent Labatut 

NaNet is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/NaNet
* Data: https://doi.org/10.5281/zenodo.6395874
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>

-----------------------------------------------------------------------

If you use this source code or the associated dataset, please cite reference [L'22].

# Description
This set of R scripts aims at extracting and analyzing character networks extracted from graphic novels. It actually works on manually constituted CSV files, so in theory the work of fiction could be anything besides graphic novels, provided the input format is enforced.

The script does the following:
1. Extracts various networks based on some tabular data containing individual and relational information.
2. Computes a number of statistics and generates the corresponding plots.
3. Performs additional analysis of the networks.


# Data
The raw dataset was manually constituted based on bande dessinée [Thorgal](https://en.wikipedia.org/wiki/Thorgal). The output files (graphs, plots, tables...) can be obtained by running the scripts, but they are also directly available on [Zenodo](https://doi.org/10.5281/zenodo.6395874).

![ThorgalStaticNet](/data/Thorgal/network.svg)


# Organization
Here are the folders composing the project:
* Folder `data`: contains the data used by the R scripts, as well as produced by them. Each subfolder corresponds to a different series, and has the same structure:
  * File `characters.csv`: list of characters, see example in folder `Test`.
  * File `interactions.csv`: list of scenes with the involved characters.
  * File `pages.csv`: list of pages with their number of panels.
  * File `volumes.csv`: list of volumes (issues) in the series.
  * Folder `networks`: all the networks extracted from the above tables, as Graphml files and plots.
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
   3. Run the install script `src/_install.R` (that may take a while).

A part of the analysis requires to compile some `C` code. The main instructions are in `src/common/stats/pli/README.txt`, then follow the instructions in the following files (look for the TODOs):
1. `src/common/stats/pli/zeta.R/`: concerns the files in folder `src/common/stats/pli/zeta-function`.
2. `src/common/stats/pli/powerexp.R`: concerns the files in folder `src/common/stats/pli/exponential-integral`.
3. `src/common/stats/pli/discpowerexp.R`: concerns the file in folder `\src/common/stats/pli/discpowerexp`.


# Use
In order to extract the networks from the raw data, compute the statistics, and generate the plots:

1. Open the `R` console.
2. Set the current directory as the working directory, using `setwd("<my directory>")`.
3. Run the main script `src/dev_main.R`.

The scripts will produce a number of files in the subfolders of folder `nets`. They are grouped in subsubfolders, each one corresponding to a specific topological measure (degree, closeness, etc.). 

The script `src/Labatut2022.R` reproduces the computations described in article [[L'22](#references)]. Please, use [v1.0.2](https://github.com/CompNet/NaNet/releases/tag/v1.0.2) of the source code in the *Releases* page. Be warned that this will take a while (possibly several days). You can directly retrieve the data resulting from this process on [Zenodo](https://doi.org/10.5281/zenodo.6573491). 


# Dependencies
Tested with `R` version 4.0.5, with the following packages:
* [`blockmodeling`](https://cran.r-project.org/web/packages/blockmodeling/): version 1.0.5.
* [`CINNA`](https://cran.r-project.org/web/packages/CINNA/): version 1.1.54.
* [`cluster`](https://cran.rstudio.com/web/packages/cluster): version 2.1.0.
* [`data.table`](https://cran.r-project.org/web/packages/data.table/): version 1.13.0.
* [`doParallel`](https://cran.r-project.org/web/packages/doParallel/): version 1.0.16.
* [`ercv`](https://cran.r-project.org/web/packages/ercv/): version 1.0.1.
* [`foreach`](https://cran.r-project.org/web/packages/foreach/): version 1.5.0.
* [`future.apply`](https://cran.r-project.org/web/packages/future.apply/): version 1.6.0.
* [`ggExtra`](https://cran.r-project.org/web/packages/ggExtra/): version 0.9.
* [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/): version 3.3.3.
* [`igraph`](http://igraph.org/r/) package: version 1.2.6.
* [`latex2exp`](https://cran.r-project.org/web/packages/latex2exp/): version 0.4.0.
* [`minpack.lm`](https://cran.r-project.org/web/packages/minpack.lm/): version 1.2.1.
* [`perm`](https://cran.r-project.org/web/packages/perm/): version 1.0.0.2.
* [`plotfunctions`](https://cran.r-project.org/web/packages/plotfunctions): version 1.4.
* [`polynom`](https://cran.r-project.org/web/packages/polynom/): version 1.4.0.
* [`poweRlaw`](https://cran.r-project.org/web/packages/poweRlaw/): version 0.70.6.
* [`SDMTools`](https://cran.rstudio.com/web/packages/SDMTools): version 1.1.221.
* [`sfsmisc`](https://cran.r-project.org/web/packages/sfsmisc/): version 1.1.12.
* [`stringr`](https://cran.r-project.org/web/packages/stringr/): version 1.4.0.
* [`vioplot`](https://cran.r-project.org/web/packages/vioplot/): version 0.3.6.
* [`viridis`](https://cran.r-project.org/web/packages/viridis/): version 0.6.0.


# To-do List
* ...


# References
* **[L'22]** Labatut, V. *Complex Network Analysis of a Graphic Novel: The Case of the Bande Dessinée Thorgal*, Advances in Complex Systems, p.22400033, 2022.  [⟨hal-03694768⟩](https://hal.archives-ouvertes.fr/hal-03694768) - DOI: [10.1142/S0219525922400033](http://doi.org/10.1142/S0219525922400033)
