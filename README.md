NaNet v1.1.0
=======
*Extraction and analysis of character networks from bandes dessinées, comics, mangas, and such*

* Copyright 2018-2024 Vincent Labatut 

NaNet is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/NaNet
* Data: see below
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>

-----------------------------------------------------------------------

If you use this source code or the associated *Thorgal* dataset, please cite reference [[L'22](#references)]:

```bibtex
@Article{Labatut2022,
  author        = {Labatut, Vincent},
  title         = {Complex Network Analysis of a Graphic Novel: The Case of the Bande Dessinée {Thorgal}},
  journal       = {Advances in Complex Systems},
  year          = {2022},
  volume        = {25},
  number        = {5\&6},
  pages         = {2240003},
  doi           = {10.1142/S0219525922400033},
}
```
If you use the *Game of Thrones* dataset, please cite reference [[A'24](#references)].


## Description
This set of `R` scripts aims at extracting and analyzing character networks extracted from graphic novels. It actually works on manually constituted `CSV` files, so in theory the work of fiction could be any media, provided the input format is enforced.

These scripts do the following:
1. Extract various networks based on some tabular data containing individual and relational information.
2. Compute a number of statistics and generate the corresponding plots.
3. Perform additional analysis of the networks.


## Data
The *Thorgal* raw dataset was manually constituted based on bande dessinée [Thorgal](https://en.wikipedia.org/wiki/Thorgal). The output files (graphs, plots, tables...) can be obtained by running script `Labatut2022.R`, but they are also directly available on [Zenodo](https://doi.org/10.5281/zenodo.6395874).

![ThorgalStaticNet](/data/Thorgal/network.svg)

The *Game of Thrones* dataset was also manually constituted, based on the comics adaptation of G.R.R. Martin's [A Song of Ice and Fire](https://en.wikipedia.org/wiki/A_Song_of_Ice_and_Fire) novels. The output files can be obtained by running script `Amalvy2024.R`, or directly on [Zenodo](https://doi.org/10.5281/zenodo.13893061). Note that this data repository set also contains other data and results, corresponding to the processing described in article [[A'24](#references)].


## Organization
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

The various narrative units used in the scripts are as follows:
* Panel: the smallest unit, a single panel from the comic. It belongs to a single page, and therefore a single volume, and therefore a single narrative arc.
* Page: all the panels present on the same page. It contains panels, and belongs to a single volume, and therefore a single arc.
* Scene: a sequence of panels, which can span several pages but not volumes. It contains panels, and belongs to a single volume, and therefore a single arc. Several scenes can take place in parallel, so a panel can belong to several scenes at once.
* Volume: all the pages of a comic book issue. It contains panels, pages and scenes, and belongs to a single arc.
* Arc: narrative arc constituting the whole story. It contains panels, pages, scenes and volumes.

We also initially defined the notion of series (subseries), which could be a sequence of volumes, but did not need it in the end, and therefore the implementation is not complete for this narrative unit.

In addition, for *Game of Thrones*, we had to add another narrative unit to match the novels: chapters. A chapter is a part of a volume. It contains panels, pages and scenes (a scene cannot span several chapters, like for volumes), and belongs to a single volume and a single arc. 


## Installation
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


## Use
In order to extract the networks from the raw data, compute the statistics, and generate the plots:
1. Open the `R` console.
2. Set the current directory as the working directory, using `setwd("<my directory>")`.
3. Run the appropriate main script located in `src`.

The scripts will produce a number of files in the subfolders of folder `nets`. They are grouped in subsubfolders, each one corresponding to a specific topological measure (degree, closeness, etc.). 

Main script `src/Labatut2022.R` reproduces the computations described in article [[L'22](#references)] for the *Thorgal* data. Please, use [v1.0.2](https://github.com/CompNet/NaNet/releases/tag/v1.0.2) of the source code in the *Releases* page. Be warned that this will take a while (possibly several days). You can directly retrieve the data resulting from this process on [Zenodo](https://doi.org/10.5281/zenodo.6573491). 

Main script `Amalvy2024.R` extracts the *Game of Thrones* comics networks used in article [[A'24](#references)]. You should use [v1.1.1](https://github.com/CompNet/NaNet/releases/tag/v1.1.0) of the source code to reproduce this processing. It is much faster than for the *Thorgal* data.


## Dependencies
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
* [`igraph`](http://igraph.org/r/): version 1.2.6.
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

## To-do List
* ...


## References
* **[A'24]** A. Amalvy, M. Janickyj, S. Mannion, P. MacCarron & V. Labatut. *Interconnected Kingdoms: Comparing 'A Song of Ice and Fire' Crossmedia Adaptations Using Complex Networks*, Social Network Analysis and Mining 14:199, 2024. ⟨[hal-04722579](https://hal.archives-ouvertes.fr/hal-04722579)⟩ - DOI: [10.1007/s13278-024-01365-z](http://doi.org/10.1007/s13278-024-01365-z)
* **[L'22]** V. Labatut. *Complex Network Analysis of a Graphic Novel: The Case of the Bande Dessinée Thorgal*, Advances in Complex Systems 25(5&6):22400033, 2022. ⟨[hal-03694768](https://hal.archives-ouvertes.fr/hal-03694768)⟩ - DOI: [10.1142/S0219525922400033](http://doi.org/10.1142/S0219525922400033)
NaNet v1.1.0
=======
*Extraction and analysis of character networks from bandes dessinées, comics, mangas, and such*

* Copyright 2018-2024 Vincent Labatut 

NaNet is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/NaNet
* Data: see below
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>

-----------------------------------------------------------------------

If you use this source code or the associated *Thorgal* dataset, please cite reference [[L'22](#references)]:

```bibtex
@Article{Labatut2022,
  author        = {Labatut, Vincent},
  title         = {Complex Network Analysis of a Graphic Novel: The Case of the Bande Dessinée {Thorgal}},
  journal       = {Advances in Complex Systems},
  year          = {2022},
  volume        = {25},
  number        = {5\&6},
  pages         = {2240003},
  doi           = {10.1142/S0219525922400033},
}
```
If you use the *Game of Thrones* dataset, please cite reference [[A'24](#references)].


## Description
This set of `R` scripts aims at extracting and analyzing character networks extracted from graphic novels. It actually works on manually constituted `CSV` files, so in theory the work of fiction could be any media, provided the input format is enforced.

These scripts do the following:
1. Extract various networks based on some tabular data containing individual and relational information.
2. Compute a number of statistics and generate the corresponding plots.
3. Perform additional analysis of the networks.


## Data
The *Thorgal* raw dataset was manually constituted based on bande dessinée [Thorgal](https://en.wikipedia.org/wiki/Thorgal). The output files (graphs, plots, tables...) can be obtained by running script `Labatut2022.R`, but they are also directly available on [Zenodo](https://doi.org/10.5281/zenodo.6395874).

![ThorgalStaticNet](/data/Thorgal/network.svg)

The *Game of Thrones* dataset was also manually constituted, based on the comics adaptation of G.R.R. Martin's [A Song of Ice and Fire](https://en.wikipedia.org/wiki/A_Song_of_Ice_and_Fire) novels. The output files can be obtained by running script `Amalvy2024.R`, or directly on [Zenodo](https://doi.org/10.5281/zenodo.13893061). Note that this data repository set also contains other data and results, corresponding to the processing described in article [[A'24](#references)].


## Organization
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

The various narrative units used in the scripts are as follows:
* Panel: the smallest unit, a single panel from the comic. It belongs to a single page, and therefore a single volume, and therefore a single narrative arc.
* Page: all the panels present on the same page. It contains panels, and belongs to a single volume, and therefore a single arc.
* Scene: a sequence of panels, which can span several pages but not volumes. It contains panels, and belongs to a single volume, and therefore a single arc. Several scenes can take place in parallel, so a panel can belong to several scenes at once.
* Volume: all the pages of a comic book issue. It contains panels, pages and scenes, and belongs to a single arc.
* Arc: narrative arc constituting the whole story. It contains panels, pages, scenes and volumes.

We also initially defined the notion of series (subseries), which could be a sequence of volumes, but did not need it in the end, and therefore the implementation is not complete for this narrative unit.

In addition, for *Game of Thrones*, we had to add another narrative unit to match the novels: chapters. A chapter is a part of a volume. It contains panels, pages and scenes (a scene cannot span several chapters, like for volumes), and belongs to a single volume and a single arc. 


## Installation
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


## Use
In order to extract the networks from the raw data, compute the statistics, and generate the plots:
1. Open the `R` console.
2. Set the current directory as the working directory, using `setwd("<my directory>")`.
3. Run the appropriate main script located in `src`.

The scripts will produce a number of files in the subfolders of folder `nets`. They are grouped in subsubfolders, each one corresponding to a specific topological measure (degree, closeness, etc.). 

Main script `src/Labatut2022.R` reproduces the computations described in article [[L'22](#references)] for the *Thorgal* data. Please, use [v1.0.2](https://github.com/CompNet/NaNet/releases/tag/v1.0.2) of the source code in the *Releases* page. Be warned that this will take a while (possibly several days). You can directly retrieve the data resulting from this process on [Zenodo](https://doi.org/10.5281/zenodo.6573491). 

Main script `Amalvy2024.R` extracts the *Game of Thrones* comics networks used in article [[A'24](#references)]. You should use [v1.1.1](https://github.com/CompNet/NaNet/releases/tag/v1.1.0) of the source code to reproduce this processing. It is much faster than for the *Thorgal* data.


## Dependencies
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
* [`igraph`](http://igraph.org/r/): version 1.2.6.
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


## To-do List
* ...


## References
* **[A'24]** A. Amalvy, M. Janickyj, S. Mannion, P. MacCarron & V. Labatut. *Interconnected Kingdoms: Comparing 'A Song of Ice and Fire' Crossmedia Adaptations Using Complex Networks*, Social Network Analysis and Mining 14:199, 2024. ⟨[hal-04722579](https://hal.archives-ouvertes.fr/hal-04722579)⟩ - DOI: [10.1007/s13278-024-01365-z](http://doi.org/10.1007/s13278-024-01365-z)
* **[L'22]** V. Labatut. *Complex Network Analysis of a Graphic Novel: The Case of the Bande Dessinée Thorgal*, Advances in Complex Systems 25(5&6):22400033, 2022. ⟨[hal-03694768](https://hal.archives-ouvertes.fr/hal-03694768)⟩ - DOI: [10.1142/S0219525922400033](http://doi.org/10.1142/S0219525922400033)
