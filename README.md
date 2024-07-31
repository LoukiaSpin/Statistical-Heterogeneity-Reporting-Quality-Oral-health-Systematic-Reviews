# Reporting and interpretation quality of statistical heterogeneity in oral health systematic reviews

## Description of the repository

The repository offers the typical structure of separate folders for the data (the datasets in .RData), R (scripts to replicate the main Table and Figures) and output (file _Figures_). 
* In the __R folder__, each R script indicates which Figure it produces (one Figure only)
* The __data folder__ includes three datasets: one with data at systematic review level (_Dataset_Review level.RData_), one with data at meta-analysis level (_Dataset_Meta level.RData_), and one with data at the summary results of the selected meta-analyses (_Dataset_Pooled level.RData_).

After downloading/cloning the repo, the user can use the .Rproj file to source all code.

## Output 

Prerequisite R packages: 
[dplyr](https://CRAN.R-project.org/package=dplyr),
[ggmosaic](https://CRAN.R-project.org/package=ggmosaic),
[ggplot2](https://CRAN.R-project.org/package=ggplot2),
[ggpubr](https://cran.r-project.org/web/packages/ggpubr/),
[gtsummary](https://CRAN.R-project.org/package=gtsummary),
[plyr](https://CRAN.R-project.org/package=plyr),
[reshape2](https://CRAN.R-project.org/package=reshape2),
[scales](https://CRAN.R-project.org/package=scales)
