#This file is to install and load all of the required packages for the ST_project_info Shiny app.

#source("https://bioconductor.org/biocLite.R")

list_of_R_packages <- c("googlesheets", "tidyr", "zoo", "shiny", "shinydashboard", "scales", "devtools", "Cairo")
#list_of_bioconductor_packages <- "" #c("org.Hs.eg.db", "org.Mm.eg.db")
#list_of_devtools_packages <- "" #c("roxygen2")

new_R_packages <- list_of_R_packages[!(list_of_R_packages %in% installed.packages()[,"Package"])]
if(length(new_R_packages)) install.packages(new_R_packages,repos="http://cran.r-project.org")

#new_bioconductor_packages <- list_of_bioconductor_packages[!(list_of_bioconductor_packages %in% installed.packages()[,"Package"])]
#if(length(new_bioconductor_packages)) biocLite(new_bioconductor_packages)

#new_devtools_packages <- list_of_devtools_packages[!(list_of_devtools_packages %in% installed.packages()[,"Package"])]
#if(length(new_devtools_packages)) devtools::install_github("klutometis/roxygen")

#invisible(lapply(c(list_of_bioconductor_packages,list_of_R_packages, list_of_devtools_packages), library, character.only = TRUE))

invisible(lapply(list_of_R_packages, library, quietly = TRUE, character.only = TRUE))

devtools::install_github('hadley/ggplot2')
library("ggplot2")
devtools::install_github("ropensci/plotly")
library("plotly")

#devtools::install("~/Spatial_transcriptomics_projects/staligner")
#library("staligner")