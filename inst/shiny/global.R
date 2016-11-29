# global.R
library(shiny)

# Core pacakges
library(FELLA)
library(igraph)

# Report generation
library(knitr)
library(xtable)

# Cytoscape tool
library(rcytoscapejs)
library(DT)

# Path to FELLA database with KEGG objects
# pathData <- "~/all/devel/big/metabolomics/FELLAdataKEGGREST/"
# pathData <- "~/all/devel/big/metabolomics/FELLAdata/"
pathDBs <- paste0(
  system.file(package = "FELLA"), 
  "/database/"
)
pathData <- setNames(
  list.dirs(pathDBs, full.names = TRUE, recursive = FALSE), 
  list.dirs(pathDBs, full.names = FALSE, recursive = FALSE)
)
pathBIMS <- "BiMS/index.html"

# Shared object for all users
# FELLA.DATA <- 
