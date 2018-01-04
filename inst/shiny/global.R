# global.R
library(shiny)

# Core pacakges
library(FELLA)
library(igraph)

# Report generation
library(knitr)
library(xtable)

# Cytoscape tool
library(visNetwork)
library(DT)

# Find internal directory
pathDBs <- paste0(
  system.file(package = "FELLA"), 
  "/database/"
)
# Find all local databases
pathData <- setNames(
  list.dirs(pathDBs, full.names = TRUE, recursive = FALSE), 
  list.dirs(pathDBs, full.names = FALSE, recursive = FALSE)
)

# The database could be loaded here to save memory
# Instead, we want to let the user choose the database
# Then it has to be loaded in server.R, once for each user. 