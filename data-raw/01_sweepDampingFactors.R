library(FELLA)
library(igraph)

path.felladata <- readLines("~/all/config/devel/felladata.path")
path.majors <- readLines("~/all/config/devel/metabolomicsEnrichment_majors.path")

load(paste0(path.felladata, "/keggdata.graph.RData"))
setwd(paste0(path.majors, "/dampingFactorSweeping"))

damping.sweep <- c(0.1, 0.3, 0.5, 0.7, 0.85, 0.9)
damping.sweep <- c(.55, .6, .65, .75, .8, .95)
plyr::l_ply(damping.sweep, function(d) {
  dir.create(path = as.character(d))
  
  FELLA::buildDataFromGraph(
    keggdata.graph = keggdata.graph, 
    matrices = "none", 
    normality = "pagerank", 
    niter = 11, 
    dampingFactor = d, 
    outputDir = as.character(d))
  
  gc()
})
