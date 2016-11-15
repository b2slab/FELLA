library(FELLA)
library(igraph)

path.majors <- readLines("~/all/config/devel/metabolomicsEnrichment_majors.path")
path.metabolon <- "~/all/devel/projects/bioinfo/002_diffusion_metabolomics/publications/articles/2016_diffusionarticle/data/compounds.RData"

setwd(paste0(path.majors, "/dampingFactorSweeping"))

load(path.metabolon)

list.paths <- list.dirs(full.names = F)
damping.sweep <- unlist(stringr::str_match_all(list.paths, "0\\.\\d{1,2}"))

plyr::l_ply(damping.sweep, function(d) {
  ans.save <- paste0(d, "/norm_0.05.RData")
  png.save <- paste0(d, "/norm_0.05.png")
    # browser()
  fella.ans <- FELLA::enrich(
    compounds = compounds.metabolon, 
    method = "pagerank", 
    loadMatrix = NULL, 
    approx = "normality", 
    path = paste0(as.character(d), "/"))
  
  fella.graph <- FELLA::generateResultsGraph(
    method = "pagerank", 
    threshold = .05, 
    nlimit = 1000, 
    splitByConnectedComponent = F, 
    object = fella.ans$user, 
    data = fella.ans$data)
  
  save(fella.graph, fella.ans, file = ans.save)
  
  png(filename = png.save, 
      res = 90, 
      width = 40, 
      height = 40, 
      units = "cm")
  
  FELLA::plot(x = fella.ans$user, 
              nlimit = 500, 
              method = "pagerank", 
              main = paste0(
                "sh22 norm, FDR < 0.05, d = ", 
                d, 
                ", n = ", 
                ifelse(is.null(fella.graph), 
                       0, 
                       vcount(fella.graph)), 
                " (caps at 500)"), 
              # threshold = .1, 
              data = fella.ans$data)
  
  dev.off()
})

plyr::l_ply(damping.sweep, function(d) {
  # browser()
  png.save <- paste0(d, "/norm_0.05.png")
  png.plots <- paste0("plots/d=", 
                      format(as.numeric(d), digits = 2, nsmall = 2), 
                      ".png")
  
  file.copy(from = png.save, 
            to = png.plots, 
            overwrite = T)
})