enrich <- function(compounds = NULL, 
                   compoundsBackground = NULL, 
                   method = "all", 
                   loadMatrix = "none", 
                   approx = "normality", 
                   niter = 1000, 
                   path = "", 
                   data = NULL, 
                   ...) {
  
  
  # Check if data is loaded
  returnList <- F
  if (class(data) != "FELLA.DATA") {
    message("No data object supplied. Loading it from the 'path' argument...")
    returnList <- T
    data <- loadKEGGdata(path = path, 
                         loadMatrix = loadMatrix)
  } 

  # Define custom metabolites
  object <- defineCompounds(compounds = compounds, 
                            compoundsBackground = compoundsBackground, 
                            data = data)
  
  
  # Run all the analyses
  if (any(method %in% c("hypergeom", "all"))) {
    object <- runHypergeom(object = object, 
                           data = data)
  }
  
  if (any(method %in% c("diffusion", "all"))) {
    object <- runDiffusion(object = object, 
                           data = data, 
                           approx = approx, 
                           niter = niter, 
                           ...)
  }

  if (any(method %in% c("pagerank", "all"))) {
    object <- runPagerank(object = object, 
                          data = data, 
                          approx = approx, 
                          niter = niter)
  }

  
  if (returnList) {
    return(list(user = object, data = data))
  }
  
  return(object)
}