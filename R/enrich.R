#' Wrapper function for enrichment analysis
#' 
#' The function \code{\link{enrich}} is a big wrapper to perform the enrichment 
#' analysis. It loads affected compounds, KEGG data and return a list containing 
#' the \code{\link{FELLA.DATA}} context data and the \code{\link{FELLA.USER}} 
#' object with the enrichment results available.
#'
#' @inheritParams .compounds
#' @inheritParams .compoundsBackground
#' @inheritParams .method
#' @inheritParams .loadMatrix
#' @inheritParams .approx
#' @inheritParams .t.df
#' @inheritParams .niter
#' @param path Character, path to load the \code{\link{FELLA.DATA}} object if 
#' it is not already passed through the argument \code{data}
#' @inheritParams .data
#'
#' @return The \code{\link{FELLA.USER}} and the \code{\link{FELLA.DATA}} objects 
#' in a list.
#' 
#' @export
enrich <- function(compounds = NULL, 
                   compoundsBackground = NULL, 
                   method = "all", 
                   loadMatrix = "none", 
                   approx = "normality", 
                   t.df = 10, 
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
                           t.df = t.df, 
                           niter = niter, 
                           ...)
  }

  if (any(method %in% c("pagerank", "all"))) {
    object <- runPagerank(object = object, 
                          data = data, 
                          approx = approx, 
                          t.df = t.df, 
                          niter = niter, 
                          ...)
  }

  
  if (returnList) {
    return(list(user = object, data = data))
  }
  
  return(object)
}