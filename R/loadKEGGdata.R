#' Load KEGG data 
#' 
#' This function loads all necessary contextual data from KEGG as a 
#' \code{\link{FELLA.DATA}} object. This object is necessary to perform any 
#' kind of enrichment using \code{\link{FELLA}}.
#'
#' @param path Path for the KEGG RData files
#' @inheritParams .loadMatrix
#'
#' @return The \code{\link{FELLA.DATA}} object that contains the KEGG representation
#' 
#' @import igraph
#' @export
loadKEGGdata <- function(path = "", 
                         loadMatrix = NULL) { 
  
  message("Loading KEGG graph data...")
#   dataLoad <- new.env()
  
  # Checking the input
  ##############################################################################
  if (!is.null(loadMatrix) & length(loadMatrix) > 1)
    stop("'loadMatrix' can only be a length 1 character ('diffusion', 'pagerank', 'all') or NULL.")

  if (!is.null(loadMatrix) & !is.character(loadMatrix))
    stop("'loadMatrix' can only be a length 1 character ('diffusion', 'pagerank', 'all') or NULL.")

  if (!is.character(path) | length(path) > 1) 
    stop("'path' must be a length 1 character to an existing directory.")
    
  ##############################################################################
  assign("F.DATA", new("FELLA.DATA"))

  # Load the graph and the identifiers (required)
  if (file.exists(paste0(path, "keggdata.graph.RData"))) {
          load(paste0(path, "keggdata.graph.RData"))
          
          F.DATA@keggdata@graph <- keggdata.graph
          F.DATA@keggdata@pvalues.size <- keggdata.pvalues.size
          F.DATA@keggdata@id2name <- V(keggdata.graph)$NAME
          names(F.DATA@keggdata@id2name) <- V(keggdata.graph)$name
          
          F.DATA@keggdata@id$pathway <- which(V(keggdata.graph)$com == 1)
          names(F.DATA@keggdata@id$pathway) <- (V(keggdata.graph)$name)[F.DATA@keggdata@id$pathway]
          F.DATA@keggdata@id$module <- which(V(keggdata.graph)$com == 2)
          names(F.DATA@keggdata@id$module) <- (V(keggdata.graph)$name)[F.DATA@keggdata@id$module]
          F.DATA@keggdata@id$enzyme <- which(V(keggdata.graph)$com == 3)
          names(F.DATA@keggdata@id$enzyme) <- (V(keggdata.graph)$name)[F.DATA@keggdata@id$enzyme]
          F.DATA@keggdata@id$reaction <- which(V(keggdata.graph)$com == 4)
          names(F.DATA@keggdata@id$reaction) <- (V(keggdata.graph)$name)[F.DATA@keggdata@id$reaction]
          F.DATA@keggdata@id$compound <- which(V(keggdata.graph)$com == 5)
          names(F.DATA@keggdata@id$compound) <- (V(keggdata.graph)$name)[F.DATA@keggdata@id$compound]
  } else {
    stop(paste0("'keggdata.graph.RData' not present in:", 
                paste0(path, "keggdata.graph.RData"), 
                ". Please check that KEGG data is available."))
  }
  message("Done.")
  
  # Load matrix for hypergeometric test
  message("Loading hypergeom data...")
  message("Loading matrix...")
  if (file.exists(paste0(path, "hypergeom.matrix.RData"))) {
    load(paste0(path, "hypergeom.matrix.RData"))
    F.DATA@hypergeom@matrix <- hypergeom.matrix
  } else {
    message(paste0("'hypergeom.matrix.RData' not present in:", 
                   paste0(path, "hypergeom.matrix.RData"), 
                   ". Hypergeometric test won't execute."))
  }
  message("Done.")
  
  # Load matrix for diffusion 
  message("Loading diffusion data...")
  message("Loading matrix...")
  if ((loadMatrix == "diffusion") || (loadMatrix == "all")) {
    if (!file.exists(paste0(path, "diffusion.matrix.RData"))) {
      message(paste0("'diffusion.matrix.RData' not present in:", 
                     paste0(path, "diffusion.matrix.RData"), 
                     ". Simulated p-values will execute slower for diffusion."))  
    } else {
      load(paste0(path, "diffusion.matrix.RData"))
      F.DATA@diffusion@matrix <- diffusion.matrix
    }
    
  } else {
    message(paste0("'diffusion.matrix.RData' not loaded. ", 
                   "Simulated p-values will execute slower for diffusion."))  
  }
  message("Done.")
  
  # Load diffusion rowsums for p-value calculation 
  message("Loading rowSums...")
  if (file.exists(paste0(path, "diffusion.rowSums.RData")) ) {
    load(paste0(path, "diffusion.rowSums.RData"))
    
    F.DATA@diffusion@rowSums <- diffusion.rowSums
    F.DATA@diffusion@squaredRowSums <- diffusion.squaredRowSums
  } else {
    message(paste0("'diffusion.rowSums.RData' not present in:", 
                   paste0(path, "diffusion.rowSums.RData"), 
                   ". Appoximate normal p-values won't be available for diffusion."))  
  }
  message("Done.")

  # Load matrix for pagerank
  message("Loading pagerank data...")
  message("Loading matrix...")
  if ((loadMatrix == "pagerank") || (loadMatrix == "all")) {
    if (!file.exists(paste0(path, "pagerank.matrix.RData"))) {
      message(paste0("'pagerank.matrix.RData' not present in:", 
                     paste0(path, "pagerank.matrix.RData"), 
                     ". Simulated p-values may execute slower for pagerank."))  
    } else {
      load(paste0(path, "pagerank.matrix.RData"))
      F.DATA@pagerank@matrix <- pagerank.matrix
    }
    
  } else {
    message(paste0("'pagerank.matrix.RData' not loaded. ", 
                   "Simulated p-values may execute slower for pagerank."))  
  }
  message("Done.")

  # Load pagerank rowsums for p-value calculation
  message("Loading rowSums...")
  if (file.exists(paste0(path, "pagerank.rowSums.RData")) ) {
    load(paste0(path, "pagerank.rowSums.RData"))
    
    F.DATA@pagerank@rowSums <- pagerank.rowSums
    F.DATA@pagerank@squaredRowSums <- pagerank.squaredRowSums
  } else {
    message(paste0("'pagerank.rowSums.RData' not present in:", 
                   paste0(path, "pagerank.rowSums.RData"), 
                   ". Appoximate normal p-values won't be available for pagerank."))  
  }
  message("Done.")

  F.DATA@keggdata@status <- "loaded"

  message("Data successfully loaded.")
  return(F.DATA)
}