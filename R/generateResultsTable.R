#' Generate readable tables from the enrichment results
#' 
#' Function \code{generateResultsTable} returns a table that contains the results 
#' of a \code{\link[FELLA]{FELLA.USER}} object 
#' with a successful enrichment analysis.
#' 
#'
#' @inheritParams .methodSingle
#' @inheritParams .threshold
#' @inheritParams .plimit
#' @inheritParams .nlimit
#' @inheritParams .LabelLengthAtPlot
#' @param capPvalues Numeric value, minimum p-value admitted for the readable 
#' formatting. Smaller p-values will be displayed as \code{< capPvalues} in the 
#' table.
#' @inheritParams .object
#' @inheritParams .data
#'
#' @return A table that contains the KEGG graph nodes with their p-value
#' 
#' @import igraph
#' @export
generateResultsTable <- function(method = "diffusion", 
                                 threshold = 0.005, 
                                 plimit = 15, 
                                 nlimit = 250, 
                                 LabelLengthAtPlot = 45, 
                                 capPvalues = 1e-10, 
                                 object = NULL, 
                                 data = NULL) {
  
  if (!is.FELLA.DATA(data)) {
    stop("'data' is not a FELLA.DATA object")
  } else if (data@keggdata@status != "loaded"){
    stop("'data' points to an empty FELLA.DATA object")
  }
  
  checkArgs <- checkArguments(
    method = method, 
    threshold = threshold, 
    plimit = plimit, 
    nlimit = nlimit, 
    LabelLengthAtPlot = LabelLengthAtPlot, 
    object = object, 
    data = data)
  
  if (!checkArgs$valid)
    stop("Bad argument when calling function 'generateResultsGraph'.")
  
  if (is.na(getValid(object, method)) || !getValid(object, method)) {
    warning(paste0("Mehod ", method, " has not been executed yet. Returning NULL..."))
    return(invisible())
  } 
  
  # HYPERGEOMETRIC TEST
  if (method == "hypergeom") {
    message("Writing hypergeom results...")
    
    pvalues <- sort(getPvalues(object, "hypergeom"))
    
    if (pvalues[1] >= threshold) {
      message("No pathway is below the p-value threshold.")
    } else {
      last <- min(plimit, tail(which(pvalues < threshold), 1))
      pvalues <- pvalues[1:last]
      
      # Info to display
      paths <- names(pvalues)
      pathnames <- as.character(data@keggdata@id2name[paths])
      pathhits <- object@hypergeom@pathhits[paths]
      pathbackground <- object@hypergeom@pathbackground[paths]
    
      # Build the dataframe
      out.hypergeom <- data.frame(paths, 
                                  pathnames, 
                                  pathhits, 
                                  pathbackground, 
                                  pvalues)
      names(out.hypergeom) <- c("KEGG id", 
                                "KEGG name", 
                                "CompoundHits", 
                                "CompoundsInPathway", 
                                "p.value")
      rownames(out.hypergeom) <- NULL
      
      message("Done.")
      
      return(out.hypergeom)

    }
  }
  
  # HEAT DIFFUSION
  if (method == "diffusion") {
    message("Writing diffusion results...")
    
    pvalues <- sort(getPvalues(object, "diffusion"))
    pvalues[pvalues < capPvalues] <- capPvalues
    
    if (pvalues[1] >= threshold) {
      message("No pathway is below the p-value threshold.")
    } else {
    last <- min(nlimit, tail(which(pvalues < threshold), 1))
    pvalues <- pvalues[1:last]
    pvalues <- pvalues[order(names(pvalues))]
    
    nodeIds <- names(pvalues)
    nodeNames <- as.character(sapply(nodeIds, function(id) {
      ans <- data@keggdata@id2name[[id]][1]
      if (is.null(ans)) 
        return(NULL)
      
      if (nchar(ans) > LabelLengthAtPlot) 
        ans <- paste0(substr(ans, 1, LabelLengthAtPlot), "...")
      return(ans)
    }))
    nodeCom <- V(getGraph(data))[nodeIds]$com
    nodeTypes <- sapply(nodeCom, function(x) switch(x, 
                                                   "1" = "Pathway", 
                                                   "2" = "Module", 
                                                   "3" = "Enzyme", 
                                                   "4" = "Reaction", 
                                                   "5" = "Compound"))

    out.diffusion <- data.frame(nodeIds,
                                nodeTypes,
                                nodeNames, 
                                pvalues)[order(nodeCom), ]
    names(out.diffusion) <- c("KEGG id", 
                              "Entry type", 
                              "KEGG name", 
                              "p.value")
    rownames(out.diffusion) <- NULL
   
    message("Done.")
    
    return(out.diffusion)
    }
  }
  
  # PAGERANK
  if (method == "pagerank") {
    message("Writing pagerank results...")
    
    pvalues <- sort(getPvalues(object, "pagerank"))
    pvalues[pvalues < capPvalues] <- capPvalues
    
    if (pvalues[1] >= threshold) {
      message("No pathway is below the p-value threshold.")
    } else {
      last <- min(nlimit, tail(which(pvalues < threshold), 1))
      pvalues <- pvalues[1:last]
      pvalues <- pvalues[order(names(pvalues))]
      
      nodeIds <- names(pvalues)
      nodeNames <- as.character(sapply(nodeIds, function(id) {
        ans <- data@keggdata@id2name[[id]][1]
        if (is.null(ans)) return(NULL)
        
        if (nchar(ans) > LabelLengthAtPlot) 
          ans <- paste0(substr(ans, 1, LabelLengthAtPlot), "...")
        return(ans)
      }))
      nodeCom <- V(getGraph(data))[nodeIds]$com
      nodeTypes <- sapply(nodeCom, function(x) switch(x, 
                                                      "1" = "Pathway", 
                                                      "2" = "Module", 
                                                      "3" = "Enzyme", 
                                                      "4" = "Reaction", 
                                                      "5" = "Compound"))
      
      out.pagerank <- data.frame(nodeIds,
                                  nodeTypes,
                                  nodeNames, 
                                  pvalues)[order(nodeCom), ]
      names(out.pagerank) <- c("KEGG id", 
                                "Entry type", 
                                "KEGG name", 
                                "p.value")
      rownames(out.pagerank) <- NULL
      
      message("Done.")
      
      return(out.pagerank)
    }
  }
  
  
  
}