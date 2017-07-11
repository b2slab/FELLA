#' Generate readable tables from the enrichment results
#' 
#' Function \code{generateResultsTable} returns a table 
#' that contains the results 
#' of a \code{\link[FELLA]{FELLA.USER}} object 
#' with a successful enrichment analysis.
#' 
#'
#' @inheritParams .params
#' @param ... ignored arguments
#'
#' @return A table that contains the KEGG graph nodes with their p-score
#' 
#' @examples 
#' data(FELLA.sample)
#' data(input.sample)
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' tab <- generateResultsTable(
#' threshold = 0.1, 
#' object = obj, 
#' data = FELLA.sample)
#' head(tab)
#' 
#' @import igraph
#' @export
generateResultsTable <- function(
    method = "diffusion", 
    threshold = 0.005, 
    plimit = 15, 
    nlimit = 250, 
    LabelLengthAtPlot = 45, 
    capPscores = 1e-10, 
    object = NULL, 
    data = NULL, 
    ...) {
    
    if (!is.FELLA.DATA(data)) {
        stop("'data' is not a FELLA.DATA object")
    } else if (getStatus(data) != "loaded"){
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
        warning(
            paste0("Mehod ", method, " has not been executed yet. "),  
            "Returning NULL...")
        return(invisible())
    } 
    
    # HYPERGEOMETRIC TEST
    if (method == "hypergeom") {
        message("Writing hypergeom results...")
        
        pscores <- sort(getPscores(object, "hypergeom"))
        
        if (pscores[1] >= threshold) {
            message("No pathway is below the p-value threshold.")
        } else {
            last <- min(plimit, tail(which(pscores < threshold), 1))
            pscores <- pscores[1:last]
            
            # Info to display
            paths <- names(pscores)
            pathnames <- as.character(data@keggdata@id2name[paths])
            pathhits <- object@hypergeom@pathhits[paths]
            pathbackground <- object@hypergeom@pathbackground[paths]
            
            # Build the dataframe
            out.hypergeom <- data.frame(
                "KEGG id" = paths, 
                "KEGG name" = pathnames, 
                "CompoundHits" = pathhits, 
                "CompoundsInPathway" = pathbackground, 
                "p.value" = pscores, 
                stringsAsFactors = FALSE)
            rownames(out.hypergeom) <- NULL
            
            message("Done.")
            
            return(out.hypergeom)
            
        }
    }
    
    # HEAT DIFFUSION
    if (method == "diffusion") {
        message("Writing diffusion results...")
        
        pscores <- sort(getPscores(object, "diffusion"))
        pscores[pscores < capPscores] <- capPscores
        
        if (pscores[1] >= threshold) {
            message("No node is below the p-value threshold.")
        } else {
            last <- min(nlimit, tail(which(pscores < threshold), 1))
            pscores <- pscores[1:last]
            pscores <- pscores[order(names(pscores))]
            
            nodeIds <- names(pscores)
            nodeNames <- as.character(sapply(nodeIds, function(id) {
                ans <- data@keggdata@id2name[[id]]
                if (length(ans) == 0) 
                    return(NULL)
                
                ans <- ans[1]
                if (nchar(ans) > LabelLengthAtPlot) 
                    ans <- paste0(substr(ans, 1, LabelLengthAtPlot), "...")
                return(ans)
            }))
            nodeCom <- V(getGraph(data))[nodeIds]$com
            nodeTypes <- sapply(
                nodeCom, function(x) 
                    switch(
                        x, 
                        "1" = "Pathway", 
                        "2" = "Module", 
                        "3" = "Enzyme", 
                        "4" = "Reaction", 
                        "5" = "Compound"))
            
            out.diffusion <- data.frame(
                nodeIds,
                nodeTypes,
                nodeNames, 
                pscores, 
                stringsAsFactors = FALSE)[order(nodeCom), ]
            names(out.diffusion) <- c(
                "KEGG id", 
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
        
        pscores <- sort(getPscores(object, "pagerank"))
        pscores[pscores < capPscores] <- capPscores
        
        if (pscores[1] >= threshold) {
            message("No node is below the p-value threshold.")
        } else {
            last <- min(nlimit, tail(which(pscores < threshold), 1))
            pscores <- pscores[1:last]
            pscores <- pscores[order(names(pscores))]
            
            nodeIds <- names(pscores)
            nodeNames <- as.character(sapply(nodeIds, function(id) {
                ans <- data@keggdata@id2name[[id]][1]
                if (length(ans) == 0) return(NULL)
                
                if (nchar(ans) > LabelLengthAtPlot) 
                    ans <- paste0(substr(ans, 1, LabelLengthAtPlot), "...")
                return(ans)
            }))
            nodeCom <- V(getGraph(data))[nodeIds]$com
            nodeTypes <- listCategories()[nodeCom]
            
            out.pagerank <- data.frame(
                "KEGG id" = nodeIds,
                "Entry type" = nodeTypes,
                "KEGG name" = nodeNames, 
                "p.score" = pscores, 
                stringsAsFactors = FALSE)[order(nodeCom), ]

            rownames(out.pagerank) <- NULL
            
            message("Done.")
            
            return(out.pagerank)
        }
    }
}