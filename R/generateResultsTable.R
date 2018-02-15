#' @inheritParams .params
#'
#' @return \code{generateResultsTable} returns a 
#' data.frame that contains the nodes below the \code{p.score} threshold 
#' from an enrichment analysis
#' 
#' @name export-funs
#' @rdname export-funs
#' 
#' @import igraph
#' @export
generateResultsTable <- function(
    method = "diffusion", 
    threshold = 0.05, 
    plimit = 15, 
    nlimit = 250, 
    LabelLengthAtPlot = 45, 
    capPscores = 1e-6, 
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
    
    if (is.na(getValid(object, method)) | !getValid(object, method)) {
        warning(
            "Mehod ", method, " has not been executed yet. ",  
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
            pscores <- head(pscores[pscores < threshold], plimit)
            
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
    } else {
        # Diffusion and pagerank
        message("Writing ", method, " results...")
        
        pscores <- sort(getPscores(object, method))
        pscores[pscores < capPscores] <- capPscores
        
        if (pscores[1] >= threshold) {
            message("No node is below the p-score threshold.")
        } else {
            pscores <- head(pscores[pscores < threshold], nlimit)
            pscores <- pscores[order(names(pscores))]
            
            nodeIds <- names(pscores)
            # Take names (1st name in KEGG, abbreviate if it's too long)
            nodeNames <- as.character(vapply(nodeIds, function(id) {
                ans <- data@keggdata@id2name[[id]]
                if (length(ans) == 0) 
                    return("")
                
                ans <- ans[1]
                if (nchar(ans) > LabelLengthAtPlot) 
                    ans <- paste0(substr(ans, 1, LabelLengthAtPlot), "...")
                return(ans)
            }, FUN.VALUE = character(1)))
            nodeCom <- V(getGraph(data))[nodeIds]$com
            nodeTypes <- listCategories()[nodeCom]
            
            out <- data.frame(
                "KEGG id" = nodeIds,
                "Entry type" = nodeTypes,
                "KEGG name" = nodeNames, 
                "p.score" = pscores, 
                stringsAsFactors = FALSE)[order(nodeCom), ]
            rownames(out) <- NULL
            
            message("Done.")
            
            return(out)
        }
    }
}