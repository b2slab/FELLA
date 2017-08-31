#' Generate enzyme tables with genes and GO annotations
#' 
#' Function \code{generateEnzymesTable} returns a table 
#' that contains the selected enzymes (with additional data) 
#' of a \code{\link[FELLA]{FELLA.USER}} object 
#' with a successful enrichment analysis.
#' 
#' @inheritParams .params
#' @param method one in \code{"diffusion"}, \code{"pagerank"}
#' @param ... ignored arguments
#'
#' @return A table that contains the enzymes 
#' along with genes and GO labels
#' 
#' @examples 
#' data(FELLA.sample)
#' data(input.sample)
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' tab <- generateEnzymesTable(
#' threshold = 0.1, 
#' object = obj, 
#' data = FELLA.sample)
#' head(tab)
#' 
#' @import igraph
#' @export
generateEnzymesTable <- function(
    method = "diffusion", 
    threshold = 0.005, 
    nlimit = 250, 
    LabelLengthAtPlot = 45, 
    capPscores = 1e-6, 
    object = NULL, 
    data = NULL, 
    ...) {
    
    checkArgs <- checkArguments(
        method = method, 
        threshold = threshold, 
        nlimit = nlimit, 
        LabelLengthAtPlot = LabelLengthAtPlot, 
        object = object, 
        data = data)
    if (!checkArgs$valid)
        stop("Bad argument when calling function 'generateEnzymesGraph'.")
    
    if (getStatus(data) != "loaded"){
        stop("'data' points to an empty FELLA.DATA object")
    }
    
    if (!(method %in% c("diffusion", "pagerank"))) {
        warning(
            "Method should be one of: 'diffusion', 'pagerank'", 
            " but it is ", method, ". Returning NULL...")
        return(NULL)
    }
    if (is.na(getValid(object, method)) | !getValid(object, method)) {
        warning(
            paste0("Mehod ", method, " has not been executed yet. "),  
            "Returning NULL...")
        return(invisible())
    } 
    
    message("Writing ", method, "enzymes...")
    
    id.ec <- getCom(data, level = 3, format = "id")
    pscores.ec <- sort(getPscores(object, method)[id.ec])
    pscores.ec[pscores.ec < capPscores] <- capPscores
    
    if (pscores.ec[1] >= threshold) {
        message("No enzyme is below the p-score threshold.")
        return(NULL)
    } 
    nodePscores <- head(pscores.ec[pscores.ec < threshold], nlimit)
    
    nodeIds <- names(nodePscores)
    nodeNames <- sapply(
        getName(data, id = nodeIds), 
        function(id) {
            ans <- id[1]
            if (is.null(ans)) return(NULL)
            
            if (nchar(ans) > LabelLengthAtPlot) 
                ans <- paste0(substr(ans, 1, LabelLengthAtPlot), "...")
            return(ans)
        })
    
    g <- getGraph(data)
    nodeGenes <- sapply(
        V(g)[nodeIds]$entrez, 
        function(genes) paste(genes, collapse = ";")
    )

    out.df <- data.frame(
        "EC_number" = nodeIds,
        "p.score" = nodePscores, 
        "EC_name" = nodeNames, 
        "Genes" = nodeGenes, 
        stringsAsFactors = FALSE)
    rownames(out.df) <- NULL
    
    if ("GO" %in% list.vertex.attributes(g)) {
        nodeGO <- sapply(
            V(g)[nodeIds]$GO, 
            function(goterms) paste(names(goterms), collapse = ";")
        )
        nodeGOname <- sapply(
            V(g)[nodeIds]$GO, 
            function(goterms) paste(goterms, collapse = ";")
        )
        out.df$GO_id <- nodeGO
        out.df$GO_name <- nodeGOname
    }
    
    message("Done.")
    
    return(out.df)
}