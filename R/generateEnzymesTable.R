#' @include generateResultsTable.R
#' 
#' @description 
#' In general, \code{generateResultsTable}, \code{generateEnzymesTable} 
#' and \code{generateResultsGraph} provide the results of an enrichment 
#' in several formats. 
#' 
#' Function \code{generateResultsTable} returns a table 
#' that contains the best hits from
#' a \code{\link{FELLA.USER}} object 
#' with a successful enrichment analysis.
#' Similarly, \code{generateEnzymesTable} returns 
#' a data frame with the best scoring enzyme families and their 
#' annotated genes.
#' 
#' @details 
#' Functions \code{generateResultsTable} and 
#' \code{generateEnzymesTable} need a 
#' \code{\link{FELLA.DATA}} object and a 
#' \code{\link{FELLA.USER}} object with a successful enrichment.
#' \code{generateResultsTable} provides the entries 
#' whose p-score is below the chosen \code{threshold} in a tabular format. 
#' \code{generateEnzymesTable} returns a table 
#' that contains (1) the enzymes that are below the user-defined 
#' p-score threshold, along with (2) the genes that belong to 
#' the enzymatic families in the organism defined in the database, 
#' and (3) GO labels of such enzymes, if \code{mart.options} is 
#' not \code{NULL} and points to the right database.
#' 
#' @inheritParams .params
#' @param method one in \code{"diffusion"}, \code{"pagerank"}
#'
#' @return \code{generateEnzymesTable} returns a 
#' data.frame that contains the enzymes below the \code{p.score} threshold,
#' along with their genes and GO labels
#' 
#' @rdname export-funs
#' 
#' @import igraph
#' @export
generateEnzymesTable <- function(
    method = "diffusion", 
    threshold = 0.05, 
    nlimit = 250, 
    LabelLengthAtPlot = 45, 
    capPscores = 1e-6, 
    mart.options = list(
        biomart = "ensembl", dataset = "hsapiens_gene_ensembl"),
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
            "Mehod ", method, " has not been executed yet. ",  
            "Returning NULL...")
        return(invisible())
    } 
    
    message("Writing ", method, " enzymes...")
    
    id.ec <- getCom(data, level = 3, format = "id")
    pscores.ec <- sort(getPscores(object, method)[id.ec])
    pscores.ec[pscores.ec < capPscores] <- capPscores
    
    if (pscores.ec[1] >= threshold) {
        message("No enzyme is below the p-score threshold.")
        return(NULL)
    } 
    nodePscores <- head(pscores.ec[pscores.ec < threshold], nlimit)
    
    nodeIds <- names(nodePscores)
    nodeNames <- vapply(
        getName(data, id = nodeIds), 
        function(id) {
            ans <- id[1]
            if (is.null(ans)) return(NULL)
            
            if (nchar(ans) > LabelLengthAtPlot) 
                ans <- paste0(substr(ans, 1, LabelLengthAtPlot), "...")
            return(ans)
        }, 
        FUN.VALUE = character(1))
    
    g <- getGraph(data)
    nodeGenes <- vapply(
        V(g)[nodeIds]$entrez, 
        function(genes) paste(genes, collapse = ";"), 
        FUN.VALUE = character(1)
    )

    out.df <- data.frame(
        "EC_number" = nodeIds,
        "p.score" = nodePscores, 
        "EC_name" = nodeNames, 
        "Genes" = nodeGenes, 
        stringsAsFactors = FALSE)
    rownames(out.df) <- NULL
    
    if (!is.null(mart.options)) {
        g <- addGOToGraph(
            graph = g, GOterm = NULL, mart.options = mart.options)
        nodeGO <- vapply(
            V(g)[nodeIds]$GO, 
            function(goterms) paste(goterms, collapse = ";"), 
            FUN.VALUE = character(1)
        )
        out.df$GO_id <- nodeGO
    }
    
    message("Done.")
    
    return(out.df)
}