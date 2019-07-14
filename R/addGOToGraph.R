#' @include exportResults.R
#' 
#' @details
#' Function \code{addGOToGraph} takes and returns 
#' a graph object with class 
#' \pkg{igraph} 
#' adding the following attributes: 
#' GO labels in \code{V(graph)$GO}, and 
#' semantic similarities in \code{V(graph)$GO.simil} if 
#' \code{GOterm != NULL}. 
#' 
#' The GO database describes genes in terms of three ontologies: 
#' molecular function (MF), biological process (BP) and 
#' cellular component (CC) [Gene Ontology Consortium, 2015].
#' The user can be interested in finding which enzymatic families 
#' reported with a low \code{p.score}
#' are closest to a particular GO term. 
#' To assess similarity between GO labels, FELLA uses the 
#' semantic similarity defined in [Yu, 2010] and their implementation 
#' in the \pkg{GOSemSim} R package. 
#' The user will obtain, for each enzymatic family, the closest GO 
#' term to his or her GO query and the semantic similarity between them. 
#' Exact matches have a similarity of \code{1}. 
#' Function \code{plotGraph} detects the presence 
#' of the GO similarity option and plots its magnitude.
#' 
#' 
#' @inheritParams .params
#'
#' @return \code{addGOToGraph} returns 
#' an \pkg{igraph} object, 
#' which is the input \code{graph} with 
#' extra attributes: GO labels in \code{V(graph)$GO}, and 
#' semantic similarities in \code{V(graph)$GO.simil} if 
#' \code{GOterm != NULL}
#' 
#' @rdname export-funs
#' 
#' @import igraph
#' @import plyr
#' @export
addGOToGraph <- function(
    graph = NULL, 
    GOterm = NULL, 
    godata.options = list(
        OrgDb = "org.Hs.eg.db", ont = "CC"),
    mart.options = list(
        biomart = "ensembl", dataset = "hsapiens_gene_ensembl")) {
    
    # check if biomaRt is available
    if (!requireNamespace("biomaRt", quietly = TRUE)) {
        stop(
            "Package biomaRt must be installed to add GO labels", 
            call. = FALSE)
    }
    
    if (!is.igraph(graph)) {
        warning("'graph' is not an igraph object. Leaving it as it was...")
        return(graph)
    }
    
    if (vcount(graph) == 0) return(graph)
    
    # First: map entrez genes to GO terms
    # All entrez ids:
    entrez.all <- unique(unlist(V(graph)$entrez))
    
    # Map them
    mart.go <- do.call(biomaRt::useMart, mart.options)
    df.entrez2go <- biomaRt::getBM(
        attributes = c('entrezgene_id', 'go_id'), 
        filters = 'entrezgene_id', 
        values = entrez.all, 
        mart = mart.go)
    list.entrez2go <- plyr::dlply(
        df.entrez2go, 
        "entrezgene_id", 
        function(df) grep("GO:\\d+", df$go_id, value = TRUE)
    )
    # Add GO terms to graph
    V(graph)$GO <- plyr::llply(
        V(graph)$entrez, 
        function(x) {
            if (!is.null(x)) {
                unique(unlist(list.entrez2go[as.character(x)]))
            }
        }
    )
    
    if (is.null(GOterm)) {
        message(
            "Null GOterm provided to addGOToGraph. ", 
            "Only the GO labels will be added. ",
            "To include similarity values as well, please specify a GOterm")
        return(graph)
    }
    
    
    # Second: add similarity to desired term
    # 
    # Check that GOSemSim is available
    if (!requireNamespace("GOSemSim", quietly = TRUE)) {
        stop(
            "Package GOSemSim must be installed to add GO semantic similarity", 
            call. = FALSE)
    }
    
    semData <- do.call(GOSemSim::godata, godata.options)
    # This outputs a list to keep the names of the best GO hit
    go.simil <- lapply(V(graph)$GO, FUN = function(entrez) {
        if (!is.null(entrez)) {
            # This is a row vector
            val <- GOSemSim::mgoSim(
                GOterm, entrez, semData = semData, combine = NULL
            )[1, ]
            return(val[which.max(val)])
        } else {
            return(NA)
        }
    })
    
    V(graph)$GO.simil <- go.simil
    return(graph)
}
