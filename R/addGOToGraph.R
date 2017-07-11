#' Internal function to add the GO semantic similarity attribute to the 
#' results graph object
#' 
#' Function \code{addGOToGraph} takes and returns 
#' a graph object with class 
#' \code{\link[igraph]{igraph}} adding the attribute 
#' \code{GO.simil} for semantic similarity.
#' 
#' @param graph An \code{\link[igraph]{igraph}} object, typically a small one 
#' coming from an enrichment procedure
#' @inheritParams .params
#' @param godata.options List, options for the database creator 
#' \code{\link[GOSemSim]{godata}}
#' @param mart.options List, options for the \code{biomaRt} function
#' \code{\link[biomaRt]{getBM}}. Importantly, this defines the organism, 
#' see \code{\link[biomaRt]{listDatasets}} for possibilities
#'
#' @return An \code{\link{igraph}} object that contains 
#' an extra attribute: \code{GO.simil}
#' 
#' @examples 
#' ## This function is internal
#' library(igraph)
#' data(FELLA.sample)
#' data(input.sample)
#' ## Enrich input
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' ## Generate graph
#' g <- generateResultsGraph(
#' threshold = 0.1, 
#' object = obj, 
#' data = FELLA.sample)
#' g
#' ## Add the cellular component
#' g.cc <- FELLA:::addGOToGraph(
#' graph = g, 
#' GOterm = "GO:0005739")
#' 
#' ## Without the CC
#' any(V(g)$GO.simil >= 0)
#' ## With the CC
#' v.cc <- unlist(V(g.cc)$GO.simil)
#' sum(v.cc >= 0, na.rm = TRUE)
#' table(v.cc)
#' 
#' @import igraph
#' @import plyr
addGOToGraph <- function(
    graph = NULL, 
    GOterm = NULL, 
    godata.options = list(
        OrgDb = "org.Hs.eg.db", ont = "CC"),
    mart.options = list(
        biomart = "ensembl", dataset = "hsapiens_gene_ensembl"),
    GONamesAsLabels = TRUE) {
    
    # check if GOSemSim and biomaRt are available
    if (!requireNamespace("GOSemSim", quietly = TRUE)) {
        stop(
            "Package GOSemSim must be installed to add GO labels", 
            call. = FALSE)
    }
    if (!requireNamespace("biomaRt", quietly = TRUE)) {
        stop(
            "Package biomaRt must be installed to add GO labels", 
            call. = FALSE)
    }
    
    if (is.null(GOterm))
        return(graph)
    
    # First: map entrez genes to GO terms
    # All entrez ids:
    entrez.all <- unique(unlist(V(graph)$entrez))
    
    # Map them
    mart.go <- do.call(biomaRt::useMart, mart.options)
    df.entrez2go <- biomaRt::getBM(
        attributes = c('entrezgene', 'go_id'), 
        filters = 'entrezgene', 
        values = entrez.all, 
        mart = mart.go)
    list.entrez2go <- plyr::dlply(
        df.entrez2go, 
        "entrezgene", 
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
    
    
    # Second: add similarity to desired term
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
