#' @include generateResultsGraph.R
#' 
#' @title Generate and manipulate tables and sub-networks from an enrichment
#' 
#' @description 
#' Function \code{exportResults} 
#' is a wrapper around \code{generateResultsTable}, 
#' \code{generateEnzymesTable} and \code{generateResultsGraph} 
#' to write the results to files.
#' 
#' @details
#' Function \code{exportResults} writes the enrichment results 
#' as the specified filetype.
#' Options are: a csv table (\code{"csv"}), 
#' an enzyme csv table (\code{"enzyme"}) 
#' an \pkg{igraph}
#' object as an \code{RData} file, 
#' or any format supported by igraph's 
#' \code{\link[igraph]{write_graph}}.
#' 
#' @param format Character, one of: \code{"csv"} for regular 
#' results table, \code{"enzyme"} for table with enzyme data, 
#' \code{"igraph"} for igraph format. 
#' Alternatively, any format supported by igraph, 
#' see \code{\link[igraph]{write_graph}}
#' @param file Character specifying the output file name
#' @param ... Optional arguments for the plotting function 
#' in \code{plotGraph}. Arguments passed to the exporting function 
#' in \code{exportResults}. Ignored otherwise.
#' @inheritParams .params
#'
#' @return \code{exportResults} returns \code{invisible()}, 
#' but as a side effect the specified \code{file} is created.
#' 
#' @rdname export-funs
#' 
#' @template refs_gene_ontology
#' 
#' @examples 
#' ## First generate a toy enrichment
#' library(igraph)
#' data(FELLA.sample)
#' data(input.sample)
#' ## Enrich input
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' 
#' ######################
#' ## Results table
#' tab.res <- generateResultsTable(
#' method = "hypergeom",
#' threshold = 0.1, 
#' object = obj, 
#' data = FELLA.sample)
#' head(tab.res)
#' 
#' tab.res <- generateResultsTable(
#' method = "diffusion",
#' threshold = 0.1, 
#' object = obj, 
#' data = FELLA.sample)
#' head(tab.res)
#' 
#' ######################
#' ## Use wrapper to write the table to a file
#' out.file <- tempfile()
#' exportResults(
#' format = "csv", 
#' threshold = 0.1, 
#' file = out.file, 
#' object = obj, 
#' data = FELLA.sample)
#' tab.wrap <- read.csv(out.file)
#' head(tab.wrap)
#' 
#' ######################
#' ## Enzymes table
#' tab.ec <- generateEnzymesTable(
#' threshold = 0.1, 
#' object = obj, 
#' data = FELLA.sample, 
#' mart.options = NULL)
#' head(tab.ec)
#' 
#' ######################
#' ## Generate graph
#' g.res <- generateResultsGraph(
#' method = "pagerank", 
#' threshold = 0.1, 
#' object = obj, 
#' data = FELLA.sample)
#' g.res
#' 
#' ## Plot graph (without GO terms)
#' plotGraph(g.res)
#' 
#' ## Add similarity to the GO CC term "mitochondrion"
#' \dontrun{
#' g.cc <- FELLA:::addGOToGraph(
#' graph = g.res, 
#' GOterm = "GO:0005739")
#' 
#' ## Plot graph (with GO terms)
#' plotGraph(g.cc)
#' 
#' ## Without the CC
#' any(V(g.res)$GO.simil >= 0)
#' ## With the CC
#' v.cc <- unlist(V(g.cc)$GO.simil)
#' sum(v.cc >= 0, na.rm = TRUE)
#' ## Similarity values
#' table(v.cc)
#' }
#' 
#' @import igraph
#' @export
exportResults <- function(
    format = "csv", 
    file = "myOutput", 
    method = "diffusion", 
    object = NULL, 
    data = NULL, 
    ...) {
    
    if (!is.FELLA.DATA(data)) {
        stop("'data' is not a FELLA.DATA object")
    } else if (getStatus(data) != "loaded"){
        stop("'data' points to an empty FELLA.DATA object")
    }
    
    # Writing tables that summarise the results
    if (format == "csv") {
        message("Exporting to a csv file...")
        df <- generateResultsTable(
                method = method, 
                object = object, 
                data = data, 
                ...)
        utils::write.table(
            df, 
            file = file, 
            sep = ",", 
            row.names = FALSE)
    } else if (format == "enzyme") {
        # Option to export enzymes with genes and GO annotations
        if (method %in% c("diffusion", "pagerank")) {
            df <- generateEnzymesTable(
                method = method, 
                object = object, 
                data = data, 
                ...)
            utils::write.table(
                df, 
                file = file, 
                sep = ",", 
                row.names = FALSE)
        } else {
            stop(
                "Enzymes are only reported in diffusion and pagerank, ",
                "but not in method ", method)
        }
    } else {
        # Exporting the graph 
        graph <- generateResultsGraph(
            method = method, 
            object = object, 
            data = data, 
            ...)
        
        if (format == "igraph") {
            message("Exporting to a RData file using 'igraph' object...")
            save(graph, file = file)
        } else {
            message(
                "Exporting to the format ", format, " using igraph...")
            write.graph(graph = graph, file = file, format = format)
        }
        
    }
    
    message("Done")
    return(invisible())
}