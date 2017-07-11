#' Export the enrichment results in several file formats
#' 
#' Function \code{exportResults} writes the enrichment results in a 
#' proper layout and filetype. 
#' It can be a plot image (png), a table (csv), 
#' an \code{\link[igraph]{igraph}} object 
#' or a pdf report [under construction]
#' 
#' @param format Character, one of: \code{"csv"}, \code{"igraph"}, 
#' \code{"png"}, [\code{"pdf"} soon] 
#' @param file Character specifying the output 
#' directory for the exported files
#' @inheritParams .params
#' @param ... Developer parameter, currently ignored
#'
#' @return Return value is \code{invisible()}, 
#' but as a permanent action the 
#' specified \code{file} is created.
#' 
#' @examples 
#' data(FELLA.sample)
#' data(input.sample)
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' out.file <- tempfile()
#' exportResults(
#' format = "csv", 
#' threshold = 0.1, 
#' file = out.file, 
#' object = obj, 
#' data = FELLA.sample)
#' df <- read.csv(out.file)
#' head(df)
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