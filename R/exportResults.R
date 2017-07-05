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
#' @inheritParams .method
#' @inheritParams .threshold
#' @inheritParams .plimit
#' @inheritParams .nlimit
#' @inheritParams .object
#' @inheritParams .data
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
# Will remove this and move it to suggests # @importFrom knitr knit2pdf
exportResults <- function(
    format = "csv", 
    file = "myOutput", 
    method = "diffusion", 
    threshold = 0.05, 
    plimit = 15, 
    nlimit = 250, 
    object = NULL, 
    data = NULL, 
    ...) {
    
    if (!is.FELLA.DATA(data)) {
        stop("'data' is not a FELLA.DATA object")
    } else if (data@keggdata@status != "loaded"){
        stop("'data' points to an empty FELLA.DATA object")
    }
    
    # Writing tables that summarise the results
    if (format == "csv") {
        message("Exporting to a csv file...")
        if (method %in% c("hypergeom", "all")) {
            #       filename <- paste0(file, "_hypergeom.csv")
            df <- generateResultsTable(
                method = "hypergeom", 
                threshold = threshold, 
                plimit = plimit, 
                object = object, 
                data = data)
            utils::write.table(
                df, 
                file = file, 
                sep = ",", 
                row.names = FALSE)
        }
        if (method %in% c("diffusion", "all")) {
            #       filename <- paste0(file, "_diffusion.csv")
            df <- generateResultsTable(
                method = "diffusion", 
                threshold = threshold, 
                nlimit = nlimit, 
                object = object, 
                data = data)
            utils::write.table(
                df, 
                file = file, 
                sep = ",", 
                row.names = FALSE)
        }
        if (method %in% c("pagerank", "all")) {
            #       filename <- paste0(file, "_pagerank.csv")
            df <- generateResultsTable(
                method = "pagerank", 
                threshold = threshold, 
                nlimit = nlimit, 
                object = object, 
                data = data)
            utils::write.table(
                df, 
                file = file, 
                sep = ",", 
                row.names = FALSE)
        }
        message("Done")
        return(invisible())
    }
    # Option to export enzymes with genes and GO annotations
    if (format == "enzyme") {
        if (method %in% c("diffusion", "pagerank")) {
            df <- generateEnzymesTable(
                method = method, 
                threshold = threshold, 
                nlimit = nlimit, 
                object = object, 
                data = data)
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
    }
    
    # Exporting to igraph objects in RData format
    if (format == "igraph") {
        message("Exporting to a RData file using 'igraph' object...")
        if (method %in% c("hypergeom", "all")) {
            #       filename <- paste0(file, "_hypergeom.RData")
            graph <- generateResultsGraph(
                method = "hypergeom", 
                threshold = threshold, 
                plimit = plimit, 
                object = object, 
                data = data)
            save(graph, file = file)
        }
        if (method %in% c("diffusion", "all")) {
            #       filename <- paste0(file, "_diffusion.RData")
            graph <- generateResultsGraph(
                method = "diffusion", 
                threshold = threshold, 
                nlimit = nlimit, 
                object = object, 
                data = data)
            save(graph, file = file)
        }
        if (method %in% c("pagerank", "all")) {
            #       filename <- paste0(file, "_pagerank.RData")
            graph <- generateResultsGraph(
                method = "pagerank", 
                threshold = threshold, 
                nlimit = nlimit, 
                object = object, 
                data = data)
            save(graph, file = file)
        }
        message("Done")
        return(invisible())
    }
    
    # If pdf is specified an image will be generated
    if (format == "png") {
        message("Generating png image...")
        
        warning("This feature is temporary down :(")
        #     plot(object, 
        #          method = method, 
        #          data = data, 
        #          method = "diffusion", 
        #          threshold = 0.05, 
        #          plimit = 15, 
        #          nlimit = 250, 
        #          ...
        #     )
        
        message("Done")
        return(invisible())
    }
    
    # If pdf is specified a report will be generated
    if (format == "pdf") {
        message("Generating pdf report...")
        
        message("Temporarily unavailable. Will be fixed soon")
        # knit2pdf(
        #     input = "/home/sergi/Rstuffbro/FEllA/data/report.Rnw")
        # knitr::knit2pdf(
        #   input = "/home/sergi/Rstuffbro/FEllA/data/report.Rnw", 
        #   output = file)
        # output = "report.tex")
        
        message("Done")
        return(invisible())
    }
    
    # Other formats that can be exported with igraph
    message("Exporting to custom format ", format, " using igraph...")
    if (method %in% c("hypergeom", "all")) {
        #     filename <- paste0(file, "_hypergeom.", format)
        graph <- generateResultsGraph(
            method = "hypergeom", 
            threshold = threshold, 
            plimit = plimit, 
            object = object, 
            data = data)
        write.graph(graph = graph, file = file, format = format)
    }
    if (method %in% c("diffusion", "all")) {
        #     filename <- paste0(file, "_diffusion.", format)
        graph <- generateResultsGraph(
            method = "diffusion", 
            threshold = threshold, 
            nlimit = nlimit, 
            object = object, 
            data = data)
        write.graph(graph = graph, file = file, format = format)
    }
    if (method %in% c("pagerank", "all")) {
        #     filename <- paste0(file, "_pagerank.", format)
        graph <- generateResultsGraph(
            method = "pagerank", 
            threshold = threshold, 
            nlimit = nlimit, 
            object = object, 
            data = data)
        write.graph(graph = graph, file = file, format = format)
    }
    message("Done")
    return(invisible())
}