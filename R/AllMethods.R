#' \code{"summary"} is an S4 method to show a summary 
#' of the FELLA.USER object
#'
# @param object A \code{\link{FELLA.USER}} object
# Already in the show documentation
#' 
#' @return \code{summary} returns a list with the summary data
#' 
#' @rdname FELLA.USER
#' @exportMethod summary
setMethod("summary", signature = "FELLA.USER", function(object) {
    breakline <- "\n---------------------------------------------------\n"
    #dic <- object@dictionary
    output <- list()
    
    if (is.na(object@hypergeom@valid)) output$hypergeom <- "Not performed"
    else if (!object@hypergeom@valid) output$hypergeom <- "Failed"
    else {
        n.show <- min(30, length(object@hypergeom@pvalues))
        p.values <- sort(object@hypergeom@pvalues)[1:n.show]
        
        output$hypergeom <- data.frame(
            "Description" = names(p.values), 
            "p.value" = signif(p.values, digits = 4))
    }
    
    if (is.na(object@diffusion@valid)) output$diffusion <- "Not performed"
    else if (!object@diffusion@valid) output$diffusion <- "Failed"
    else {
        dif.select <- which(object@diffusion@pscores < 0.05)
        
        out.pscores <- signif(object@diffusion@pscores[dif.select], digits = 3)
        smallest <- which(out.pscores < 2e-16)
        out.pscores <- format(out.pscores)
        out.pscores[smallest] <- "<2e-16"
        
        out.names <- names(out.pscores)
        #out.description <- dic[out.names]
        #out.highlight <- out.names %in% object@diffusion@highlight
        
        out.order <- order(out.names)
        
        output$diffusion <- data.frame(
            "KEGG id" = out.names, "p.score" = out.pscores, 
            row.names = NULL)[out.order, ]
        row.names(output$diffusion) <- NULL
    }
    
    if (is.na(object@pagerank@valid)) output$pagerank <- "Not performed"
    else if (!object@pagerank@valid) output$pagerank <- "Failed"
    else {
        dif.select <- which(object@pagerank@pscores < 0.05)
        
        out.pscores <- signif(object@pagerank@pscores[dif.select], digits = 3)
        smallest <- which(out.pscores < 2e-16)
        out.pscores <- format(out.pscores)
        out.pscores[smallest] <- "<2e-16"
        
        out.names <- names(out.pscores)
        #out.description <- dic[out.names]
        #out.highlight <- out.names %in% object@diffusion@highlight
        
        
        output$pagerank <- data.frame(
            "KEGG id" = out.names, 
            "p.score" = out.pscores)
        row.names(output$pagerank) <- NULL
    }
    
    return(output)
})


#' \code{"show"} is an S4 method to show a FELLA.DATA object
#'
#' @param object A \code{\link[FELLA]{FELLA.DATA}} object
#' 
#' @return \code{show} returns \code{invisible()}
#' 
#' @rdname FELLA.DATA
#' @exportMethod show
setMethod("show", signature = "FELLA.DATA", function(object) {
    breakline <- "\n---------------------------------------------------\n"
    
    cat("General data:\n")
    if (vcount(object@keggdata@graph) == 0) {
        cat("- KEGG graph not loaded.\n")
    } else {
        cat("- KEGG graph is ready.\n")
    }
    if (length(object@keggdata@id2name) == 0) {
        cat("- KEGG names not loaded.")
    } else {
        cat("- KEGG names are ready.")
    }
    
    cat(breakline)
    
    cat("Hypergeometric test:\n")
    if (prod(dim(object@hypergeom@matrix)) == 1) {
        cat("- Matrix not loaded.")
    } else {
        cat("- Matrix is ready.")
    }
    
    cat(breakline)
    
    cat("Heat diffusion:\n")
    if (prod(dim(object@diffusion@matrix)) == 1) {
        cat("- Matrix not loaded.\n")
    } else {
        cat("- Matrix is ready.\n")
    }
    if (length(object@diffusion@rowSums) == 0 || 
        length(object@diffusion@squaredRowSums) == 0) {
        cat("- RowSums not loaded.")
    } else {
        cat("- RowSums are ready.")
    }
    
    cat(breakline)
    
    cat("PageRank:\n")
    if (prod(dim(object@pagerank@matrix)) == 1) {
        cat("- Matrix not loaded.\n")
    } else {
        cat("- Matrix is ready.\n")
    }
    if (length(object@pagerank@rowSums) == 0 || 
        length(object@pagerank@squaredRowSums) == 0) {
        cat("- RowSums not loaded.\n")
    } else {
        cat("- RowSums are ready.\n")
    }
    
    invisible()
})

#' \code{"show"} is an S4 method to show a FELLA.USER object
#'
#' @param object A \code{\link{FELLA.USER}} object
#' 
#' @return \code{show} returns \code{invisible()}
#' 
#' @rdname FELLA.USER
#' @exportMethod show
setMethod("show", signature = "FELLA.USER", function(object) {
    breakline <- function() {
        cat(fill = TRUE)
        cat("---------------------------------------------------", fill = TRUE)
    }
    
    cat("Compounds in the input: ")
    if (length(getInput(object)) == 0) {
        cat("empty", fill = TRUE)
    } else {
        cat(length(getInput(object)), fill = TRUE)
        print(getInput(object))
    }
    if (length(getBackground(object)) == 0) {
        cat("Background compounds: all available compounds (default)")
    } else {
        cat("Background compounds:", length(getBackground(object)))
    }
    
    breakline()
    
    cat("Hypergeometric test: ")
    if (is.na(getValid(object, "hypergeom"))) cat("not performed")
    else if (!getValid(object, "hypergeom")) cat("error during execution")
    else {
        cat("ready.", fill = TRUE)
        n.show <- min(15, length(
            getPscores(object = object, type = "hypergeom")))
        cat("Top", n.show, "p-values:", fill = TRUE)
        print(sort(getPscores(object, "hypergeom"))[1:n.show])
    }
    
    breakline()
    
    cat("Heat diffusion: ")
    if (is.na(getValid(object, "diffusion"))) cat("not performed")
    else if (!getValid(object, "diffusion")) cat("error during execution")
    else {
        cat("ready.", fill = TRUE)
        cat("Significant nodes (0.05): ", 
            sum(getPscores(object, "diffusion") < 0.05))
    }
    
    breakline()
    
    cat("PageRank: ")
    if (is.na(getValid(object, "pagerank"))) cat("not performed")
    else if (!getValid(object, "pagerank")) cat("error during execution")
    else {
        cat("ready.", fill = TRUE)
        cat("Significant nodes (0.05): ", 
            sum(getPscores(object, "pagerank") < 0.05))
    }
    
    invisible()
})

#' \code{"plot"} is an S4 method to plot the results 
#' from a FELLA.USER object
#'
#' @param x A \code{\link{FELLA.USER}} object
#' @inheritParams .methodSingle
#' @inheritParams .threshold
#' @inheritParams .thresholdConnectedComponent
#' @inheritParams .plimit
#' @inheritParams .nlimit
#' @inheritParams .layout 
#' @param filename Character; optional file name to save the plot
#' @inheritParams .splitByConnectedComponent
#' @inheritParams .askPlots
#' @inheritParams .GO.CellularComponent
#' @inheritParams .GONamesAsLabels
#' @inheritParams .LabelLengthAtPlot
#' @inheritParams .data
#' @param ... Additional arguments passed to plotting functions
#' 
#' 
#' @return \code{plot} returns a layout if \code{layout = T}, 
#' otherwise \code{invisible()}
#' 
#' @rdname FELLA.USER
#' @importFrom grDevices dev.off png
#' @importFrom graphics par mtext
#' @exportMethod plot
setMethod(
    "plot", 
    signature(x = "FELLA.USER", y = "missing"), 
    function(
        x = 1, 
        method = "hypergeom", 
        threshold = 0.005, 
        plimit = 15, 
        nlimit = 250, 
        layout = FALSE, 
        filename = NULL, 
        splitByConnectedComponent = FALSE, 
        askPlots = TRUE,  
        thresholdConnectedComponent = 0.05, 
        LabelLengthAtPlot = 22, 
        data = NULL, 
        ...) {
        
        checkArgs <- checkArguments(
            method = method, 
            threshold = threshold, 
            plimit = plimit, 
            nlimit = nlimit, 
            layout = layout, 
            splitByConnectedComponent = splitByConnectedComponent, 
            askPlots = askPlots, 
            thresholdConnectedComponent = thresholdConnectedComponent, 
            LabelLengthAtPlot = LabelLengthAtPlot, 
            object = x, 
            data = data)
        if (!checkArgs$valid)
            stop("Bad argument when calling function 'FELLA::plot'.")
        
        if (data@keggdata@status != "loaded"){
            stop("'data' points to an empty FELLA.DATA object")
        }
        
        if (method == "hypergeom") {
            if (is.na(x@hypergeom@valid) || !x@hypergeom@valid) {
                stop("Hypergeometric test is not ready yet.")
            } 
            
            graph.bipartite <- generateResultsGraph(
                method = method, 
                threshold = threshold, 
                plimit = plimit,
                object = x, 
                data = data)
            
            if (!is.null(filename)) 
                grDevices::png(filename = filename, height = 1000, width = 800)
            
            ans.return <- plotBipartite(
                graph = graph.bipartite, 
                layout = layout, 
                main = "Hypergeometric test results", 
                ...)
            
            if (!is.null(filename)) grDevices::dev.off()
            
            return(invisible(ans.return))
            
        } else if (method == "diffusion" || method == "pagerank") {
            valid <- slot(x, method)@valid
            if (is.na(valid) || !valid) {
                stop(paste0("Results from ", method, " are not ready yet."))
            } 
            
            # That may be a list or a unique graph
            if (!splitByConnectedComponent) {
                graph <- generateResultsGraph(
                    method = method, 
                    threshold = threshold, 
                    nlimit = nlimit,
                    splitByConnectedComponent = FALSE, 
                    LabelLengthAtPlot = LabelLengthAtPlot, 
                    object = x, 
                    data = data)
                
                if (!is.null(filename)) 
                    png(filename = filename, height = 1400, width = 1500)
                
                ans.return <- plotGraph(
                    graph = graph, 
                    input = getInput(x), 
                    layout = layout, 
                    ...)
                if (!is.null(filename)) dev.off()
                return(invisible(ans.return))
            } else {
                graph.list <- generateResultsGraph(
                    method = method, 
                    threshold = threshold, 
                    nlimit = nlimit,
                    splitByConnectedComponent = TRUE, 
                    LabelLengthAtPlot = LabelLengthAtPlot, 
                    object = x, 
                    data = data)
                
                if (is.null(filename) & askPlots) {
                    parOld <- graphics::par(mar = c(0, 0, 0, 0))
                    graphics::par(ask = TRUE)
                } 
                ans <- lapply(1:length(graph.list), function(graph.id) {
                    graph <- graph.list[[graph.id]]
                    
                    if (!is.null(filename)) {
                        png(
                            filename = paste0(
                                substr(filename, 1, nchar(filename) - 4), 
                                "_", 
                                graph.id, 
                                ".png"), 
                            height = 1400, 
                            width = 1500)
                    }
                    
                    
                    ans.layout <- plotGraph(
                        graph = graph, 
                        input = getInput(x), 
                        layout = layout, 
                        ...)
                    graphics::mtext(paste0(
                        "p-value from permutation by component size (", 
                        vcount(graph), 
                        " nodes): " , names(graph.list)[graph.id]))
                    
                    if (!is.null(filename)) dev.off()
                    ans.layout
                })
                if (is.null(filename) & askPlots) graphics::par(parOld)
                names(ans) <- names(graph.list)
                
                if (layout) return(invisible(ans))
                return(invisible())
            }
        }
        
        stop(
            "The 'method' argument must be ", 
            "'hypergeom', 'diffusion' or 'pagerank'.")
        return(invisible())
    })


