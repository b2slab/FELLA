#' \code{"summary"} is an S4 method to show a summary 
#' of the FELLA.USER object
#'
# @param object A \code{\link{FELLA.USER}} object
# Already in the show documentation
#' 
#' @return \code{summary} returns a list with the summary data
#' 
#' @rdname FELLA.USER
#' @importFrom stats setNames
#' @exportMethod summary
setMethod("summary", signature = "FELLA.USER", function(object) {
    breakline <- "\n---------------------------------------------------\n"
    
    lapply(
        stats::setNames(listMethods(), listMethods()), 
        function(method) {
            ans <- list()
            
            valid <- getValid(object, method)
            if (is.na(valid)) return("Not performed")
            if (!valid) return("Failed")
            
            if (method == "hypergeom") {
                pval <- getPscores(object, "hypergeom")
                p.values <- head(sort(pval), 30)
                
                data.frame(
                    "Description" = names(p.values), 
                    "p.value" = signif(p.values, digits = 4))
            } else {
                psco <- sort(getPscores(object, method))
                out.pscores <- signif(psco[psco < .05], digits = 3)
                # Set a threshold

                out.pscores <- format(out.pscores)
                out.pscores[out.pscores < 1e-6] <- "<1e-6"
                
                out.names <- names(out.pscores)
                out.order <- order(out.names)
                
                df <- data.frame(
                    "KEGG id" = names(out.pscores), 
                    "p.score" = out.pscores)
                row.names(df) <- NULL
                
                df
            }
        }
    )
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
    if (vcount(getGraph(object)) == 0) {
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
    if (prod(dim(getMatrix(object, "hypergeom"))) == 1) {
        cat("- Matrix not loaded.")
    } else {
        cat("- Matrix is ready.")
    }
    
    cat(breakline)
    
    cat("Heat diffusion:\n")
    if (prod(dim(getMatrix(object, "diffusion"))) == 1) {
        cat("- Matrix not loaded.\n")
    } else {
        cat("- Matrix is ready.\n")
    }
    if (length(getSums(object, "diffusion", squared = FALSE)) == 0 || 
        length(getSums(object, "diffusion", squared = TRUE)) == 0) {
        cat("- RowSums not loaded.")
    } else {
        cat("- RowSums are ready.")
    }
    
    cat(breakline)
    
    cat("PageRank:\n")
    if (prod(dim(getMatrix(object, "pagerank"))) == 1) {
        cat("- Matrix not loaded.\n")
    } else {
        cat("- Matrix is ready.\n")
    }
    if (length(getSums(object, "pagerank", squared = FALSE)) == 0 || 
        length(getSums(object, "pagerank", squared = TRUE)) == 0) {
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
            getPscores(object = object, method = "hypergeom")))
        cat("Top", n.show, "p-values:", fill = TRUE)
        print(sort(getPscores(object, "hypergeom"))[1:n.show])
    }
    
    breakline()
    
    cat("Heat diffusion: ")
    if (is.na(getValid(object, "diffusion"))) cat("not performed")
    else if (!getValid(object, "diffusion")) cat("error during execution")
    else {
        cat("ready.", fill = TRUE)
        cat("P-scores under 0.05: ", 
            sum(getPscores(object, "diffusion") < 0.05))
    }
    
    breakline()
    
    cat("PageRank: ")
    if (is.na(getValid(object, "pagerank"))) cat("not performed")
    else if (!getValid(object, "pagerank")) cat("error during execution")
    else {
        cat("ready.", fill = TRUE)
        cat("P-scores under 0.05: ", 
            sum(getPscores(object, "pagerank") < 0.05))
    }
    
    invisible()
})

#' \code{"plot"} is an S4 method to plot the results 
#' from a FELLA.USER object
#'
#' @param x A \code{\link{FELLA.USER}} object
#' @inheritParams .params
#' @param ... Additional arguments passed to plotting functions
#' 
#' @return \code{plot} returns a layout if \code{layout = T}, 
#' otherwise \code{invisible()}
#' 
#' @rdname FELLA.USER
#' @exportMethod plot
setMethod(
    "plot", 
    signature(x = "FELLA.USER", y = "missing"), 
    function(
        x = 1, 
        method = "hypergeom", 
        threshold = 0.05, 
        plimit = 15, 
        nlimit = 250, 
        layout = FALSE, 
        splitByConnectedComponent = FALSE, 
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
            thresholdConnectedComponent = thresholdConnectedComponent, 
            LabelLengthAtPlot = LabelLengthAtPlot, 
            object = x, 
            data = data)
        if (!checkArgs$valid)
            stop("Bad argument when calling function 'plot' in FELLA.")
        
        if (getStatus(data) != "loaded"){
            stop("'data' points to an empty FELLA.DATA object")
        }
        
        valid <- getValid(x, method)
        if (is.na(valid) || !valid) {
            stop(paste0("Results from ", method, " are not ready yet."))
        } 
        
        if (method == "hypergeom") {
            graph.bipartite <- generateResultsGraph(
                method = method, 
                threshold = threshold, 
                plimit = plimit,
                object = x, 
                data = data)
            
            ans.return <- plotBipartite(
                graph = graph.bipartite, 
                layout = layout, 
                ...)

            return(invisible(ans.return))
            
        } else {
            # We have checked that "method" is in hypergeom, diffusion
            # and pagerank. Therefore, it's one of the last two
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
                
                ans.return <- plotGraph(
                    graph = graph, 
                    input = getInput(x), 
                    layout = layout, 
                    ...)

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
                
                ans <- lapply(1:length(graph.list), function(graph.id) {
                    graph <- graph.list[[graph.id]]
                    
                    ans.layout <- plotGraph(
                        graph = graph, 
                        input = getInput(x), 
                        layout = layout, 
                        ...)

                    ans.layout
                })

                names(ans) <- names(graph.list)
                
                if (layout) return(invisible(ans))
                return(invisible())
            }
        }
        
        return(invisible())
    })
