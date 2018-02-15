#' \code{"show"} is an S4 method to show a FELLA.DATA object
#'
#' @param object A \code{\link[FELLA]{FELLA.DATA}} object
#' 
#' @return \code{show} returns \code{invisible()}
#' 
#' @rdname FELLA.DATA
#' @import igraph
#' @exportMethod show
setMethod("show", signature = "FELLA.DATA", function(object) {
    breakline <- "\n-----------------------------\n"
    
    cat("General data:\n")
    g <- getGraph(object)
    if (vcount(g) == 0) {
        cat("- KEGG graph not loaded.\n")
    } else {
        cat("- KEGG graph:\n")
        # To keep the original order, subset it
        tab.com <- table(listCategories()[V(g)$com])[listCategories()]
        tab.show <- paste0("    + ", names(tab.com), " [", tab.com, "]")
        cat("  * Nodes: ", vcount(g), "\n")
        cat("  * Edges: ", ecount(g), "\n")
        cat("  * Density: ", graph.density(g), "\n")
        cat("  * Categories:\n")
        cat(tab.show, sep = "\n")
        cat("  * Size: ", format(utils::object.size(g), "auto"), "\n")
    }
    if (length(object@keggdata@id2name) == 0) {
        cat("- KEGG names not loaded.")
    } else {
        cat("- KEGG names are ready.")
    }
    
    cat(breakline)
    
    cat("Hypergeometric test:\n")
    mat <- getMatrix(object, "hypergeom")
    if (prod(dim(mat)) == 1) {
        cat("- Matrix not loaded.")
    } else {
        cat("- Matrix is ready\n")
        cat("  * Dim: ", nrow(mat), "x", ncol(mat), "\n")
        cat("  * Size: ", format(utils::object.size(mat), "auto"))
    }
    
    cat(breakline)
    
    cat("Heat diffusion:\n")
    mat <- getMatrix(object, "diffusion")
    if (prod(dim(mat)) == 1) {
        cat("- Matrix not loaded.\n")
    } else {
        cat("- Matrix is ready\n")
        cat("  * Dim: ", nrow(mat), "x", ncol(mat), "\n")
        cat("  * Size: ", format(utils::object.size(mat), "auto"), "\n")
    }
    if (length(getSums(object, "diffusion", squared = FALSE)) == 0 || 
        length(getSums(object, "diffusion", squared = TRUE)) == 0) {
        cat("- RowSums not loaded.")
    } else {
        cat("- RowSums are ready.")
    }
    
    cat(breakline)
    
    cat("PageRank:\n")
    mat <- getMatrix(object, "pagerank")
    if (prod(dim(mat)) == 1) {
        cat("- Matrix not loaded.\n")
    } else {
        cat("- Matrix is ready\n")
        cat("  * Dim: ", nrow(mat), "x", ncol(mat), "\n")
        cat("  * Size: ", format(utils::object.size(mat), "auto"), "\n")
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
#' Assigning the value of show to a variable will provide
#' small data frames with the best scoring pathways (\code{hypergeom})
#' and the best nodes in the KEGG network (\code{diffusion} and 
#' \code{pagerank})
#'
#' @param object A \code{\link{FELLA.USER}} object
#' 
#' @return \code{show} invisibly returns a list of data frames with the 
#' best hits for each applied method
#' 
#' @rdname FELLA.USER
#' @exportMethod show
setMethod("show", signature = "FELLA.USER", function(object) {
    breakline <- function() {
        cat(fill = TRUE)
        cat("-----------------------------", fill = TRUE)
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
        print(head(sort(getPscores(object, "hypergeom")), n.show))
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
    
    # Return data frames
    ans <- lapply(
        stats::setNames(listMethods(), listMethods()), 
        function(method) {
            ans <- list()
            
            valid <- getValid(object, method)
            if (is.na(valid)) return("Not performed")
            if (!valid) return("Failed")
            
            if (method == "hypergeom") {
                # Data frame with top pathway hits
                pval <- getPscores(object, "hypergeom")
                p.values <- head(sort(pval), 30)
                
                data.frame(
                    "Description" = names(p.values), 
                    "p.value" = signif(p.values, digits = 4), 
                    row.names = NULL)
            } else {
                # Data frame with best diffusion scores
                out.pscores <- signif(
                    head(
                        sort(getPscores(object, method)), 
                        50), 
                    digits = 4)

                # Set an order (we do not use the kegg data 
                # as it only uses the FELLA.USER object...)
                out.names <- names(out.pscores)
                out.order <- order(out.names)
                
                out.pscores <- format(out.pscores[out.order])
                out.pscores[out.pscores < 1e-6] <- "<1e-6"
                
                df <- data.frame(
                    "KEGG id" = names(out.pscores), 
                    "p.score" = out.pscores, 
                    row.names = NULL)
                
                df
            }
        }
    )
    invisible(ans)
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
        x = new("FELLA.USER"), 
        method = "hypergeom", 
        threshold = 0.05, 
        plimit = 15, 
        nlimit = 250, 
        layout = FALSE, 
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
            stop("Results from ", method, " are not ready yet.")
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
            # It has to be one graph
            graph <- generateResultsGraph(
                method = method, 
                threshold = threshold, 
                nlimit = nlimit,
                LabelLengthAtPlot = LabelLengthAtPlot, 
                object = x, 
                data = data)
            
            ans.return <- plotGraph(
                graph = graph, 
                layout = layout, 
                ...)
            
            return(invisible(ans.return))
        }
        return(invisible())
    })
