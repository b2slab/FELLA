#' \code{"summary"} is an S4 method to show a summary of the FELLA.USER object
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
    
    # output$hypergeom <- data.frame(dic[names(p.values)], signif(p.values, digits = 4))
    output$hypergeom <- data.frame(names(p.values), signif(p.values, digits = 4))
    names(output$hypergeom) <- c("Description", "p.value")
  }
  
  if (is.na(object@diffusion@valid)) output$diffusion <- "Not performed"
  else if (!object@diffusion@valid) output$diffusion <- "Failed"
  else {
    
    dif.select <- which(object@diffusion@pvalues < 0.05)
    
    out.pvalues <- signif(object@diffusion@pvalues[dif.select], digits = 3)
    smallest <- which(out.pvalues < 2e-16)
    out.pvalues <- format(out.pvalues)
    out.pvalues[smallest] <- "<2e-16"
    
    out.names <- names(out.pvalues)
    #out.description <- dic[out.names]
    #out.highlight <- out.names %in% object@diffusion@highlight
    
    out.order <- order(out.names)
    
    # output$diffusion <- data.frame(out.names, out.pvalues, out.description, out.highlight, 
    output$diffusion <- data.frame(out.names, out.pvalues, 
                                   row.names = NULL)[out.order, ]
    row.names(output$diffusion) <- NULL
    # names(output$diffusion) <- c("KEGG id", "p.value", "Description", "Guess?")
    names(output$diffusion) <- c("KEGG id", "p.value")
  }
  
  if (is.na(object@pagerank@valid)) output$pagerank <- "Not performed"
  else if (!object@pagerank@valid) output$pagerank <- "Failed"
  else {
    dif.select <- which(object@pagerank@pvalues < 0.05)
    
    out.pvalues <- signif(object@pagerank@pvalues[dif.select], digits = 3)
    smallest <- which(out.pvalues < 2e-16)
    out.pvalues <- format(out.pvalues)
    out.pvalues[smallest] <- "<2e-16"
    
    out.names <- names(out.pvalues)
    #out.description <- dic[out.names]
    #out.highlight <- out.names %in% object@diffusion@highlight
    
    # output$pagerank <- data.frame(out.names, out.pvalues, out.description, out.highlight, 
    output$pagerank <- data.frame(out.names, out.pvalues, 
                                   row.names = NULL)
    row.names(output$pagerank) <- NULL
    # names(output$pagerank) <- c("KEGG id", "p.value", "Description", "Guess?")
    names(output$pagerank) <- c("KEGG id", "p.value")

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
    cat(fill = T)
    cat("---------------------------------------------------", fill = T)
  }
  
  cat("Compounds in the input: ")
  if (length(getInput(object)) == 0) {
    cat("empty", fill = T)
  } else {
    cat(length(getInput(object)), fill = T)
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
    cat("ready.", fill = T)
    n.show <- min(15, length(getPvalues(object = object, type = "hypergeom")))
    cat("Top", n.show, "p-values:", fill = T)
    print(sort(getPvalues(object, "hypergeom"))[1:n.show])
  }
  
  breakline()
  
  cat("Heat diffusion: ")
  if (is.na(getValid(object, "diffusion"))) cat("not performed")
  else if (!getValid(object, "diffusion")) cat("error during execution")
  else {
    cat("ready.", fill = T)
    cat("Significant nodes (0.05): ", sum(getPvalues(object, "diffusion") < 0.05))
  }
  
  breakline()
  
  cat("PageRank: ")
  if (is.na(getValid(object, "pagerank"))) cat("not performed")
  else if (!getValid(object, "pagerank")) cat("error during execution")
  else {
    cat("ready.", fill = T)
    cat("Significant nodes (0.05): ", sum(getPvalues(object, "pagerank") < 0.05))
  }
  
  invisible()
})

#' \code{"plot"} is an S4 method to plot the results from a FELLA.USER object
#'
#' @param x A \code{\link{FELLA.USER}} object
#' @inheritParams .methodSingle
#' @param main Character; plot title
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
#' @exportMethod plot
setMethod("plot", 
          signature(x = "FELLA.USER", y = "missing"), 
          function(x = 1, 
                   method = "hypergeom", 
                   main = "Affected subgraph", 
                   threshold = 0.005, 
                   plimit = 15, 
                   nlimit = 250, 
                   layout = F, 
                   filename = NULL, 
                   splitByConnectedComponent = F, 
                   askPlots = T,  
                   thresholdConnectedComponent = 0.05, 
                   GO.CellularComponent = NULL, 
                   GONamesAsLabels = T, 
                   LabelLengthAtPlot = 22, 
                   data = NULL, 
                   ...) {

  checkArgs <- checkArguments(method = method, 
                              threshold = threshold, 
                              plimit = plimit, 
                              nlimit = nlimit, 
                              layout = layout, 
                              splitByConnectedComponent = splitByConnectedComponent, 
                              askPlots = askPlots, 
                              thresholdConnectedComponent = thresholdConnectedComponent, 
                              GO.CellularComponent = GO.CellularComponent, 
                              GONamesAsLabels = GONamesAsLabels, 
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
      

    graph.bipartite <- generateResultsGraph(method = method, 
                                          threshold = threshold, 
                                          plimit = plimit,
                                          object = x, 
                                          data = data)

    if (!is.null(filename)) png(filename = filename, height = 1000, width = 800)


    ans.return <- plotBipartite(graph = graph.bipartite, 
                                layout = layout, 
                                main = "Hypergeometric test results", 
                                ...)

    if (!is.null(filename)) dev.off()

    return(invisible(ans.return))
    
  } else if (method == "diffusion" || method == "pagerank") {
    valid <- slot(x, method)@valid
    if (is.na(valid) || !valid) {
      stop(paste0("Results from ", method, " are not ready yet."))
    } 

    # That may be a list or a unique graph
    if (!splitByConnectedComponent) {
      graph <- generateResultsGraph(method = method, 
                                    threshold = threshold, 
                                    nlimit = nlimit,
                                    splitByConnectedComponent = F, 
                                    GO.CellularComponent = GO.CellularComponent, 
                                    GONamesAsLabels = GONamesAsLabels, 
                                    LabelLengthAtPlot = LabelLengthAtPlot, 
                                    object = x, 
                                    data = data)
      
      if (!is.null(filename)) png(filename = filename, height = 1400, width = 1500)
      ans.return <- plotGraph(graph = graph, 
                    input = getInput(x), 
                    layout = layout, 
                    main = main, 
                    ...)
      if (!is.null(filename)) dev.off()
      return(invisible(ans.return))
    } else {
      graph.list <- generateResultsGraph(method = method, 
                                         threshold = threshold, 
                                         nlimit = nlimit,
                                         splitByConnectedComponent = T, 
                                         GO.CellularComponent = GO.CellularComponent, 
                                         GONamesAsLabels = GONamesAsLabels, 
                                         LabelLengthAtPlot = LabelLengthAtPlot, 
                                         object = x, 
                                         data = data)
      
      if (is.null(filename) & askPlots) {
        parOld <- par(mar = c(0, 0, 0, 0))
        par(ask = T)
      } 
      ans <- lapply(1:length(graph.list), function(graph.id) {
        graph <- graph.list[[graph.id]]
        
        if (!is.null(filename)) png(filename = paste0(substr(filename, 1, nchar(filename) - 4), 
                                                      "_", 
                                                      graph.id, 
                                                      ".png"), height = 1400, width = 1500)
        
        ans.layout <- plotGraph(graph = graph, 
                                input = getInput(x), 
                                layout = layout, 
                                main = main, 
                                ...)
        mtext(paste0("p-value from permutation by component size (", 
                      vcount(graph), 
                      " nodes): " , names(graph.list)[graph.id]))
        
        if (!is.null(filename)) dev.off()
        ans.layout
      })
      if (is.null(filename) & askPlots) par(parOld)
      names(ans) <- names(graph.list)
      
      if (layout) return(invisible(ans))
      return(invisible())
    }
  }
  
  stop("The 'method' argument must be 'hypergeom', 'diffusion' or 'pagerank'.")
  return(invisible())
})


  