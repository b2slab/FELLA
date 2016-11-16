#' Generate graph objects from the enrichment results
#' 
#' Function \code{generateResultsGraph} returns a graph object with class 
#' \code{\link[igraph]{igraph}} or a list of graphs of the same class. 
#' according to the specified threshold. A \code{\link[FELLA]{FELLA.USER}} object 
#' with a successful enrichment analysis must be supplied.
#' 
#'
#' @inheritParams .methodSingle
#' @inheritParams .threshold
#' @inheritParams .plimit
#' @inheritParams .nlimit
#' @inheritParams .splitByConnectedComponent
#' @inheritParams .thresholdConnectedComponent
#' @inheritParams .GO.CellularComponent
#' @inheritParams .GONamesAsLabels
#' @inheritParams .LabelLengthAtPlot
#' @inheritParams .object
#' @inheritParams .data
#'
#' @return An \code{\link{igraph}} object if \code{splitByConnectedComponent = F}; 
#' a list of \code{\link{igraph}} objects otherwise.
#' 
#' @import igraph
#' @export
generateResultsGraph <- function(
  method = "diffusion", 
  threshold = 0.05, 
  plimit = 15, 
  nlimit = 250, 
  splitByConnectedComponent = FALSE, 
  thresholdConnectedComponent = 0.05, 
  GO.CellularComponent = NULL,
  GONamesAsLabels = TRUE, 
  LabelLengthAtPlot = 22, 
  object = NULL, 
  data = NULL) {
  
#   browser()
  if (!is.FELLA.DATA(data)) {
    stop("'data' is not a FELLA.DATA object")
  } else if (data@keggdata@status != "loaded"){
    stop("'data' points to an empty FELLA.DATA object")
  }
  
  checkArgs <- checkArguments(
    method = method, 
    threshold = threshold, 
    plimit = plimit, 
    nlimit = nlimit, 
    splitByConnectedComponent = splitByConnectedComponent, 
    thresholdConnectedComponent = thresholdConnectedComponent, 
    GO.CellularComponent = GO.CellularComponent, 
    GONamesAsLabels = GONamesAsLabels, 
    LabelLengthAtPlot = LabelLengthAtPlot, 
    object = object, 
    data = data)
 
  if (!checkArgs$valid)
    stop("Bad argument when calling function 'generateResultsGraph'.")
  
  if (is.na(getValid(object, method)) || !getValid(object, method)) {
    warning(paste0("Mehod ", method, " has not been executed yet. "), 
            "Returning NULL...")
    return(invisible())
  } 
  
  ##############################################################################
  
  if (method == "hypergeom") {
    # HYPERGEOMETRIC TEST
    
    pvalues <- getPvalues(object, "hypergeom")
    
    # Select pathways and compounds
    n.paths <- sum(pvalues < threshold)
    if (n.paths < 1) {
      message("Graph is empty. None of the pathways is significant.")
      return(NULL)
    } else if (n.paths > plimit) {
      nodes <- sort(order(pvalues)[1:plimit])
      path.hypergeom <- names(pvalues)[nodes]
    } else {
      path.hypergeom <- names(pvalues)[pvalues < threshold]
    }
    
    comp.hypergeom <- intersect(
      getInput(object), 
      rownames(getMatrix(data, "hypergeom")))
#     browser()
    # Build the bipartite graph
    
    if (length(path.hypergeom) == 1) {
      incidence <- as.matrix(
        getMatrix(data, "hypergeom")[comp.hypergeom, path.hypergeom], 
        ncol = 1)
      colnames(incidence) <- path.hypergeom
    } else {
      incidence <- getMatrix(data, "hypergeom")[comp.hypergeom, 
                                                path.hypergeom]
    }
      
    graph.bipartite <- graph.incidence(
      incidence = incidence)
#     browser()
    graph.bipartite <- induced.subgraph(
      graph.bipartite, 
      vids = (degree(graph.bipartite) > 0))
    
    # The com attribute for each node
    V(graph.bipartite)$com <- ifelse(
      grepl("C\\d{5}", V(graph.bipartite)$name), 
      5, 
      1
    )

    return(graph.bipartite)
  } else { 
    # DIFFUSION AND PAGERANK
    pvalues <- getPvalues(object, method)
    
    n.nodes <- sum(pvalues < threshold)
    if (n.nodes < 1) {
      message("Graph is empty.")
      return(NULL)
    } else if (n.nodes > nlimit) {
      message(paste0(
        n.nodes, 
        " nodes below the threshold have been limited to ", 
        nlimit, 
        " nodes."))
      nodes <- names(pvalues)[sort(head(order(pvalues), nlimit))]
    } else {
      nodes <- names(pvalues)[pvalues < threshold]
    }
    
    graph <- induced.subgraph(graph = getGraph(data), vids = nodes)

    if (method == "diffusion") graph <- as.undirected(graph)
    
    # Build the cellular component data if specified
    if (!is.null(GO.CellularComponent) & 
        !identical(GO.CellularComponent, "")) {
      message("Adding cellular component similarity...")
      
      graph <- addCellularComponentToGraph(
        graph = graph, 
        GO.CellularComponent = GO.CellularComponent, 
        GONamesAsLabels = GONamesAsLabels)
      
      message("Done.")
    } else {
      graph <- set.vertex.attribute(
        graph = graph, 
        name = "GO.CC", 
        value = rep(-1, vcount(graph)))
    }
    
    # Define labels for the plot
    vertex.labels <- character(vcount(graph))
    for (i in 1:vcount(graph)) {
      name.aux <- (V(graph)[[i]]$NAME)[1]
      name.def <- substr(name.aux, 1, LabelLengthAtPlot)
      if (nchar(name.aux) > LabelLengthAtPlot) 
        name.def <- paste0(name.def, "...")

      if (V(graph)[i]$GO.CC != -1) 
        name.def <- paste0(name.def, "[", names(V(graph)[[i]]$GO.CC), "]")
      
      vertex.labels[i] <- name.def
    }
    
    graph <- set.vertex.attribute(graph = graph, 
                                  name = "LABEL", 
                                  value = vertex.labels)
    
    if (!splitByConnectedComponent) return(graph)
    
    n.nodes.graph <- vcount(graph)
    
    # Connected components
    graph.clust <- clusters(graph)
    
    # Size cannot exceed 250 to calculate p-values... we will approximate
    if (n.nodes.graph > 250) {
      warning(paste0(
        "The number of nodes of the whole solution, which is ", 
        n.nodes.graph, 
        ", exceeds 250. p-values will be computed ", 
        "using 250 nodes instead."))
      
      n.nodes.graph <- 250
    }
      
    csize.significant <- which(
      getPvaluesSize(data)[, n.nodes.graph] < thresholdConnectedComponent)[1]
    if (length(csize.significant) == 0) {
      warning("None of the connected components are below the ", 
              "'thresholdConnectedComponent'. Returning NULL...")
      return(invisible())
    } 
    
    # Select the biggest ones only
    clust.select <- which(graph.clust$csize >= csize.significant)
    
    graph.listed <- list()
    for (i in 1:length(clust.select)) {
      clust <- clust.select[i]
      nodes.graph <- which(graph.clust$membership == clust)
      graph.temp <- induced.subgraph(graph = graph, vids = nodes.graph)
      graph.listed[[i]] <- graph.temp
      names(graph.listed)[i] <- getPvaluesSize(data)[vcount(graph.temp), 
                                                     n.nodes.graph]
    }
    
    return(graph.listed)
  }
  
}