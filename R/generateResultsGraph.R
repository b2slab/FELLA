#' Generate graph objects from the enrichment results
#' 
#' Function \code{generateResultsGraph} returns a graph object with class 
#' \code{\link[igraph]{igraph}} or a list of graphs of the same class. 
#' according to the specified threshold. A \code{\link[FELLA]{FELLA.USER}} 
#' object with a successful enrichment analysis must be supplied.
#' 
#' @inheritParams .params
#' @param ... ignored arguments
#'
#' @return An \code{\link{igraph}} object if 
#' \code{splitByConnectedComponent = F}; 
#' a list of \code{\link{igraph}} objects otherwise.
#' 
#' @examples 
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
    LabelLengthAtPlot = 22, 
    object = NULL, 
    data = NULL, 
    ...) {
    
    #   browser()
    if (!is.FELLA.DATA(data)) {
        stop("'data' is not a FELLA.DATA object")
    } else if (getStatus(data) != "loaded"){
        stop("'data' points to an empty FELLA.DATA object")
    }
    
    checkArgs <- checkArguments(
        method = method, 
        threshold = threshold, 
        plimit = plimit, 
        nlimit = nlimit, 
        splitByConnectedComponent = splitByConnectedComponent, 
        thresholdConnectedComponent = thresholdConnectedComponent, 
        LabelLengthAtPlot = LabelLengthAtPlot, 
        object = object, 
        data = data)
    
    if (!checkArgs$valid)
        stop("Bad argument when calling function 'generateResultsGraph'.")
    
    valid <- getValid(object, method)
    if (is.na(valid) || !valid) {
        warning(
            paste0("Method ", method, " has not been executed yet. "), 
            "Returning NULL...")
        return(invisible())
    } 
    
    #####################
    
    if (method == "hypergeom") {
        # HYPERGEOMETRIC TEST
        pscores <- getPscores(object, "hypergeom")
        g.data <- getGraph(data)
        
        # Select pathways and compounds
        n.paths <- sum(pscores < threshold)
        if (n.paths < 1) {
            message("Graph is empty. None of the pathways is significant.")
            return(NULL)
        } else {
            path.hypergeom <- names(head(
                sort(pscores[pscores < threshold]), 
                plimit))
        } 
        
        comp.hypergeom <- intersect(
            getInput(object), 
            rownames(getMatrix(data, "hypergeom")))

        # Build the bipartite graph
        incidence <- getMatrix(data, "hypergeom")[
            comp.hypergeom, path.hypergeom, drop = FALSE]
        
        graph.bipartite <- graph.incidence(incidence = incidence)
        #     browser()
        graph.bipartite <- induced.subgraph(
            graph.bipartite, 
            vids = (degree(graph.bipartite) > 0))
        
        # The com attribute for each node
        V(graph.bipartite)$com <- V(g.data)[V(graph.bipartite)$name]$com
        V(graph.bipartite)$label <- sapply(
            V(g.data)[V(graph.bipartite)$name]$NAME, 
            function(name.aux) {
                if (length(name.aux) == 0) return(NA)
                # Take first name
                name.first <- name.aux[[1]]
                name.def <- substr(name.first, 1, LabelLengthAtPlot)
                if (nchar(name.first) > LabelLengthAtPlot) 
                    name.def <- paste0(name.def, "...")
                
                name.def
            }
        )
            
        return(graph.bipartite)
    } else { 
        # DIFFUSION AND PAGERANK
        pscores <- getPscores(object, method)
        
        n.nodes <- sum(pscores < threshold)
        if (n.nodes < 1) {
            message("Graph is empty.")
            return(NULL)
        } else if (n.nodes > nlimit) {
            message(paste0(
                n.nodes, 
                " nodes below the threshold have been limited to ", 
                nlimit, 
                " nodes."))
            nodes <- names(pscores)[sort(head(order(pscores), nlimit))]
        } else {
            nodes <- names(pscores)[pscores < threshold]
        }
        
        graph <- induced.subgraph(graph = getGraph(data), vids = nodes)
        
        if (method == "diffusion") graph <- as.undirected(graph)
        
        # Define labels for the plot
        vertex.labels <- character(vcount(graph))
        vertex.labels <- sapply(
            V(graph)$NAME, 
            function(name.aux) {
                if (length(name.aux) == 0) return(NA)
                # Take first name
                name.first <- name.aux[[1]]
                name.def <- substr(name.first, 1, LabelLengthAtPlot)
                if (nchar(name.first) > LabelLengthAtPlot) 
                    name.def <- paste0(name.def, "...")
                
                name.def
            }
        )
        
        V(graph)$label <- vertex.labels
        
        if (!splitByConnectedComponent) return(graph)
        
        n.nodes.graph <- vcount(graph)
        
        # Connected components
        graph.clust <- clusters(graph)
        
        # Size cannot exceed 250 to 
        # calculate p-values... we will approximate
        if (n.nodes.graph > 250) {
            warning(
                "The number of nodes of the whole solution, which is ", 
                n.nodes.graph, 
                ", exceeds 250. p-values will be computed ", 
                "using 250 nodes instead.")
            
            n.nodes.graph <- 250
        }
        
        tab.significant <- getPvaluesSize(data)[, n.nodes.graph]
        csize.significant <- which(
            tab.significant < thresholdConnectedComponent)[1]
        if (length(csize.significant) == 0) {
            warning(
                "None of the connected components are below the ", 
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
            names(graph.listed)[i] <- getPvaluesSize(data)[
                vcount(graph.temp), n.nodes.graph]
        }
        
        return(graph.listed)
    }
    
}