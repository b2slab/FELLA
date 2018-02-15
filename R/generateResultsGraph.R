#' @include generateEnzymesTable.R
#' 
#' @description 
#' Function \code{generateResultsGraph} 
#' gives a sub-network, plottable through 
#' \code{plotGraph}, witht the nodes with 
#' the lowest \code{p.score} from an enrichment analysis. 
#' Function \code{addGOToGraph} can be applied to such 
#' sub-networks to overlay GO labels and 
#' similarity to a user-defined GO term. 
#' 
#' @details
#' Function \code{generateResultsGraph} returns an 
#' \pkg{igraph} 
#' object with a relevant sub-network 
#' for manual examination. 
#' A \code{\link{FELLA.USER}} 
#' object with a successful enrichment analysis and the corresponding 
#' \code{\link{FELLA.DATA}} must be supplied. 
#' Graph nodes are prioritised by \code{p.score} and selected through 
#' the most stringent between (1) p.score \code{threshold} and 
#' (2) maximum number of nodes \code{nlimit}. 
#' 
#' There is an additional filtering feature for tiny connected components, 
#' controllable through \code{thresholdConnectedComponent} 
#' (smaller is stricter). 
#' The user can choose to turn off this filter by setting 
#' \code{thresholdConnectedComponent = 1}.  
#' The idea is to discard connected components so small 
#' that are likely to arise from random selection of nodes. 
#' Let \code{k} be the order of the current sub-network.
#' A connected component of order \code{r} will
#' be kept only if the probability that a 
#' random subgraph from the whole KEGG knowledge model 
#' of order \code{k} contains a
#' connected component of order at least \code{r} 
#' is smaller than \code{thresholdConnectedComponent}. 
#' Such probabilities are estimated during 
#' \code{\link[=data-funs]{buildDataFromGraph}}; the amount of random 
#' trials can be controlled by its \code{niter} argument. 
#' 
#' @inheritParams .params
#'
#' @return \code{generateResultsGraph} returns 
#' an \pkg{igraph}
#' object: a sub-network from the whole 
#' KEGG knowledge model under the specified thresholds 
#' (\code{threshold} and \code{thresholdConnectedComponent})
#' 
#' @rdname export-funs
#' 
#' @import igraph
#' @export
generateResultsGraph <- function(
    method = "diffusion", 
    threshold = 0.05, 
    plimit = 15, 
    nlimit = 250, 
    thresholdConnectedComponent = 0.05, 
    LabelLengthAtPlot = 22, 
    object = NULL, 
    data = NULL, 
    ...) {
    
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
        thresholdConnectedComponent = thresholdConnectedComponent, 
        LabelLengthAtPlot = LabelLengthAtPlot, 
        object = object, 
        data = data)
    
    if (!checkArgs$valid)
        stop("Bad argument when calling function 'generateResultsGraph'.")
    
    valid <- getValid(object, method)
    if (is.na(valid) || !valid) {
        warning(
            "Method ", method, " has not been executed yet. ", 
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
        graph.bipartite <- induced.subgraph(
            graph.bipartite, 
            vids = (degree(graph.bipartite) > 0))
        
        # The com attribute for each node
        V(graph.bipartite)$com <- V(g.data)[V(graph.bipartite)$name]$com
        V(graph.bipartite)$label <- vapply(
            V(g.data)[V(graph.bipartite)$name]$NAME, 
            function(name.aux) {
                if (length(name.aux) == 0) return(NA_character_)
                # Take first name
                name.first <- name.aux[[1]]
                name.def <- substr(name.first, 1, LabelLengthAtPlot)
                if (nchar(name.first) > LabelLengthAtPlot) 
                    name.def <- paste0(name.def, "...")
                
                name.def
            }, 
            FUN.VALUE = character(1)
        )
            
        return(graph.bipartite)
    } else { 
        # DIFFUSION AND PAGERANK
        pscores <- getPscores(object, method)
        input <- getInput(object)
        
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
        vertex.labels <- vapply(
            V(graph)$NAME, 
            function(name.aux) {
                if (length(name.aux) == 0) return(NA_character_)
                # Take first name
                name.first <- name.aux[[1]]
                name.def <- substr(name.first, 1, LabelLengthAtPlot)
                if (nchar(name.first) > LabelLengthAtPlot) 
                    name.def <- paste0(name.def, "...")
                
                name.def
            }, 
            FUN.VALUE = character(1)
        )
        
        V(graph)$label <- vertex.labels
        V(graph)$input <- V(graph)$name %in% input
        
        # Filter out small CCs
        n.nodes.graph <- vcount(graph)
        
        # Connected components
        graph.clust <- clusters(graph)
        
        # Size cannot exceed 250 to 
        # calculate p-values... we will approximate
        if (n.nodes.graph > 250) {
            warning(
                "The number of nodes of the whole solution, which is ", 
                n.nodes.graph, 
                ", exceeds 250. Small connected components ", 
                "will be filtered using 250 nodes instead.")
            
            n.nodes.graph <- 250
        }
        
        # p-values of largest cc size (they are decreasing in 
        # CC size)
        tab.significant <- getPvaluesSize(data)[, n.nodes.graph]
        # Minimum size to consider "significant"
        csize.significant <- which(
            tab.significant <= thresholdConnectedComponent)[1]
        if (length(csize.significant) == 0) {
            warning(
                "None of the connected components are below the ", 
                "'thresholdConnectedComponent'. Returning empty graph...")
            return(igraph::induced_subgraph(graph, NULL))
        } 
        
        # Graph is not empty 
        # Select the biggest CCs only
        clust.select <- which(graph.clust$csize >= csize.significant)
        
        graph.ans <- igraph::induced_subgraph(
            graph, 
            vids = which(graph.clust$membership %in% clust.select)
        )
        
        return(graph.ans)
    }
    
}