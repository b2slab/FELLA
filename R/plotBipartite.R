#' Internal function to plot a bipartite graph
#' 
#' This function plots a bipartite graph, tailored for the hypergeometric 
#' over representation analysis. As the nodes can only be compounds and 
#' pathways, the layout is simple and bipartite.
#'
#' @inheritParams .params
#' @param graph Graph result that must come from the 
#' hypergeometric test analysis
#' @param ... Additional parameters passed to 
#' \code{\link[igraph]{plot.igraph}}
#' 
#' @return If \code{layout = F} then the value 
#' returned is \code{invisible()}. 
#' Otherwise, the layout is returned, also in an invisible fashion.
#' 
#' @examples 
#' ## This function is internal
#' data(FELLA.sample)
#' data(input.sample)
#' ## Enrich input
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample, 
#' method = "hypergeom")
#' ## Generate the bipartite graph (only in the hypergeometric test)
#' g <- generateResultsGraph(
#' method = "hypergeom", 
#' threshold = 1, 
#' object = obj, 
#' data = FELLA.sample)
#' ## Plot it
#' FELLA:::plotBipartite(g)
#' 
#' @import igraph
#' 
#' @keywords internal
plotBipartite <- function(
    graph = NULL, 
    layout = FALSE, 
    ...) {
    
    graph.com <- as.character(V(graph)$com)
    
    # Generate a pretty layout
    graph.asp <- 4/3
    graph.layout <- layout.bipartite(
        graph, 
        types = (V(graph)$com == 5), 
        hgap = .5, 
        vgap = 2.5)
    graph.layout <- layout.norm(
        graph.layout[, c(2, 1)], 
        xmin = -1, 
        xmax = 1, 
        ymin = -1, 
        ymax = 1)
    
    # Vertex shape & color
    mp.shape <- c("1" = "circle", "5" = "square")
    mp.color <- c("1" = "red3", "5" = "palegreen4")
    mp.deg <- c("1" = 0, "5" = -pi)
    
    vertex.shape <- mp.shape[graph.com]
    vertex.color <- mp.color[graph.com]
    
    # Labels and sizes
    vertex.size <- 3
    vertex.label.dist <- 0.7
    vertex.label.degree <- mp.deg[graph.com]

    # Actual plot
    plot.igraph(
        x = graph, 
        layout = graph.layout, 
        vertex.size = vertex.size, 
        vertex.label = V(graph)$name, 
        vertex.label.dist = vertex.label.dist, 
        vertex.label.color = vertex.color, 
        vertex.label.degree = vertex.label.degree, 
        vertex.color = vertex.color, 
        vertex.shape = vertex.shape,
        edge.color = "#000000AA", 
        edge.arrow.size = 0.25,
        asp = graph.asp, 
        ...)
    
    if (!layout) {
        return(invisible(NULL))
    } else  {
        x <- graph.layout[, 1]
        y <- graph.layout[, 2]
        
        mp.prefix <- c("1" = "", "5" = "cpd:")
        out.id <- paste0(
            mp.prefix[as.character(V(graph)[vertex]$com)], 
            V(graph)[vertex]$name
        )
        
        out.complete <- data.frame(x, y, out.id, stringsAsFactors = FALSE)
        
        return(invisible(out.complete))
    }
}