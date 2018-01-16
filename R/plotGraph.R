#' Add triangular shape to igraph plot
#' 
#' This function enables the usage of triangles as shape in 
#' the function \code{\link[igraph]{plot.igraph}}. 
#' 
#' @param coords,v,params clipping arguments, see 
#' \code{\link[igraph]{shapes}}
#' 
#' @return Plot symbols
#' 
#' @examples 
#' ## This function is internal
#' library(igraph)
#' 
#' add.vertex.shape(
#' "triangle", clip = shapes("circle")$clip,
#' plot = FELLA:::mytriangle)
#' 
#' g <- barabasi.game(10)
#' plot(
#' g, vertex.shape = "triangle", 
#' vertex.color = rainbow(vcount(g)),
#' vertex.size = seq(10, 20, length = vcount(g)))
#' 
#' @importFrom graphics symbols
#' 
#' @keywords internal
mytriangle <- function(coords, v=NULL, params) {
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
        vertex.color <- vertex.color[v]
    }
    vertex.size <- 1/200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
        vertex.size <- vertex.size[v]
    }
    
    graphics::symbols(
        x = coords[, 1], y = coords[, 2], bg = vertex.color,
        stars = cbind(vertex.size, vertex.size, vertex.size),
        add = TRUE, inches = FALSE)
}


#' @details
#' Function \code{plotGraph} 
#' plots a solution graph from the diffusion and pagerank analysis. 
#' For plotting hypergeom results, please use \code{plot} instead. 
#' Specific colors and shapes for each KEGG category are used: 
#' pathways are maroon, modules are violet, enzymes are orange, 
#' reactions are blue and compounds are green. 
#' If the graph contains the similarity to a GO term, enzymes will 
#' be displayed as triangles whose color depicts the strength of 
#' such measure (yellow: weak, purple: strong). 
#' At the moment, \code{plotGraph} allows plotting 
#' throug the static \code{\link[igraph]{plot.igraph}} and the 
#' interactive \code{\link[igraph]{tkplot}}. 
#'
#' @inheritParams .params
#' @param graph.layout Two-column numeric matrix, 
#' if this argument is not null 
#' then it is used as graph layout
#' @param plotLegend Logical, should the legend be plotted as well?
#' @param plot.fun Character, can be either 
#' \code{plot.igraph} or \code{tkplot}
#' 
#' @return \code{plotGraph} returns 
#' \code{invisible()} if \code{layout = F} and 
#' the plotting layout as a data.frame otherwise.
#' 
#' @rdname export-funs
#' 
#' @import igraph
#' @export
plotGraph <- function(
    graph = NULL, 
    layout = FALSE,
    graph.layout = NULL, 
    plotLegend = TRUE, 
    plot.fun = "plot.igraph", 
    NamesAsLabels = TRUE, 
    ...) {
    
    if (vcount(graph) == 0) {
        warning("The graph is empty and won't be plotted.")
        return(invisible())
    }
    
    # If there is GO cellular component data available, plot it..!
    if ("GO.simil" %in% list.vertex.attributes(graph)) {
        GO.simil <- V(graph)$GO.simil
        GO.annot <- TRUE
    } else {
        GO.annot <- FALSE
    }
    
    # triangle vertex shape
    add.vertex.shape(
        "triangle", clip = vertex.shapes("circle")$clip, plot = mytriangle)
    #########################################################
    
    # Nodes in the input are in V(graph)$input
    graph.input <- V(graph)$input
    graph.com <- as.character(V(graph)$com)
    
    # Vertex shape
    vertex.shape <- rep("circle", vcount(graph))
    vertex.shape[graph.input] <- "square"
    
    vertex.number <- vcount(graph)
    
    graph.asp <- 1
    if (is.null(graph.layout)) graph.layout <- layout.auto(graph)
    
    graph.layout <- layout.norm(
        graph.layout, 
        xmin = -1, 
        xmax = 1, 
        ymin = -1, 
        ymax = 1)
    
    ## Define vertex colour
    mapSolidColor <- c(
        "1" = "#CD0000",
        "2" = "#CD96CD",
        "3" = "#FFA200",
        "4" = "#8DB6CD",
        "5" = "#548B54"
    )
    vertex.color <- vapply(V(graph), function(y) {
        solidColor <- mapSolidColor[graph.com[y]]
        if (!GO.annot) return(solidColor)
        
        GO.y <- GO.simil[y]
        if (!is.na(GO.y)) {
            if (GO.y < 0.5) solidColor <- "#FFD500"
            else if (GO.y < 0.7) solidColor <- "#FF5500"
            else if (GO.y < 0.9) solidColor <- "#FF0000"
            else solidColor <- "#B300FF"
        }
        
        solidColor
    }, FUN.VALUE = character(1))
    
    # Vertex frame color
    vertex.frame.color <- rep("black", vcount(graph))
    if (GO.annot) {
        vertex.frame.color[!is.na(GO.simil)] <- "#CD0000"
        vertex.shape[!is.na(GO.simil)] <- "triangle"
    }
    
    # Vertex size
    mapSize <- c(
        "1" = 7,
        "2" = 5.5,
        "3" = 4.25,
        "4" = 3.5,
        "5" = 3
    )
    vertex.size <- mapSize[graph.com]
    vertex.size[graph.input] <- 4
    vertex.size <- vertex.size*(300/vcount(graph))^(1/3)
    
    # Labels
    vertex.label.dist <- 0.1*(300/vcount(graph))^(1/3)
    vertex.label.degree <- -pi/2
    
    if (NamesAsLabels) {
        vertex.label <- V(graph)$label
    } else {
        vertex.label <- V(graph)$name
    }
    
    options <- as.list(substitute(list(...)))[-1L]
    args.shared <- list(
        layout = graph.layout, 
        vertex.size = vertex.size, 
        vertex.label = vertex.label, 
        vertex.label.dist = vertex.label.dist, 
        vertex.label.color = vertex.color, 
        vertex.label.degree = vertex.label.degree, 
        vertex.frame.color = vertex.frame.color, 
        vertex.color = vertex.color, 
        vertex.shape = vertex.shape,
        edge.color = "#000000AA", 
        edge.arrow.size = 0.25,
        asp = graph.asp)
    
    if (plot.fun == "plot.igraph") {
        do.call(
            plot.fun, 
            c(list(x = graph), args.shared, options)
        )
    } 
    if (plot.fun == "tkplot") {
        do.call(
            plot.fun, 
            c(list(graph = graph), args.shared, options)
        )
    }
    
    # Plot the legend
    if (plotLegend) plotLegend(GO.annot = GO.annot, cex = 0.5)
    
    mapPrefix <- c(
        "1" = "",
        "2" = "md:",
        "3" = "ec:",
        "4" = "rn:",
        "5" = "cpd:"
    )
    if (!layout) {
        return(invisible(NULL))
    } else  {
        out.complete <- data.frame(
            x = graph.layout[, 1], 
            y = graph.layout[, 2], 
            out.id = paste0(mapPrefix[V(graph)$com], V(graph)$name), 
            out.name = V(graph)$label, 
            stringsAsFactors = FALSE)

        return(invisible(out.complete))
    }
}