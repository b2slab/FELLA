#' Internal function to plot a solution graph
#' 
#' This function plots a solution graph, tailored for the diffusion and pagerank
#' analysis. 
#'
#' @param graph Graph result that must come from diffusion or pagerank analysis
#' @param input Character vector, compounds in the input to be highlighted
#' @inheritParams .layout
#' @inheritParams .NamesAsLabels
#' @param ... Additional parameters passed to \code{\link[igraph]{plot.igraph}}
#' 
#' @return If \code{layout = F} then the value returned is \code{invisible()}. 
#' Otherwise, the layout is returned, also in an invisible fashion.
#' 
#' @import igraph
plotGraph <- function(graph = NULL, 
                      input = NULL, 
                      layout = F,
                      NamesAsLabels = T, 
                      ...) {
  
  if (vcount(graph) == 0) {
    warning("The graph is empty and won't be plotted.")
    return(invisible())
  }

  GO.CellularComponent <- F
  # If there is GO cellular component data available, plot it..!
  if ("GO.CC" %in% list.vertex.attributes(graph)) {
    GO.CC <- V(graph)$GO.CC
    GO.CellularComponent <- T
  }
  
  # triangle vertex shape
  #########################################################
  # Code from igraph examples (vertex.shapes documentation)
  mytriangle <- function(coords, v=NULL, params) {
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
      vertex.color <- vertex.color[v]
    }
    vertex.size <- 1/200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
      vertex.size <- vertex.size[v]
    }
    
    symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
            stars=cbind(vertex.size, vertex.size, vertex.size),
            add=TRUE, inches=FALSE)
  }
  # clips as a circle
  add.vertex.shape("triangle", clip=vertex.shapes("circle")$clip,
                   plot=mytriangle)
  #########################################################
  
  # Nodes in the input
  graph.input <- intersect(input, V(graph)$name)
  graph.input <- as.numeric(V(graph)[graph.input])

  graph.com <- as.character(V(graph)$com)
  
  # Vertex shape
  vertex.shape <- rep("circle", vcount(graph))
  vertex.shape[graph.input] <- "square"

  vertex.number <- vcount(graph)
  
  # Centrality for the transparency attribute
  graph.alpha <- betweenness(graph, directed = F, normalized = T)
  graph.alpha[is.nan(graph.alpha)] <- 0
  
  graph.asp <- 1
  graph.layout <- layout.auto(graph)
  graph.layout <- layout.norm(graph.layout, 
                              xmin = -1, 
                              xmax = 1, 
                              ymin = -1, 
                              ymax = 1)
  
  vertex.color <- sapply(V(graph), function(y) {
    solidColor <- switch(graph.com[y], 
                         "1" = "#CD0000",
                         "2" = "#CD96CD",
                         "3" = "#FFD500",
                         "4" = "#8DB6CD",
                         "5" = "#548B54"
    )

    if (GO.CellularComponent && GO.CC[y] != -1) {
      if (GO.CC[y] < 0.5) solidColor <- "#FFD500"
      else if (GO.CC[y] < 0.7) solidColor <- "#FF5500"
      else if (GO.CC[y] < 0.9) solidColor <- "#FF0000"
      else solidColor <- "#B300FF"
    }
# browser()

    transpa <- max(round(10*vertex.number*graph.alpha[y]), 80)
    if (transpa > 150) transpa <- 150
    
    transpaColor <- format(as.hexmode(transpa), upper.case = T)
    if (transpa < 16) transpaColor <- paste0("0", transpaColor)
    
    return(c(paste0(solidColor, "FF"), paste0(solidColor, transpaColor)))
  })
  rownames(vertex.color) <- c("label", "node")

  # Vertex frame color

  vertex.frame.color <- rep("black", vcount(graph))
  if (GO.CellularComponent) {
    vertex.frame.color[GO.CC != -1] <- "#CD0000"
    vertex.shape[GO.CC != -1] <- "triangle"
  }

  # Vertex size
  vertex.size <- sapply(graph.com, function(y) {
    vertexSize <- switch(y, 
                        "1" = 7,
                        "2" = 5.5,
                        "3" = 4.25,
                        "4" = 3.5,
                        "5" = 3
    )
    return(vertexSize)
  })
  
  vertex.size[graph.input] <- 4
  
  vertex.size <- vertex.size*(300/vcount(graph))^(1/3)
  
  # Labels
  vertex.label.dist <- 0.1*(300/vcount(graph))^(1/3)
  vertex.label.degree <- -pi/2
  
  if (NamesAsLabels) {
    vertex.label <- V(graph)$LABEL
  } else {
    vertex.label <- V(graph)$name
  }

  plot.igraph(x = graph, 
              layout = graph.layout, 
              vertex.size = vertex.size, 
              vertex.label = vertex.label, 
              vertex.label.dist = vertex.label.dist, 
              vertex.label.color = vertex.color["label", ], 
              vertex.label.degree = vertex.label.degree, 
              vertex.frame.color = vertex.frame.color, 
              vertex.color = vertex.color["node", ], 
              vertex.shape = vertex.shape,
              edge.curved = F, 
              edge.color = "#000000AA", 
              edge.arrow.size = 0.25,
              asp = graph.asp, 
              ...)

  # Plot the legend
  showLegend(GO.CellularComponent)
  
  if (!layout) {
    return(invisible(NULL))
  } else  {
    x <- graph.layout[, 1]
    y <- graph.layout[, 2]
    out.id <- sapply(V(graph), function(vertex) paste0(
      switch(as.character(V(graph)[vertex]$com),
             "1" = "",
             "2" = "md:",
             "3" = "ec:",
             "4" = "rn:",
             "5" = "cpd:"),
      V(graph)[vertex]$name))
    
    out.name <- V(graph)$LABEL
    out.complete <- data.frame(x, y, out.id, out.name, stringsAsFactors = F)
    names(out.complete) <- c("x", "y", "out.id", "out.name")
    
    return(invisible(out.complete))
  }
  
}