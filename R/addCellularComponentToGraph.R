#' Internal function to add the CC semantic similarity attribute to the 
#' results graph object
#' 
#' Function \code{addCellularComponentToGraph} takes and returns a graph object with class 
#' \code{\link[igraph]{igraph}} adding the attribute \code{GO.CC} for semantic similarity.
#' 
#'
#' @param graph An \code{\link[igraph]{igraph}} object, typically a small one 
#' coming from an enrichment procedure
#' @inheritParams .GO.CellularComponent
#' @inheritParams .GONamesAsLabels
#'
#' @return An \code{\link{igraph}} object that contains an extra attribute: \code{GO.CC}
#' 
#' @import igraph
#' @importFrom GOSemSim goSim
addCellularComponentToGraph <- function(graph = NULL, 
                                        GO.CellularComponent = NULL, 
                                        GONamesAsLabels = T) {
  
  #library(GOSemSim)
  
  if ((length(GO.CellularComponent) == 0) | (GO.CellularComponent == ""))
    return(graph)
  
  similarity.CC <- lapply(V(graph)$GO, FUN = function(x) {
    if (!is.na(x)) {
      ids <- names(x)
      #     browser()
      
      sim <- GOSemSim::goSim(GOID1 = GO.CellularComponent, 
                             GOID2 = ids, 
                             # ont = "CC", 
                             organism = "human")
      
      # Pick best similarity
      val <- max(sim, na.rm = T)
      
      # Infinity if all the similarities are NA... so we treat is as missing (-1)
      if (val == -Inf) {
        warning("Semantic similarity returned NA. Is the GO term properly typed?")
        val <- -1
        names(val) <- "NONE"
      } else {
        if (GONamesAsLabels) names(val) <- x[which.max(sim)]
        else names(val) <- ids[which.max(sim)]
      }
      
      return(val)
    }
    # If it is NA... place -1
    val <- -1
    names(val) <- "NONE"
    #   show(val)
    return(val)
  })
  
  #detach("package:GOSemSim", unload = T)
  
  # Careful! We add it as a list!
  graph <- set.vertex.attribute(graph = graph, name = "GO.CC", value = similarity.CC)
  
  return(graph)
}