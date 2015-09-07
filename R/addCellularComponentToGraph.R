addCellularComponentToGraph <- function(graph = NULL, 
                                        GO.CellularComponent = NULL, 
                                        GONamesAsLabels = T) {
  
  library(GOSemSim)
  
  if ((length(GO.CellularComponent) == 0) | (GO.CellularComponent == ""))
    return(graph)
  
  similarity.CC <- lapply(V(graph)$GO, FUN = function(x) {
    if (!is.na(x[1])) {
      ids <- names(x)
      #     browser()
      
      sim <- goSim(GOID1 = GO.CellularComponent, 
                   GOID2 = ids, 
                   ont = "CC", 
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
  
  detach("package:GOSemSim", unload = T)
  
  # Careful! We add it as a list!
  graph <- set.vertex.attribute(graph = graph, name = "GO.CC", value = similarity.CC)
  
  return(graph)
}