#' Get KEGG graph
#' 
#' Extractor function for the KEGG graph from the FELLA.DATA object
#'
#' @inheritParams .data
#'
#' @return KEGG graph as an \link[igraph]{igraph} object
getGraph <- function(data) {
  return(data@keggdata@graph)
}