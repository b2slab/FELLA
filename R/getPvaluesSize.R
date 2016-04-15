#' Get matrix for the p-value regarding CC size
#' 
#' Extractor function for the matrix containing p-value by CC size
#' that compares to a random selection of nodes in the KEGG graph
#'
#' @inheritParams .data
#'
#' @return Matrix with p-values for CC size (internal usage)
getPvaluesSize <- function(data) {
  return(data@keggdata@pvalues.size)
}