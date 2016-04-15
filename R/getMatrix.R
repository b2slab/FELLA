#' Get matrix for the desired methodology
#' 
#' Extractor function for the matrices of hypergeometric, diffusion and PageRank methodologies
#'
#' @inheritParams .data
#' @inheritParams .type 
#'
#' @return Matrix for the desired methodology (internal usage)
getMatrix <- function(data, type) {
  return(slot(data, type)@matrix)
}