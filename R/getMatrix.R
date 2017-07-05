#' Get matrix for the desired methodology
#' 
#' Extractor function for the matrices of 
#' hypergeometric, diffusion and PageRank methodologies
#'
#' @inheritParams .data
#' @inheritParams .type 
#'
#' @return Matrix for the desired methodology (internal usage)
#' @examples 
#' ## This function is internal
#' attach(environment(FELLA:::getMatrix))
#' data(FELLA.sample)
#' # When a matrix is loaded:
#' x <- getMatrix(FELLA.sample, "hypergeom")
#' dim(x)
#' # When it is not:
#' y <- getMatrix(FELLA.sample, "diffusion")
#' dim(y)
#' y
getMatrix <- function(data, type) {
    return(slot(data, type)@matrix)
}