#' Get matrix for the p-value regarding CC size
#' 
#' Extractor function for the matrix containing p-value by CC size
#' that compares to a random selection of nodes in the KEGG graph
#'
#' @inheritParams .data
#'
#' @return Matrix with p-values for CC size (internal usage)
#' 
#' @examples 
#' ## This function is internal
#' attach(environment(FELLA:::getPvaluesSize))
#' data(FELLA.sample)
#' M <- getPvaluesSize(FELLA.sample)
#' dim(M)
#' summary(as.vector(M))
getPvaluesSize <- function(data) {
    return(data@keggdata@pvalues.size)
}