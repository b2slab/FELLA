#' Get excluded compounds
#' 
#' Extractor function for the compounds in the input that were not mapped to the KEGG graph
#'
#' @inheritParams .object
#'
#' @return Vector of the excluded compounds
#' 
#' @export
getExcluded <- function(object) {
  return(object@userinput@excluded)
}