#' Get metabolites in the input
#' 
#' Extractor function for the metabolites specified by the user in the input
#'
#' @inheritParams .object
#'
#' @return Vector of metabolites in the input
#' 
#' @export
getInput <- function(object) {
  return(object@userinput@metabolites)
}