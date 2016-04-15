#' Get compounds in the defined background
#' 
#' Extractor function for the compounds defined as background
#'
#' @inheritParams .object
#'
#' @return Vector of compounds in the background. If this vector is empty, all 
#' the compounds are used as background by default.
#' 
#' @export
getBackground <- function(object) {
  return(object@userinput@metabolitesbackground)
}