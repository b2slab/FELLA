#' Get p-values from the desired methodology
#' 
#' Extractor function for the p-values using the desired methodology
#'
#' @inheritParams .object
#' @inheritParams .type
#'
#' @return Named vector of p-values
#' 
#' @export
getPvalues <- function(object, type) {
  return(slot(object, type)@pvalues)
}
