#' Get the slot "valid"
#' 
#' Extractor function for the slot "valid"
#'
#' @inheritParams .object
#' @inheritParams .type 
#'
#' @return Slot "valid" (internal usage)
getValid <- function(object, type) {
  return(slot(object, type)@valid)
}