#' Get the slot "used"
#' 
#' Extractor function for the slot "used"
#'
#' @inheritParams .object 
#' @inheritParams .type 
#'
#' @return Slot "used" (internal usage)
getUsed <- function(object, type) {
  return(slot(object, type)@used)
}