#' Get the slot "valid"
#' 
#' Extractor function for the slot "valid"
#'
#' @inheritParams .params
#'
#' @return Slot "valid" (internal usage)
#' 
#' @examples 
#' ## This function is internal
#' attach(environment(FELLA:::getValid))
#' 
#' data(FELLA.sample)
#' data(input.sample)
#' 
#' obj <- enrich(
#' compounds = input.sample, 
#' method = "diffusion", 
#' data = FELLA.sample)
#' 
#' ## If the analysis is valid
#' getValid(obj, "diffusion")
#' 
#' ## Otherwise
#' getValid(new("FELLA.USER"), "diffusion")
#' getValid(obj, "pagerank")
#' 
getValid <- function(object, method.) {
    return(slot(object, method.)@valid)
}