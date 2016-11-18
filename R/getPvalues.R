#' Get p-values from the desired methodology
#' 
#' Extractor function for the p-values using the desired methodology
#'
#' @inheritParams .object
#' @inheritParams .type
#'
#' @return Named vector of p-values
#' 
#' @examples
#' data(FELLA.sample)
#' data(input.sample)
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' p <- getPvalues(obj, "diffusion")
#' sum(p < 0.1)
#' 
#' @export
getPvalues <- function(object, type) {
  return(slot(object, type)@pvalues)
}
