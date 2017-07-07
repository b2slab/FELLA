#' Get p-scores from the desired methodology
#' 
#' Extractor function for the p-scores using the desired methodology
#'
#' @inheritParams .params
#'
#' @return Named vector of p-scores
#' 
#' @examples
#' data(FELLA.sample)
#' data(input.sample)
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' p <- getPscores(obj, "diffusion")
#' sum(p < 0.1)
#' 
#' @export
getPscores <- function(object, method.) {
    if (method. == "hypergeom") return(slot(object, method.)@pvalues)
    return(slot(object, method.)@pscores)
}
