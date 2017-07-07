#' Get p-scores from the desired methodology
#' 
#' Extractor function for the p-scores using the desired methodology
#'
#' @inheritParams .object
#' @inheritParams .type
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
getPscores <- function(object, type) {
    if (type == "hypergeom") return(slot(object, type)@pvalues)
    return(slot(object, type)@pscores)
}
