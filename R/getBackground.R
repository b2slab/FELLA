#' Get compounds in the defined background
#' 
#' Extractor function for the compounds defined as background
#'
#' @inheritParams .object
#'
#' @return Vector of compounds in the background. 
#' If this vector is empty, all 
#' the compounds are used as background by default.
#' 
#' @examples
#' data(FELLA.sample)
#' data(input.sample)
#' input <- head(input.sample, 12)
#' 
#' ## If the background is default, we see an empty vector 
#' ## Note that the number of iterations is really small in the example
#' obj <- enrich(
#' compounds = input, 
#' method = "diffusion", 
#' approx = "simulation", 
#' niter = 100, 
#' data = FELLA.sample)
#' 
#' getBackground(obj)
#' 
#' ## Otherwise we see the background compounds that mapped to the graph
#' obj <- enrich(
#' compounds = input, 
#' compoundsBackground = input.sample, 
#' method = "diffusion", 
#' approx = "simulation", 
#' niter = 100, 
#' data = FELLA.sample)
#' getBackground(obj)
#' 
#' @export
getBackground <- function(object) {
  return(object@userinput@metabolitesbackground)
}