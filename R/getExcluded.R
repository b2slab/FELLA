#' Get excluded compounds
#' 
#' Extractor function for the compounds in the input 
#' that were not mapped to the KEGG graph
#'
#' @inheritParams .object
#'
#' @return Vector of the excluded compounds
#' 
#' @examples 
#' data(FELLA.sample)
#' data(input.sample)
#' 
#' ## No excluded compounds
#' obj <- defineCompounds(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' getExcluded(obj)
#' 
#' ## One compound does not map
#' ## The user gets a warning as well
#' obj <- defineCompounds(
#' compounds = c(input.sample, "intruder"), 
#' data = FELLA.sample)
#' getExcluded(obj)
#' @export
getExcluded <- function(object) {
    return(object@userinput@excluded)
}