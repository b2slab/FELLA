#' Get metabolites in the input
#' 
#' Extractor function for the metabolites 
#' specified by the user in the input
#'
#' @inheritParams .params
#'
#' @return Vector of metabolites in the input
#' 
#' @examples
#' data(FELLA.sample)
#' data(input.sample)
#' 
#' ## No excluded compounds: the input is recovered as is
#' obj <- defineCompounds(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' i1 <- getInput(obj)
#' 
#' ## One compound does not map: the input contains only the mapped entities
#' obj <- defineCompounds(
#' compounds = c(input.sample, "intruder"), 
#' data = FELLA.sample)
#' i2 <- getInput(obj)
#' 
#' identical(sort(i1), sort(i2))
#' @export
getInput <- function(object) {
    return(object@userinput@metabolites)
}