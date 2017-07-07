#' List of node categories
#' 
#' Node categories used in the internal representations
#'
#' @return Character vector
#' 
#' @examples 
#' listCategories()
#' 
#' @export
listCategories <- function() {
    c("pathway", "module", "enzyme", "reaction", "compound")
}

#' List of methods
#' 
#' Methods available for the analysis
#'
#' @return Character vector
#' 
#' @examples 
#' listMethods()
#' 
#' @export
listMethods <- function() {
    c("hypergeom", "diffusion", "pagerank")
}

#' List of approximations
#' 
#' Available approximations for the analysis
#'
#' @return Character vector
#' 
#' @examples 
#' listApprox()
#' 
#' @export
listApprox <- function() {
    c("simulation", "normality", "gamma", "t")
}
