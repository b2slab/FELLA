#' Check FELLA.DATA class
#' 
#' Is \code{x} a \code{\link[FELLA]{FELLA.DATA}} object?
#'
#' @param x Object to check
#'
#' @return Logical value stating if \code{x} 
#' is a \code{\link[FELLA]{FELLA.DATA}} object
#' 
#' @export
#' @examples 
#' data(FELLA.sample)
#' is.FELLA.DATA(FELLA.sample)
#' is.FELLA.DATA(42)
is.FELLA.DATA <- function(x = NULL) {
    return(inherits(x, "FELLA.DATA"))
}

#' Check FELLA.USER class
#' 
#' Is \code{x} a \code{\link[FELLA]{FELLA.USER}} object?
#'
#' @param x Object to check
#'
#' @return Logical value stating if \code{x} 
#' is a \code{\link[FELLA]{FELLA.USER}} object
#' 
#' @examples 
#' is.FELLA.USER(new("FELLA.USER"))
#' is.FELLA.USER(42)
#' 
#' data(FELLA.sample)
#' data(input.sample)
#' obj <- enrich(
#' compounds = input.sample, 
#' method = "diffusion", 
#' data = FELLA.sample)
#' is.FELLA.USER(obj)
#' 
#' @export
is.FELLA.USER <- function(x = NULL) {
    return(inherits(x, "FELLA.USER"))
}