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
    return(is(x, "FELLA.DATA"))
}