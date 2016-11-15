#' Check FELLA.USER class
#' 
#' Is \code{x} a \code{\link[FELLA]{FELLA.USER}} object?
#'
#' @param x Object to check
#'
#' @return Logical value stating if \code{x} is a \code{\link[FELLA]{FELLA.USER}} object
#' 
#' @export
is.FELLA.USER <- function(x = NULL) {
  return(is(x, "FELLA.USER"))
}