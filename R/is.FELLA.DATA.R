#' Check FELLA.DATA class
#' 
#' Is \code{x} a \code{\link{FELLA.DATA}} object?
#'
#' @param x Object to check
#'
#' @return Logical value stating if \code{x} is a \code{\link{FELLA.DATA}} object
#' 
#' @export
is.FELLA.DATA <- function(x = NULL) {
  return(class(x) == "FELLA.DATA")
}