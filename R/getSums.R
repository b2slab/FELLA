#' Get rowSums/squaredRowSums
#' 
#' Extractor function for rowSums/squaredRowSums
#'
#' @inheritParams .data
#' @inheritParams .type
#' @param squared Logical, whether to return 
#' \code{rowSums} (\code{F}) or \code{squaredRowSums} (\code{T})
#'
#' @return Named vector with rowSums/squaredRowSums (internal usage)
#' @examples 
#' ## This function is internal
#' attach(environment(FELLA:::getSums))
#' data(FELLA.sample)
#' rowsums <- getSums(FELLA.sample, "diffusion", squared = FALSE)
#' hist(rowsums)
getSums <- function(data, type, squared) {
  if (squared) 
    return(slot(data, type)@squaredRowSums)
  
  if (!squared) 
    return(slot(data, type)@rowSums)
  
  return(invisible())
}