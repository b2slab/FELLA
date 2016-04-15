#' Get rowSums/squaredRowSums
#' 
#' Extractor function for rowSums/squaredRowSums
#'
#' @inheritParams .data
#' @inheritParams .type
#' @param squared Logical, whether to return \code{rowSums} (\code{F}) or \code{squaredRowSums} (\code{T})
#'
#' @return Named vector with rowSums/squaredRowSums (internal usage)
getSums <- function(data, type, squared) {
  if (squared) 
    return(slot(data, type)@squaredRowSums)
  
  if (!squared) 
    return(slot(data, type)@rowSums)
  
  return(invisible())
}