#' Map KEGG identifiers to KEGG names
#' 
#' Map KEGG identifiers to KEGG names, multiple names 
#' for an ID are reported if annotated. 
#' The KEGG identifiers may have mixed levels.
#'
#' @inheritParams .data
#' @param id KEGG IDs whose name is desired
#'
#' @return List whose names are KEGG IDs and 
#' whose entries are the vectors of matches
#' @examples 
#' data(FELLA.sample)
#' getName(FELLA.sample, c("C00002", "C00040"))
#' 
#' @export
getName <- function(data, id) {
  ans <- id
  names(ans) <- id
  
  names.intersect <- id %in% names(data@keggdata@id2name)
  ans[names.intersect] <- data@keggdata@id2name[ans[names.intersect]]
  return(ans)
}