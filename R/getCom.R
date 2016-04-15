#' Get community
#' 
#' Extractor function for all the nodes from a level/community of KEGG graph
#'
#' @inheritParams .data
#' @inheritParams .level
#' @param format Format of the output, "name" returns names whereas "id" returns vertices id
#'
#' @return Vector of the names/ids of the desired KEGG graph community
getCom <- function(data, level, format = "name") {
  if (format == "name")
    return(names(data@keggdata@id[[level]]))
  
  if (format == "id")
    return(data@keggdata@id[[level]]) 
  
  stop("'format' must be 'name' or 'id'.")
}