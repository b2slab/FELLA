#' Get community
#' 
#' Extractor function for all the nodes from a level/community of KEGG graph
#'
#' @inheritParams .data
#' @inheritParams .level
#' @param format Format of the output, "name" returns KEGG IDs whereas 
#'  "id" returns vertices IDs
#'
#' @return Vector of the names/ids of the desired KEGG graph community
#' 
#' @examples 
#' ## This function is internal
#' attach(environment(FELLA:::getCom))
#' data(FELLA.sample)
#' ## Pathways
#' getCom(FELLA.sample, 1, format = "name")
#' getCom(FELLA.sample, 1, format = "id")
#' ## Modules
#' getCom(FELLA.sample, 2)
#' ## Enzymes
#' head(getCom(FELLA.sample, 3))
#' ## Reactions
#' head(getCom(FELLA.sample, 4))
#' ## Compounds
#' head(getCom(FELLA.sample, 5))
getCom <- function(data, level, format = "name") {
  if (format == "name")
    return(names(data@keggdata@id[[level]]))
  
  if (format == "id")
    return(data@keggdata@id[[level]]) 
  
  stop("'format' must be 'name' or 'id'.")
}