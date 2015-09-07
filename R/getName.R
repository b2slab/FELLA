getName <- function(data, names) {
  ans <- names
  names(ans) <- names
  
  names.intersect <- names %in% names(data@keggdata@id2name)
  ans[names.intersect] <- data@keggdata@id2name[ans[names.intersect]]
  return(ans)
}