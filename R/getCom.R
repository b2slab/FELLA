getCom <- function(data, type, ans = "name") {
  if (ans == "name")
    return(names(data@keggdata@id[[type]]))
  
  if (ans == "id")
    return(data@keggdata@id[[type]]) 
  
  stop("'ans' must be 'name' or 'id'.")
}