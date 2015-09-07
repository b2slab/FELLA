getMatrix <- function(data, type) {
  return(slot(data, type)@matrix)
}