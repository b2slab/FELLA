getUsed <- function(object, type) {
  return(slot(object, type)@used)
}