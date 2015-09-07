getValid <- function(user, type) {
  return(slot(user, type)@valid)
}