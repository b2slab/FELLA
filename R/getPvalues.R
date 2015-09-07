getPvalues <- function(user, type) {
  return(slot(user, type)@pvalues)
}