getSums <- function(data, type, squared) {
  if (squared) 
    return(slot(data, type)@squaredRowSums)
  
  if (!squared) 
    return(slot(data, type)@rowSums)
  
  return(invisible())
}