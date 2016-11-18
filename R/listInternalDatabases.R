#' List internal databases
#' 
#' This function lists the directories in the local database path
#'
#' @param full.names Logical, should full paths be returned?
#'
#' @return Vector with database directories
#' 
#' @examples 
#' listInternalDatabases()
#' 
#' @export
listInternalDatabases <- function(full.names = FALSE) {
  dir.internal <- system.file("database", package = "FELLA")
  
  if (dir.internal == "") {
    message("No local databases have been built yet.")
    return(NULL)
  }
  
  list.dirs(dir.internal, full.names = full.names, recursive = FALSE)
}