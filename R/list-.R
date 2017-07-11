#' List of node categories
#' 
#' Node categories used in the internal representations
#'
#' @return Character vector
#' 
#' @examples 
#' listCategories()
#' 
#' @export
listCategories <- function() {
    c("pathway", "module", "enzyme", "reaction", "compound")
}

#' List of methods
#' 
#' Methods available for the analysis
#'
#' @return Character vector
#' 
#' @examples 
#' listMethods()
#' 
#' @export
listMethods <- function() {
    c("hypergeom", "diffusion", "pagerank")
}

#' List of approximations
#' 
#' Available approximations for the analysis
#'
#' @return Character vector
#' 
#' @examples 
#' listApprox()
#' 
#' @export
listApprox <- function() {
    c("simulation", "normality", "gamma", "t")
}

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