#' @title Launch a shiny app with FELLA 
#' 
#' @description 
#' \code{\link[FELLA]{launchApp}} deploys a shiny application 
#' to perform the metabolomics data enrichment. 
#' Although this app does not provide all the options available in 
#' \code{\link[FELLA]{FELLA}}, it is easily accessible for the lay user. 
#' 
#' @details 
#' The graphical interface allows to: (1) upload the data and 
#' check if the KEGG ids have successfully mapped, 
#' (2) select database, set analysis and graphical parameters, 
#' (3) interactively browse the resulting sub-network as a graph or 
#' as a table, and (4) export such results as a table or a network. 
#' At least one database is needed before deploying the app. 
#' See \code{?buildDataFromGraph} for further details.  
#'
#' @param ... Parameters passed to \code{\link[shiny]{runApp}}
#'
#' @return \code{invisible()}, but as a side effect the app will be launched
#' 
#' @examples 
#' \dontrun{
#' r <- try(launchApp())
#' }
#' 
#' @export
launchApp <- function(...) {
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop(
            "The 'shiny' package must be installed ", 
            "to run the interactive app", 
            call. = FALSE)
    }
    
    dir.app <- system.file("shiny", package = "FELLA")
    
    if (dir.app == "") {
        warning(
            "Something went wrong and the shiny app is not in the ", 
            "application directory. Try reinstalling FELLA...")
        return(invisible())
    }
    
    shiny::runApp(appDir = dir.app, ...)
    
    invisible()
}
