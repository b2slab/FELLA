#' Launch a shiny app with FELLA 
#' 
#' Launch a shiny application to perform the metabolomics data enrichment. 
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
