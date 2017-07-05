#' Launch a shiny app with FELLA 
#' 
#' Launch a shiny application to perform the metabolomics data enrichment. 
#'
#' @param port,host,launch.browser Port and host to launch the app, 
#' and whether to launch browser or not
#' @param ... Additional parameters passed to \code{\link[shiny]{runApp}}
#'
#' @return \code{invisible()}, but as a side effect the app will be launched
#' 
#' @examples 
#' r <- try(launchApp())
#' 
launchApp <- function(
    port = 8888, 
    host = "localhost", 
    launch.browser = FALSE, 
    ...
) {
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop(
            "The 'shiny' package must be installed ", 
            "to run the interactive app", 
            call. = FALSE)
    }
    
    dir.app <- system.file("shiny", package = "FELLA")
    
    if (host == "eko") host <- "147.83.71.87"
    if (host == "localhost") host <- "127.0.0.1"
    
    if (dir.app == "") {
        warning(
            "Something went wrong and the shiny app is not in the ", 
            "application directory. Try reinstalling FELLA...")
        return(invisible())
    }
    
    shiny::runApp(
        appDir = dir.app, 
        launch.browser = launch.browser, 
        port = port, 
        host = host, 
        ...)
    
    invisible()
}
