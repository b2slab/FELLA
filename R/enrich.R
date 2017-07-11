#' Wrapper function for enrichment analysis
#' 
#' The function \code{\link[FELLA]{enrich}} is a big wrapper 
#' to perform the enrichment analysis. It loads affected compounds, 
#' KEGG data and return a list containing 
#' the \code{\link[FELLA]{FELLA.DATA}} context data and the 
#' \code{\link[FELLA]{FELLA.USER}} 
#' object with the enrichment results available.
#'
#' @inheritParams .params
#' @param databaseDir Character, path to load the 
#' \code{\link[FELLA]{FELLA.DATA}} object if
#' it is not already passed through the argument \code{data}
#' @param internalDir Logical, is the directory located 
#' in the package directory?
#' @param ... Further arguments for the enrichment function(s) 
#' \code{\link[FELLA]{runDiffusion}}, \code{\link[FELLA]{runPagerank}}
#' 
#' @return The \code{\link[FELLA]{FELLA.USER}} 
#' and the \code{\link[FELLA]{FELLA.DATA}} objects 
#' in a list.
#' 
#' @examples 
#' ## Load the internal database. This one is a toy example!
#' ## Do not use as a regular database)
#' data(FELLA.sample)
#' ## Load a list of compounds to enrich
#' data(input.sample)
#' ## Launch wrapper
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' obj
#' 
#' @export
enrich <- function(
    compounds = NULL, 
    compoundsBackground = NULL, 
    methods = listMethods(), 
    loadMatrix = "none", 
    approx = "normality", 
    t.df = 10, 
    niter = 1000, 
    databaseDir = "myDatabase", 
    internalDir = TRUE,
    data = NULL, 
    ...) {
    
    # Check if data is loaded
    returnList <- FALSE
    if (class(data) != "FELLA.DATA") {
        message(
            "No data object supplied. ", 
            "Loading it from the 'databaseDir' argument...")
        returnList <- TRUE
        data <- loadKEGGdata(
            databaseDir = databaseDir, 
            internalDir = internalDir, 
            loadMatrix = loadMatrix)
    } 
    
    # Define custom metabolites
    object <- defineCompounds(
        compounds = compounds, 
        compoundsBackground = compoundsBackground, 
        data = data)
    
    
    # Run all the analyses
    if ("hypergeom" %in% methods) {
        object <- runHypergeom(object = object, data = data)
    }
    
    if ("diffusion" %in% methods) {
        object <- runDiffusion(
            object = object, 
            data = data, 
            approx = approx, 
            t.df = t.df, 
            niter = niter, 
            ...)
    }
    
    if ("pagerank" %in% methods) {
        object <- runPagerank(
            object = object, 
            data = data, 
            approx = approx, 
            t.df = t.df, 
            niter = niter, 
            ...)
    }
    
    if (returnList) {
        return(list(user = object, data = data))
    }
    
    return(object)
}