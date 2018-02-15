#' @title Functions to map and enrich a list of metabolites
#' 
#' @description 
#' Functions \code{runHypergeom}, 
#' \code{runDiffusion} and \code{runPagerank} 
#' perform an enrichment on a \code{\link{FELLA.USER}} with 
#' the mapped input metabolites 
#' (through \code{defineCompounds}) 
#' and a \code{\link{FELLA.DATA}} object. 
#' They are based on the hypergeometric test, the heat diffusion model 
#' and the PageRank algorithm, respectively. 
#' 
#' Function \code{enrich} is a wrapper with 
#' the following order: 
#' \code{loadKEGGdata} (optional), 
#' \code{defineCompounds} and one or more in 
#' \code{runHypergeom}, \code{runDiffusion} 
#' and \code{runPagerank}
#' 
#' @template approxTemplate
#' 
#' @details
#' Finally, the function \code{enrich} 
#' is a wrapper to perform the enrichment analysis. 
#' If no \code{\link{FELLA.DATA}} object is supplied, 
#' it loads it, maps the affected compounds and performs 
#' the desired enrichment(s) with a single call.
#' Returned is a list with the loaded 
#' \code{\link{FELLA.DATA}} object 
#' and the results in a \code{\link{FELLA.USER}} object. 
#' Conversely, the user can supply the 
#' \code{\link{FELLA.DATA}} object and the wrapper 
#' will map the metabolites and run the desired enrichment 
#' method(s). 
#' In this case, only the \code{\link{FELLA.USER}} 
#' will be returned.
#'
#' @inheritParams .params
#' @param databaseDir Character, path to load the 
#' \code{\link{FELLA.DATA}} object if
#' it is not already passed through the argument \code{data}
#' @param internalDir Logical, is the directory located 
#' in the package directory?
#' @param ... Further arguments for the enrichment function(s) 
#' \code{runDiffusion}, \code{runPagerank}
#' 
#' @return \code{enrich} returns a 
#' \code{\link{FELLA.USER}} object 
#' updated with the desired enrichment results if 
#' the \code{\link{FELLA.DATA}} was supplied. 
#' Otherwise, a list with the freshly loaded  
#' \code{\link{FELLA.DATA}} object and the 
#' corresponding enrichment in the 
#' \code{\link{FELLA.USER}} object. 
#' 
#' @template refs_enrichment
#' @include runPagerank.R
#' 
#' @rdname enrich-funs 
#' 
#' @examples 
#' ## Load the internal database. 
#' ## This one is a toy example!
#' ## Do not use as a regular database
#' data(FELLA.sample)
#' ## Load a list of compounds to enrich
#' data(input.sample)
#' 
#' ######################
#' ## Example, step by step
#' 
#' ## First, map the compounds
#' obj <- defineCompounds(
#' compounds = c(input.sample, "I_dont_map", "me_neither"), 
#' data = FELLA.sample)
#' obj
#' ## See the mapped and unmapped compounds
#' getInput(obj)
#' getExcluded(obj)
#' ## Compounds are already mapped 
#' ## We can enrich using any method now
#' 
#' ## If no compounds are mapped an error is thrown. Example:
#' \dontrun{
#' data(FELLA.sample)
#' obj <- defineCompounds(
#' compounds = c("C00049", "C00050"), 
#' data = FELLA.sample)}
#' 
#' ## Enrich using hypergeometric test
#' obj <- runHypergeom(
#' object = obj, 
#' data = FELLA.sample)
#' obj
#' 
#' ## Enrich using diffusion
#' ## Note how the results are added;  
#' ## the hypergeometric results are not overwritten
#' obj <- runDiffusion(
#' object = obj, 
#' approx = "normality", 
#' data = FELLA.sample)
#' obj
#' 
#' ## Enrich using PageRank
#' ## Again, this does not overwrite other methods 
#' obj <- runPagerank(
#' object = obj, 
#' approx = "simulation", 
#' data = FELLA.sample)
#' obj
#' 
#' ######################
#' ## Example using the "enrich" wrapper
#' 
#' ## Only diffusion
#' obj.wrap <- enrich(
#' compounds = input.sample, 
#' method = "diffusion", 
#' data = FELLA.sample)
#' obj.wrap
#' 
#' ## All the methods
#' obj.wrap <- enrich(
#' compounds = input.sample, 
#' methods = FELLA::listMethods(), 
#' data = FELLA.sample)
#' obj.wrap
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
    databaseDir = NULL, 
    internalDir = TRUE,
    data = NULL, 
    ...) {
    
    # Check if data is loaded
    returnList <- FALSE
    if (!is.FELLA.DATA(data)) {
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