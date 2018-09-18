#' Dummy function with function arguments
#' 
#' This function eases parameter inheritance to centralise the 
#' documentation
#' 
#' @param databaseDir Path for the KEGG RData files
#' @param internalDir Logical, is the directory located 
#' in the package directory?
#' @param object FELLA.USER object
#' @param data FELLA.DATA object
#' @param type Character vector, containing entries in
#' "hypergeom", "diffusion" or "pagerank"
#' @param level Desired level, can be coded as a number or a character: 
#' 1 or "pathway"; 2 or "module"; 3 or "enzyme"; 
#' 4 or "reaction"; 5 or "compound".
#' @param method Character, exactly one of: 
#' \code{"hypergeom"}, \code{"diffusion"}, \code{"pagerank"}
#' @param methods Character vector, containing some of: 
#' \code{"hypergeom"}, \code{"diffusion"}, \code{"pagerank"}
#' @param approx Character: "simulation" for Monte Carlo, "normality", 
#' "gamma" or "t" for parametric approaches
#' @param loadMatrix Character vector to choose if 
#' heavy matrices should be loaded. 
#' Can contain: \code{"diffusion"}, \code{"pagerank"}
#' @param threshold Numeric value between 0 and 1. 
#' \code{p.score} threshold applied when filtering KEGG nodes. 
#' Lower thresholds are more stringent. 
#' @param thresholdConnectedComponent Numeric value between 0 and 1. 
#' Connected components that are below the threshold are kept, 
#' while the ones exceeding it (because they are too small) are discarded. 
#' @param plimit Pathway limit, must be a numeric value between 1 and 50. 
#' Limits the amount of pathways in \code{method = "hypergeom"}
#' @param nlimit Node limit, must be a numeric value between 1 and 1000. 
#' Limits the order of the solution sub-graph when 
#' in \code{method = "diffusion"} and \code{method = "pagerank"}
#' @param niter Number of iterations (permutations) 
#' for Monte Carlo ("simulation"), 
#' must be a numeric value between 1e2 and 1e5
#' @param layout Logical, should the plot be returned as a layout?
#' @param graph An \pkg{igraph} object, 
#' typically a small one, 
#' coming from an enrichment through \code{"diffusion"} or \code{"pagerank"}.
#' @param GOterm Character, GO entry to draw 
#' semantic similarity in the solution graph. 
#' If \code{NULL}, the GO labels will be appended without similarities.
#' @param GONamesAsLabels Logical, should GO names 
#' be displayed as labels instead of GO identifiers?
#' @param LabelLengthAtPlot Numeric value between 10 and 50. 
#' Maximum length that a label can reach when plotting the graph. 
#' The remaining characters will be truncated using "..."
#' @param godata.options List, options for the database creator 
#' \code{\link[GOSemSim]{godata}}
#' @param mart.options List, options for the \code{biomaRt} function
#' \code{\link[biomaRt]{getBM}}. Importantly, this defines the organism, 
#' see \code{\link[biomaRt]{listDatasets}} for possibilities. 
#' If calling \code{generateEnzymesTable}, the user can set 
#' \code{mart.options = NULL} to avoid adding GO labels to enzymes.
# Other
#' @param p.adjust Character passed to the 
#' \code{\link[stats]{p.adjust}} method
#' @param dampingFactor Numeric value between 0 and 1 (none inclusive), 
#' damping factor \code{d} for 
#' PageRank (\code{\link[igraph:page_rank]{page.rank}})
#' @param t.df Numeric value; number of degrees of freedom 
#' of the t distribution 
#' if the approximation \code{approx = "t"} is used
#' @param compounds Character vector containing the 
#' KEGG IDs of the compounds considered as affected
#' @param compoundsBackground Character vector containing the KEGG IDs of 
#' the compounds that belong to the background. Can be \code{NULL} for the 
#' default background (all compounds)
#' @param NamesAsLabels Logical, should KEGG names be displayed 
#' as labels instead of KEGG identifiers?
#' @param capPscores Numeric value, minimum p-score 
#' admitted for the readable 
#' formatting. Smaller p-scores will be displayed 
#' as \code{< capPscores} 
#' 
#' @return \code{NULL}
#' 
#' @keywords internal
.params <- function() {}

#' Internal function to check arguments and give personalised errors
#' 
#' This function checks if the arguments are of the desired type, length 
#' and range. If it fails, it gives an error explaining why the argument 
#' is invalid. 
#'
#' @inheritParams .params
#' @param ... ignored arguments
#' 
#' @return A list with values. Currently only a 
#' logical value named \code{valid} 
#' if the process runs smoothly. If the checking fails, 
#' it also returns an 
#' object called \code{ans}, which depends on 
#' the situation (can be the original object, 
#' \code{NULL}, et cetera). 
#' 
#' @examples 
#' ## This function is internal
#' arg1 <- FELLA:::checkArguments(method = "hello")
#' arg1$valid
#' arg2 <- FELLA:::checkArguments(method = "diffusion")
#' arg2$valid
#' 
#' @keywords internal
checkArguments <- function(
    databaseDir = "myDatabase", 
    internalDir = TRUE, 
    method = "diffusion", 
    methods = "diffusion", 
    approx = "normality", 
    loadMatrix = NULL, 
    threshold = 0.05,
    plimit = 15, 
    nlimit = 250, 
    niter = 1e3, 
    t.df = 10, 
    dampingFactor = 0.85, 
    layout = FALSE, 
    thresholdConnectedComponent = 0.05, 
    GOterm = NULL,
    GONamesAsLabels = TRUE, 
    LabelLengthAtPlot = 22, 
    object = new("FELLA.USER"), 
    data = new("FELLA.DATA"), 
    ...) {
    
    # DIRECTORIES
    ###########################
    
    if ((!is.character(databaseDir) & !is.null(databaseDir))  | 
        length(databaseDir) > 1) {
        message(
            "'databaseDir' must be a length 1 character ", 
            " or NULL.")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (!is.logical(internalDir) | is.na(internalDir)) {
        message("'internalDir' must be a non-NA logical value")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # METHOD
    ###########################
    
    # version with length restriction
    if (!is.character(method)) {
        message(
            "'method' must be of character type and must be one of: ", 
            "'hypergeom', 'diffusion' and 'pagerank'. ", 
            "Returning original 'object'...")
        return(list(ans = object, valid = FALSE))
    }
    if (length(method) > 1) {
        message(
            "'method' must be a length 1 character. ", 
            "Returning original 'object'...")
        return(list(ans = object, valid = FALSE))
    }
    if (!(method %in% listMethods())) {
        message(
            "'method' must contain exactly one of: ", 
            "'hypergeom', 'diffusion' or 'pagerank'. ", 
            "Returning original 'object'...")
        return(list(ans = object, valid = FALSE))
    }
    ## method (several allowed)
    if (!is.character(methods)) {
        message(
            "'methods' must be a character containing some of: ", 
            "'hypergeom', 'diffusion' or 'pagerank'. ", 
            "Returning original 'object'...")
        return(list(ans = object, valid = FALSE))
    }
    if (!any(methods %in% listMethods())) {
        message(
            "'methods' must be a character vector ", "
            containing at least one of: ", 
            "'hypergeom', 'diffusion' or 'pagerank'. ", 
            "Returning original 'object'...")
        return(list(ans = object, valid = FALSE))
    }
    
    # approx
    ###########################
    
    if (!is.character(approx)) {
        message(
            "'approx' must be a character: ", 
            "'simulation',  'normality', 'gamma' or 't'. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(approx) > 1) {
        message(
            "'approx' must be a length 1 character. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (!(approx %in% listApprox())) {
        message(
            "'approx' must be a character: ", 
            "'simulation',  'normality', 'gamma' or 't'. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # loadMatrix
    #################
    if (!is.null(loadMatrix) & !is.character(loadMatrix)) {
        message(
            "'loadMatrix' can only be a character vector containing some in: ", 
            "'diffusion', 'pagerank', or NULL.")
        return(list(ans = NULL, valid = FALSE))
    }
    
    
    # threshold
    #################
    if (!is.numeric(threshold)) {
        message(
            "'threshold' must be numeric between 0 and 1. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(threshold) > 1) {
        message(
            "'threshold' must be a length 1 numeric. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (threshold <= 0 | threshold > 1) {
        message(
            "'threshold' must be numeric between 0 and 1. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # plimit
    ##########################
    if (!is.numeric(plimit)) {
        message(
            "'plimit' must be numeric between 1 and 50. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(plimit) > 1) {
        message(
            "'plimit' must be a length 1 numeric. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (plimit <= 0 | plimit >= 50) {
        message(
            "'plimit' must be numeric between 1 and 50. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # nlimit
    ##########################
    if (!is.numeric(nlimit)) {
        message(
            "'nlimit' must be numeric between 1 and 1000. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(nlimit) > 1) {
        message(
            "'nlimit' must be a length 1 numeric. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (nlimit <= 0 | nlimit > 1000) {
        message(
            "'nlimit' must be numeric between 1 and 1000. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # niter
    ##########################
    if (!is.numeric(niter)) {
        message(
            "'niter' must be numeric between 1e2 and 1e5. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(niter) > 1) {
        message(
            "'niter' must be a length 1 numeric. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (niter < 1e2 | niter > 1e5) {
        message(
            "'niter' must be numeric between 1e2 and 1e5. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # t.df
    ##########################
    if (!is.numeric(t.df)) {
        message(
            "'t.df' must be a positive numeric value. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(t.df) > 1) {
        message(
            "'t.df' must be a length 1 numeric. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (t.df <= 0) {
        message(
            "'t.df' must be a positive numeric value. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # dampingFactor
    #################
    if (!is.numeric(dampingFactor)) {
        message(
            "'dampingFactor' must be numeric between 0 and 1, none included. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(dampingFactor) > 1) {
        message(
            "'dampingFactor' must be a length 1 numeric. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (dampingFactor <= 0 | dampingFactor >= 1) {
        message(
            "'dampingFactor' must be numeric between 0 and 1, none included. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # layout
    ###########################
    if (!is.logical(layout)) {
        message(
            "'layout' must be logical. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(layout) > 1) {
        message(
            "'layout' must be a length 1 logical. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # thresholdConnectedComponent
    ###################################
    if (!is.numeric(thresholdConnectedComponent)) {
        message(
            "'thresholdConnectedComponent' must be ", 
            "numeric between 0 and 1. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(thresholdConnectedComponent) > 1) {
        message(
            "'thresholdConnectedComponent' must be a length 1 numeric. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (thresholdConnectedComponent < 0 | 
        thresholdConnectedComponent > 1) {
        message(
            "'thresholdConnectedComponent' must be numeric between 0 and 1.", 
            " Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # GOterm
    ##################################
    if (!is.null(GOterm) & 
        !is.character(GOterm)) {
        message(
            "'GOterm' must be NULL or a GO entry. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(GOterm) > 1) {
        message(
            "'GOterm' must be a length 1 character. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # GONamesAsLabels
    ################################
    if (!is.logical(GONamesAsLabels)) {
        message(
            "'GONamesAsLabels' must be logical. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(GONamesAsLabels) > 1) {
        message(
            "'GONamesAsLabels' must be a length 1 logical. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # LabelLengthAtPlot
    ##################################
    if (!is.numeric(LabelLengthAtPlot)) {
        message(
            "'LabelLengthAtPlot' must be numeric between 10 and 50. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(LabelLengthAtPlot) > 1) {
        message(
            "'LabelLengthAtPlot' must be a length 1 numeric. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (LabelLengthAtPlot < 10 | LabelLengthAtPlot > 100) {
        message(
            "'LabelLengthAtPlot' must be numeric between 10 and 100. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # object
    ##################################
    if (!is.FELLA.USER(object)) {
        message(
            "'object' is not a FELLA.USER object. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    } 
    
    # data
    ##################################
    if (!is.FELLA.DATA(data)) {
        message(
            "'data' is not a FELLA.DATA object. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    return(list(valid = TRUE))
}