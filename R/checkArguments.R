#' Internal function to check arguments and give personalised errors
#' 
#' This function checks if the arguments are of the desired type, length 
#' and range. If it fails, it gives an error explaining why the argument 
#' is invalid. 
#'
#' @inheritParams .params
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
#' attach(environment(FELLA:::checkArguments))
#' arg1 <- checkArguments(method = "hello")
#' arg1$valid
#' arg2 <- checkArguments(method = "diffusion")
#' arg2$valid
checkArguments <- function(
    method = "diffusion", 
    approx = "normality", 
    loadMatrix = NULL, 
    threshold = 0.05, 
    plimit = 15, 
    nlimit = 250, 
    niter = 1e3, 
    layout = FALSE, 
    splitByConnectedComponent = FALSE, 
    askPlots = TRUE, 
    thresholdConnectedComponent = 0.05, 
    GOterm = NULL,
    GONamesAsLabels = TRUE, 
    LabelLengthAtPlot = 22, 
    object = new("FELLA.USER"), 
    data = new("FELLA.DATA")) {
    
    # METHOD
    ###########################
    if (!is.character(method)) {
        message(
            "'method' must be a character: ", 
            "'hypergeom', 'diffusion', 'pagerank' or 'all'. ", 
            "Returning original 'object'...")
        return(list(ans = object, valid = FALSE))
    }
    
    if (length(method) > 1) {
        message(
            "'method' must be a length 1 character. ", 
            "Returning original 'object'...")
        return(list(ans = object, valid = FALSE))
    }
    
    if (!(method %in% c("hypergeom", "diffusion", "pagerank", "all"))) {
        message(
            "'method' must be a character: ", 
            "'hypergeom', 'diffusion', 'pagerank' or 'all'. ", 
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
    
    if (!(approx %in% c("simulation", "normality"))) {
        message(
            "'approx' must be a character: ", 
            "'simulation',  'normality', 'gamma' or 't'. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # loadMatrix
    #################
    if (!is.null(loadMatrix) & length(loadMatrix) > 1)  {
        message(
            "'loadMatrix' can only be a length 1 character ", 
            "('diffusion', 'pagerank', 'all') or NULL.")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (!is.null(loadMatrix) & !is.character(loadMatrix)) {
        message(
            "'loadMatrix' can only be a length 1 character ", 
            "('diffusion', 'pagerank', 'all') or NULL.")
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
    
    # splitByConnectedComponent
    ############################
    if (!is.logical(splitByConnectedComponent)) {
        message(
            "'splitByConnectedComponent' must be logical. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(splitByConnectedComponent) > 1) {
        message(
            "'splitByConnectedComponent' must be a length 1 logical. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    # askPlots
    ############################
    if (!is.logical(askPlots)) {
        message(
            "'askPlots' must be logical. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(askPlots) > 1) {
        message(
            "'askPlots' must be a length 1 logical. ", 
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
    
    if (thresholdConnectedComponent <= 0 | 
        thresholdConnectedComponent >= 1) {
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
    
    # splitByConnectedComponent
    ###############################
    if (!is.logical(splitByConnectedComponent)) {
        message(
            "'splitByConnectedComponent' must be logical. ", 
            "Returning NULL...")
        return(list(ans = NULL, valid = FALSE))
    }
    
    if (length(splitByConnectedComponent) > 1) {
        message(
            "'splitByConnectedComponent' must be a length 1 logical. ", 
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