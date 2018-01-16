#' @include defineCompounds.R
#' 
#' @details
#' Function \code{runHypergeom} performs an over representation analysis 
#' through the hypergeometric test [Fisher, 1935] on a 
#' \code{\link{FELLA.USER}} object with mapped metabolites 
#' and a \code{\link{FELLA.DATA}} object. 
#' If a custom background was specified, it will be used. 
#' This approach is included for completeness and it is not the 
#' main purpose behind the \code{\link{FELLA}} package. 
#' Importantly, \code{runHypergeom} is not a hypergeometric test using the 
#' original KEGG pathways. 
#' Instead, a compound "belongs" to a "pathway" if 
#' it can reach the original pathway in the 
#' upwards-directed KEGG graph. 
#' This is a way to evaluate enrichment including indirect connections 
#' to a pathway, e.g. through an enzymatic family. 
#' New "pathways" are expected to be larger than the original pathways
#' in this analysis and therefore the results can differ from the 
#' standard over representation.
#'
#' @inheritParams .params
#'
#' @return \code{runHypergeom} returns a 
#' \code{\link{FELLA.USER}} object 
#' updated with the hypergeometric test results
#' 
#' @rdname enrich-funs 
#' 
#' @importFrom stats phyper p.adjust
#' @import Matrix
#' @export
runHypergeom <- function(
    object = NULL, 
    data = NULL, 
    p.adjust = "fdr") {
    
    message("Running hypergeom...")
    
    # Checking the input
    ###############################
    if (!is.FELLA.USER(object)) {
        message(
            "'object' is not a FELLA.USER object. ", 
            "Returning NULL...")
        return(invisible())
    } 
    if (!is.FELLA.DATA(data)) {
        message(
            "'data' is not a FELLA.DATA object. ", 
            "Returning NULL...")
        return(invisible())
    }
    
    if (getStatus(data) != "loaded"){
        message(
            "'data' points to an empty FELLA.DATA object! ", 
            "Returning original 'object'...")
        return(object)
    }
    #############################
    
    message("Starting hypergeometric p-values calculation...")
    
    # The matrix
    if (prod(dim(getMatrix(data, "hypergeom"))) == 1) {
        message(
            "Hypergeometric test failed ", 
            "because its matrix is not loaded.")
        return(object)
    }
    hypergeom.matrix <- getMatrix(data, "hypergeom")
    metabolites.input <- getInput(object)
    metabolites.input.intersect <- intersect(
        metabolites.input, 
        rownames(hypergeom.matrix))
    metabolites.background.intersect <- intersect(
        getBackground(object), 
        rownames(hypergeom.matrix))
    
    # Metabolites in the input
    if (length(metabolites.input) == 0) {
        message(
            "Hypergeometric test failed because ", 
            "there are no compounds in the input.")
        return(object)
    } else if (length(metabolites.input.intersect) == 0) {
        message(
            "None of the compounds were in the ", 
            "hypergeometric test background.")
        return(object)
    } else if (
        length(metabolites.input.intersect) < length(metabolites.input)) {
        message(
            "Some of the compounds have been excluded ", 
            "as they are not in the hypergeometric test background.")
        message(
            "Amount decreased from ", length(metabolites.input), 
            " to ", length(metabolites.input.intersect))
        metabolites.input <- metabolites.input.intersect
    }
    
    # Background metabolites (if supplied) resize the matrix
    if (length(metabolites.background.intersect) > 0) {
        hypergeom.matrix <- hypergeom.matrix[
            metabolites.background.intersect, ]
    }
    #   hypergeom.matrix <- hypergeom.matrix[, colSums(hypergeom.matrix) > 0]
    row_comp <- which(rownames(hypergeom.matrix) %in% metabolites.input)
    
    pvalues.path <- vector("double", length = dim(hypergeom.matrix)[2])
    names(pvalues.path) <- colnames(hypergeom.matrix)
    
    # Additional variables for the report
    pathbackground <- pvalues.path
    pathhits <- pvalues.path
    
    # p-values calculation
    for (path in seq_len(ncol(hypergeom.matrix))) {
        sample_success <- sum(hypergeom.matrix[row_comp, path])
        pathhits[path] <- sample_success
        
        # sum over row or column (current)?
        total_success <- sum(hypergeom.matrix[, path])
        pathbackground[path] <- total_success
        
        total_failure <- dim(hypergeom.matrix)[1] - total_success
        sample_size <- length(row_comp)
        
        pvalues.path[path] <- stats::phyper(
            sample_success - 1, 
            total_success, 
            total_failure, 
            sample_size, 
            lower.tail = FALSE)
    }
    
    object@hypergeom@pvalues <- stats::p.adjust(pvalues.path, method = p.adjust)
    object@hypergeom@nbackground <- dim(hypergeom.matrix)[1]
    object@hypergeom@ninput <- length(metabolites.input.intersect)
    object@hypergeom@pathhits <- pathhits
    object@hypergeom@pathbackground <- pathbackground
    
    message("Done.")
    
    object@hypergeom@valid <- TRUE
    return(object)
}