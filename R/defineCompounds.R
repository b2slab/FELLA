#' @description 
#' Function \code{defineCompounds} creates a 
#' \code{\link{FELLA.USER}} object from a list of 
#' compounds and a \code{\link{FELLA.DATA}} object.
#' 
#' @details
#' Function \code{defineCompounds} maps the 
#' specficied list of KEGG compounds [Kanehisa, 2017], usually from an 
#' experimental metabolomics study, to the graph contained in the
#' \code{\link{FELLA.DATA}} object. 
#' Importantly, the names must be KEGG ids, so other formats 
#' (common names, HMDB ids, etc) must be mapped to KEGG first. 
#' For example, through the "Compound ID Conversion" 
#' tool in MetaboAnalyst [Xia, 2015].
#' The user can also define a personalised background as a 
#' list of KEGG compound ids, which should be more extensive than 
#' the list of input metabolites. 
#' Once the compounds are mapped, the enrichment 
#' can be performed through \code{runHypergeom}, 
#' \code{runDiffusion} and \code{runPagerank}.
#'
#' @inheritParams .params
#'
#' @return \code{defineCompounds} returns 
#' the \code{\link{FELLA.USER}} object 
#' with the mapped metabolites, ready to be enriched.
#' 
#' @name enrich-funs
#' @rdname enrich-funs 
#' 
#' @export
defineCompounds <- function(
    compounds = NULL, 
    compoundsBackground = NULL, 
    data = NULL) {
    
    FELLA.USER <- new("FELLA.USER")
    
    # Checking the input
    ################################
    # Load the FELLA.DATA object (required right now)
    if (!is.FELLA.DATA(data)) {
        stop("'data' is not a FELLA.DATA object")
    } else if (getStatus(data) != "loaded"){
        stop("'data' points to an empty FELLA.DATA object")
    }
    ################################
    
    # Optional: load the background of compounds for permutation calculation
    if (length(compoundsBackground) == 0) {
        message(
            "No background compounds specified. ", 
            "Default background will be used.")
        compoundsBackground <- getCom(data, "compound")
    } else {
        compoundsBackground <- as.character(compoundsBackground)
        compoundsBackground <- intersect(
            compoundsBackground, 
            getCom(data, "compound"))
        
        if (length(compoundsBackground) < 10) {
            warning(
                "Less than ten of the specified background compounds", 
                "appear in the available KEGG data. ", 
                "Default background will be used instead.")
            compoundsBackground <- getCom(data, "compound")
        } else {
            FELLA.USER@userinput@metabolitesbackground <- compoundsBackground
        }
    }
    
    # Load affected metabolites for the enrichment
    if (length(compounds) == 0) stop("Argument 'compounds' cannot be empty.")
    # Repeated compounds will be accounted once only
    compounds <- unique(as.character(compounds))
    compoundsOld <- compounds
    
    # Check whether any of the compounds 
    # in the input are not in the background...
    
    compoundsInBackground <- intersect(compoundsBackground, compounds)
    if (length(compoundsInBackground) < length(compounds)) {
        warning(
            "Some compounds were introduced as affected ", 
            "but they do not belong to the background. ", 
            "These compounds will be excluded from the analysis. ", 
            "Use 'getExcluded' to see them.")
        compounds <- compoundsInBackground
    }
    
    if (length(compounds) == 0) {
        stop(
            "None of the specified compounds ", 
            "appear in the available KEGG data.")
    } else {
        FELLA.USER@userinput@metabolites <- compounds
    }
    
    FELLA.USER@userinput@excluded <- setdiff(compoundsOld, compounds)
    
    return(FELLA.USER)
}
