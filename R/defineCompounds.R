#' Define the list of input compounds
#' 
#' This function maps the specficied list of compounds, usually from an 
#' experimental metabolomics study, in the graph contained in the
#' \code{\link[FELLA]{FELLA.DATA}} object. 
#' Once the compounds are mapped, the enrichment 
#' can be performed. There is also the option 
#' to define a personalised background.
#'
#' @inheritParams .compounds
#' @inheritParams .compoundsBackground
#' @inheritParams .data
#'
#' @return The \code{\link[FELLA]{FELLA.USER}} object 
#' to perform the user analysis
#' 
#' @examples 
#' data(FELLA.sample)
#' obj <- defineCompounds(
#' compounds = c("C00010", "C00011", "C00000"), 
#' data = FELLA.sample)
#' obj
#' 
#' ## If no compounds are mapped an error is thrown
#' \dontrun{
#' data(FELLA.sample)
#' obj <- defineCompounds(
#' compounds = c("C00049", "C00050"), 
#' data = FELLA.sample)}
#' 
#' @export
defineCompounds <- function(
  compounds = NULL, 
  compoundsBackground = NULL, 
  data = NULL) {
  
  FELLA.USER <- new("FELLA.USER")
  
  # Checking the input
  ##############################################################################
  # Load the FELLA.DATA object (required right now)
  if (!is.FELLA.DATA(data)) {
    stop("'data' is not a FELLA.DATA object")
  } else if (data@keggdata@status != "loaded"){
    stop("'data' points to an empty FELLA.DATA object")
  }
  ##############################################################################
  
  # Optional: load the background of compounds for p-value calculation
  if (length(compoundsBackground) == 0) {
    message("No background compounds specified. ", 
            "Default background will be used.")
    compoundsBackground <- getCom(data, "compound")
  } else {
    compoundsBackground <- as.character(compoundsBackground)
    compoundsBackground <- intersect(compoundsBackground, 
                                     getCom(data, "compound"))
    
    if (length(compoundsBackground) < 10) {
      warning("Less than ten of the specified background compounds", 
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
    warning("Some compounds were introduced as affected ", 
            "but they do not belong to the background. ", 
            "These compounds will be excluded from the analysis. ", 
            "Use 'getExcluded' to see them.")
    compounds <- compoundsInBackground
  }
  
  if (length(compounds) == 0) {
    stop("None of the specified compounds appear in the available KEGG data.")
  } else {
    FELLA.USER@userinput@metabolites <- compounds
  }

  FELLA.USER@userinput@excluded <- setdiff(compoundsOld, compounds)
  
  return(FELLA.USER)
}
