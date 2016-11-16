#' Pathway enrichment through hypergeometric test 
#' 
#' Function \code{runHypergeom} performs the over representation analysis through
#' the hypergeometric test on a \code{\link[FELLA]{FELLA.USER}} object. 
#' If a custom background was specified, it will be used. 
#' By default, this test has smaller background than \code{\link[FELLA]{runDiffusion}}
#' and \code{\link[FELLA]{runPagerank}}.
#'
#' @inheritParams .object
#' @inheritParams .data
#' @inheritParams .p.adjust
#'
#' @return The \code{\link[FELLA]{FELLA.USER}} object with the hypergeometric test results
#' 
#' @import Matrix
#' @export
runHypergeom <- function(object = NULL, 
                         data = NULL, 
                         p.adjust = "fdr") {

  message("Running hypergeom...")
  
  # Checking the input
  ##############################################################################
  if (!is.FELLA.USER(object)) {
    message("'object' is not a FELLA.USER object. Returning NULL...")
    return(invisible())
  } 
  if (!is.FELLA.DATA(data)) {
    message("'data' is not a FELLA.DATA object. Returning NULL...")
    return(invisible())
  }
  
  if (data@keggdata@status != "loaded"){
    message("'data' points to an empty FELLA.DATA object! Returning original 'object'...")
    return(object)
  }
  ##############################################################################
  
  message("Starting hypergeometric p-values calculation...")
  
  # The matrix
  if (prod(dim(getMatrix(data, "hypergeom"))) == 1) {
    message("Hypergeometric test failed because its matrix is not loaded.")
    return(object)
  }
  hypergeom.matrix <- getMatrix(data, "hypergeom")
  metabolites.input <- getInput(object)
  metabolites.input.intersect <- intersect(metabolites.input, 
                                           rownames(hypergeom.matrix))
  metabolites.background.intersect <- intersect(getBackground(object), 
                                                rownames(hypergeom.matrix))
    
  # Metabolites in the input
  if (length(metabolites.input) == 0) {
    message("Hypergeometric test failed because there are no compounds in the input.")
    return(object)
  } else if (length(metabolites.input.intersect) == 0) {
    message("None of the compounds were in the hypergeometric test background.")
    return(object)
  } else if (length(metabolites.input.intersect) < length(metabolites.input)) {
    message("Some of the compounds have been excluded as they are not in the hypergeometric test background.")
    message(paste0("Amount decreased from ", length(metabolites.input), 
                   " to ", length(metabolites.input.intersect)))
    metabolites.input <- metabolites.input.intersect
  }
  
  # Background metabolites (if supplied) resize the matrix
  if (length(metabolites.background.intersect) > 0) {
    hypergeom.matrix <- hypergeom.matrix[metabolites.background.intersect, ]
  }
  
#   hypergeom.matrix <- hypergeom.matrix[, colSums(hypergeom.matrix) > 0]
  row_comp <- which(rownames(hypergeom.matrix) %in% metabolites.input)
  
  
  pvalues.path <- vector("double", length = dim(hypergeom.matrix)[2])
  names(pvalues.path) <- colnames(hypergeom.matrix)
  
  # Additional variables for the report
  pathbackground <- pvalues.path
  pathhits <- pvalues.path

  # p-values calculation
  for (path in 1:dim(hypergeom.matrix)[2]) {
    sample_success <- sum(hypergeom.matrix[row_comp, path])
    pathhits[path] <- sample_success
    
    # sum over row or column (current)?
    total_success <- sum(hypergeom.matrix[, path])
    pathbackground[path] <- total_success
    
    total_failure <- dim(hypergeom.matrix)[1] - total_success
    sample_size <- length(row_comp)
    
    pvalues.path[path] <- phyper(sample_success - 1, 
                                 total_success, 
                                 total_failure, 
                                 sample_size, 
                                 lower.tail = F)
  }

  object@hypergeom@pvalues <- p.adjust(pvalues.path, method = p.adjust)
  object@hypergeom@nbackground <- dim(hypergeom.matrix)[1]
  object@hypergeom@ninput <- length(metabolites.input.intersect)
  object@hypergeom@pathhits <- pathhits
  object@hypergeom@pathbackground <- pathbackground
  
  message("Done.")


  object@hypergeom@valid <- T
  return(object)
}