#' @param object FELLA.USER object
.object <- function() NULL

#' @param data FELLA.DATA object
.data <- function() NULL

#' @param type Character, one of "hypergeom", "diffusion" or "pagerank"
.type <- function() NULL

#' @param level Desired level, can be coded as a number or a character: 
#' 1 or "pathway"; 2 or "module"; 3 or "enzyme"; 
#' 4 or "reaction"; 5 or "compound".
.level <- function() NULL

#' @param method Character, one of: 
#' "hypergeom", "diffusion", "pagerank" or "all"
.method <- function() NULL

#' @param method Character, one of: 
#' "hypergeom", "diffusion" or "pagerank"
.methodSingle <- function() NULL

#' @param approx Character: "simulation" for Monte Carlo, "normality", 
#' "gamma" or "t" for parametric approaches
.approx <- function() NULL

#' @param loadMatrix Character to choose if heavy matrices should be loaded. 
#' One of: "diffusion", "pagerank", "all" or NULL
.loadMatrix <- function() NULL

#' @param threshold Numeric value between 0 and 1. 
#' Applied when filtering KEGG nodes
.threshold <- function() NULL

#' @param thresholdConnectedComponent Numeric value between 0 and 1. 
#' Connected components that are below the threshold are kept, 
#' while the ones exceeding it (because they are too small) are discarded. 
.thresholdConnectedComponent <- function() NULL

#' @param plimit Pathway limit, must be a numeric value between 1 and 50
.plimit <- function() NULL

#' @param nlimit Node limit, must be a numeric value between 1 and 1000. 
#' This limits the order of the solution graph
.nlimit <- function() NULL

#' @param niter Number of iterations (permutations) 
#' for Monte Carlo ("simulation"), 
#' must be a numeric value between 1e2 and 1e5
.niter <- function() NULL

#' @param layout Logical, should the plot be returned as a layout?
.layout <- function() NULL

#' @param splitByConnectedComponent Logical, 
#' should the solution be split by CC?
.splitByConnectedComponent <- function() NULL

#' @param askPlots Logical, should R ask 
#' for the next plots using hit<ENTER>?
.askPlots <- function() NULL

#' @param thresholdConnectedComponent Numeric value between 0 and 1, 
#' refers to the threshold to keep a CC regarding its size
.thresholdConnectedComponent <- function() NULL

#' @param GO.CellularComponent Character, GO entry to draw 
#' semantic similarity in the solution graph. 
#' If no similarity is desired, leave it as NULL
.GO.CellularComponent <- function() NULL

#' @param GONamesAsLabels Logical, should GO names 
#' be displayed as labels instead of GO identifiers?
.GONamesAsLabels <- function() NULL

#' @param LabelLengthAtPlot Numeric value between 10 and 50. 
#' Maximum length that a label can reach when plotting the graph. 
#' The remaining characters will be truncated using "..."
.LabelLengthAtPlot <- function() NULL

# Other
#' @param p.adjust Character passed to the 
#' \code{\link[stats]{p.adjust}} method
.p.adjust <- function() NULL

#' @param t.df Numeric value; number of degrees of freedom 
#' of the t distribution 
#' if the approximation \code{approx = "t"} is used
.t.df <- function() NULL

#' @param format Character passed to the 
#' \code{\link[stats]{p.adjust}} method
.format <- function() NULL

#' @param compounds Character vector containing the 
#' KEGG IDs of the compounds considered as affected
.compounds <- function() NULL

#' @param compoundsBackground Character vector containing the KEGG IDs of 
#' the compounds that belong to the background. Can be \code{NULL} for the 
#' default background (all compounds)
.compoundsBackground <- function() NULL

#' @param NamesAsLabels Logical, should KEGG names be displayed 
#' as labels instead of KEGG identifiers?
.NamesAsLabels <- function() NULL

# Dev options
#' @param BIMODAL Logical value, should the bimodal test 
#' for compounds be used?
.BIMODAL <- function() NULL