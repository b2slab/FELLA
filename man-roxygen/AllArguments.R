#' @param object FELLA.USER object
#' @param data FELLA.DATA object
#' @param type Character vector, containing entries in
#' "hypergeom", "diffusion" or "pagerank"
#' @param level Desired level, can be coded as a number or a character: 
#' 1 or "pathway"; 2 or "module"; 3 or "enzyme"; 
#' 4 or "reaction"; 5 or "compound".
#' @param method. Character, exactly one of: 
#' \code{"hypergeom"}, \code{"diffusion"}, \code{"pagerank"}
#' @param method Character vector, containing some of: 
#' \code{"hypergeom"}, \code{"diffusion"}, \code{"pagerank"}
#' @param approx Character: "simulation" for Monte Carlo, "normality", 
#' "gamma" or "t" for parametric approaches
#' @param loadMatrix Character vector to choose if 
#' heavy matrices should be loaded. 
#' Can contain: \code{"diffusion"}, \code{"pagerank"}
#' @param threshold Numeric value between 0 and 1. 
#' Applied when filtering KEGG nodes
#' @param thresholdConnectedComponent Numeric value between 0 and 1. 
#' Connected components that are below the threshold are kept, 
#' while the ones exceeding it (because they are too small) are discarded. 
#' @param plimit Pathway limit, must be a numeric value between 1 and 50
#' @param nlimit Node limit, must be a numeric value between 1 and 1000. 
#' This limits the order of the solution graph
#' @param niter Number of iterations (permutations) 
#' for Monte Carlo ("simulation"), 
#' must be a numeric value between 1e2 and 1e5
#' @param layout Logical, should the plot be returned as a layout?
#' @param splitByConnectedComponent Logical, 
#' should the solution be split by CC?
#' @param askPlots Logical, should R ask 
#' for the next plots using hit<ENTER>?
#' @param thresholdConnectedComponent Numeric value between 0 and 1, 
#' refers to the threshold to keep a CC regarding its size
#' @param GOterm Character, GO entry to draw 
#' semantic similarity in the solution graph. 
#' @param GONamesAsLabels Logical, should GO names 
#' be displayed as labels instead of GO identifiers?
#' @param LabelLengthAtPlot Numeric value between 10 and 50. 
#' Maximum length that a label can reach when plotting the graph. 
#' The remaining characters will be truncated using "..."
# Other
#' @param p.adjust Character passed to the 
#' \code{\link[stats]{p.adjust}} method
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