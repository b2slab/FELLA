#' This function gives statistically normalised diffusion scores
#' for each node and allows 
#' the extraction of a subgraph according to a fixed threshold.
#' The p-scores are in the range [0,1], where lower scores are better, and 
#' can be computed using two kinds of approximations: 
#' the stochastic Monte Carlo trials (\code{"simulation"}) and the parametric 
#' scores (\code{"normality"}, \code{"gamma"}, \code{"t"}). 
#' Using \code{"simulation"} will perform random resampling of the 
#' input and will compute a score between 0 and 1 based on an empirical 
#' p-value calculation. 
#' On the other hand, the parametric approaches use the theoretical
#' first and second order statistics of the permuted distributions to 
#' normalise the scores. In particular, \code{"normality"} computes a 
#' z-score and maps to the [0,1] scale through \code{\link[stats]{pnorm}}. 
#' \code{"t"} does the same but using \code{\link[stats]{pt}} instead.
#' \code{"gamma"} uses the mean and variance to define the shape 
#' (E^2/V) and scale (V/E) of the gamma distribution, and 
#' \code{\link[stats]{pgamma}} to map to [0,1].
