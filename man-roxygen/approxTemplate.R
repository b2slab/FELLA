#' @details
#' There is an important detail for \code{"diffusion"} 
#' and \code{"pagerank"}: the scores are statistically normalised. 
#' Omitting this normalisation leads to a systematic bias, 
#' especially in pathway nodes, as described in [Picart-Armada, 2017]. 
#' 
#' Therefore, in both cases, scores undergo a normalisation 
#' through permutation analysis. 
#' The score of a node \code{i} is compared to its null distribution 
#' under input permutation, leading to their p-scores. 
#' As described in [Picart-Armada, 2017], two alternatives are offered: 
#' a parametric and deterministic approach 
#' and a non-parametric, stochastic one.
#' 
#' Stochastic Monte Carlo trials (\code{"simulation"}) imply 
#' randomly permuting the input \code{niter} times and counting, 
#' for each node \code{i}, how many trials 
#' led to an equally or more extreme value than the original score. 
#' An empirical p-value is returned [North, 2002].
#' 
#' On the other hand, the parametric 
#' scores (\code{approx = "normality"}) 
#' give a z-score for such permutation analysis. 
#' The expected value and variance of such null distributions 
#' are known quantities, see supplementary 
#' file S4 from [Picart-Armada, 2017].
#' To work in the same range \code{[0,1]}, z-scores are 
#' transformed using the routine \code{\link[stats:Normal]{pnorm}}. 
#' The user can also choose the Student's t using 
#' \code{approx = "t"} and choosing a number of degrees of freedom 
#' through \code{t.df}. 
#' This uses the function \code{\link[stats:TDist]{pt}} instead.
#' Alternatively, a gamma distribution can be used by setting 
#' \code{approx = "gamma"}. 
#' The theoretical mean (E) and variance (V) 
#' are used to define the shape 
#' (E^2/V) and scale (V/E) of the gamma distribution, and 
#' \code{\link[stats:GammaDist]{pgamma}} to map to [0,1].
#' 
#' Any sub-network prioritised by \code{"diffusion"} 
#' and \code{"pagerank"} is selected by applying 
#' a threshold on the p-scores.
