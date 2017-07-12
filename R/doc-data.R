#' @name FELLA.sample
#' @title FELLA.DATA sample data
#' @description This \code{\link[FELLA]{FELLA.DATA}} object is 
#' a small KEGG graph object. 
#' Despite being a small database that only contains 
#' the two metabolic pathways 
#' hsa00010 - Glycolysis / Gluconeogenesis, and hsa00640 - 
#' Propanoate metabolism, 
#' it is useful to play around with \code{FELLA}'s 
#' functions. It is also used for internal testing of this package. 
#' @docType data
#' @usage data(FELLA.sample)
#' @examples data(FELLA.sample)
#' @return A \code{\link[FELLA]{FELLA.DATA}} object
#' @source Generated from a mid-2017 KEGG release 
#' (\url{http://www.genome.jp/kegg/})
"FELLA.sample"

#' @name input.sample
#' @title A randomly generated list of affected metabolites
#' @description This \code{character} vector object has been 
#' generated using the 
#' sample data in the object \code{FELLA.sample}. 
#' The KEGG compounds have been chosen 
#' with preference for the hsa00640 pathway, 
#' so that the enrichment results choose 
#' pathway hsa00640 over hsa00010. 
#' @docType data
#' @usage data(input.sample)
#' @examples data(input.sample)
#' @return A \code{character} vector containing 30 KEGG IDs
#' @source Generated from a mid-2017 KEGG release 
#' (\url{http://www.genome.jp/kegg/})
"input.sample"