#' @title The FELLA package
#' 
#' @description 
#' \code{FELLA} is a metabolomics data enrichment tool that contextualises 
#' a list of metabolites using KEGG reactions, 
#' enzymes, modules and pathways [Picart-Armada, 2017]. 
#' 
#' @details
#' \code{FELLA} can build knowledge models for the desired organism 
#' from the KEGG database [Kanehisa, 2017]. 
#' Once a model is ready, the input for the enrichment is introduced 
#' as a list of affected metabolites (as KEGG IDs). 
#' The output contains a comprehensive biological network layout 
#' that relates relevant pathways to the affected metabolites. 
#' Results are available in network and tabular format. 
#' 
#' \code{FELLA} is equipped with a simple graphical interface 
#' for the lay user, deployed through \link{launchApp}.
#' 
#' \code{FELLA} relies mainly on the following packages: 
#' KEGGREST for the queries 
#' to the KEGG server [Tenenbaum, 2013], 
#' \pkg{igraph} for the 
#' network support [Csardi, 2006] and 
#' \pkg{shiny} for the 
#' graphical user interface [Chang, 2017].
#' 
#' @examples 
#' ## Walkthrough
#' browseVignettes("FELLA")
#' ## I: create database
#' ?buildGraphFromKEGGREST
#' ## II: enrich data
#' ?enrich
#' ## III: export results
#' ?exportResults
#' 
#' @references 
#' Methodology:
#' 
#' Picart-Armada, S., Fernandez-Albert, F., Vinaixa, 
#' M., Rodriguez, M. A., Aivio, S., Stracker, 
#' T. H., Yanes, O., & Perera-Lluna, A. (2017). 
#' Null diffusion-based enrichment for metabolomics data. 
#' PLOS ONE, 12(12), e0189012.
#' 
#' 
#' Database: 
#' 
#' Kanehisa, M., Furumichi, M., Tanabe,
#' M., Sato, Y., & Morishima, K. (2017).
#' KEGG: new perspectives on genomes, pathways, diseases and drugs.
#' Nucleic acids research, 45(D1), D353-D361.
#' 
#' 
#' Main dependencies:
#' 
#' Tenenbaum, D. (2013). KEGGREST: Client-side REST access 
#' to KEGG. R package version, 1(1).
#' 
#' Csardi, G., & Nepusz, T. (2006). The igraph software package 
#' for complex network research. 
#' InterJournal, Complex Systems, 1695(5), 1-9.
#' 
#' Chang, W., Cheng, J., Allaire, JJ., 
#' Xie, Y., & McPherson, J. (2017).
#' shiny: Web Application Framework for R. R package version 1.0.5.
#' https://CRAN.R-project.org/package=shiny
#' 
#' 
#' @docType package
#' @name FELLA
NULL