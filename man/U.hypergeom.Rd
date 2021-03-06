% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{U.hypergeom-class}
\alias{U.hypergeom-class}
\title{An internal S4 class for the user data 
of the hypergeometric over representation 
analysis}
\description{
An internal S4 class for the user data 
of the hypergeometric over representation 
analysis
}
\section{Slots}{

\describe{
\item{\code{valid}}{Logical value; is the analysis valid?}

\item{\code{pvalues}}{Named numeric vector with p-values}

\item{\code{pathhits}}{Numeric named vector with 
the quantities "sample_success" 
for the hypergeometric distribution (#affected in path)}

\item{\code{pathbackground}}{Numeric named vector with 
the quantities "total_success" 
for the hypergeometric distribution (total in path)}

\item{\code{nbackground}}{Numeric value, number of compoudns in the background. 
Equivalently, number of rows for the hypergeometric binary matrix}

\item{\code{ninput}}{Numeric value, number of affected compounds matched to the 
rownames}
}}

