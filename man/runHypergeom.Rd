\name{runHypergeom}
\alias{runHypergeom}
\title{Pathway enrichment using hypergeometric test}
\description{
Function \code{runHypergeom} performs the over representation analysis through
the hypergeometric test on a FELLA.USER object. If a custom background was specified,
it will be used. By default, this test has smaller background than \link{runDiffusion} 
and \link{runPagerank}.
}
\usage{

runHypergeom(object = NULL, 
             data = "FELLA.DATA")

}

\arguments{
  \item{object}{
  FELLA.USER object containing the compounds and background data.
  }
  \item{data}{
  Character, name of FELLA.DATA object that has at least the hypergeometric matrix loaded.
  }
}

\value{
A FELLA.USER object, whose p-values for hypergeometric test are available.
}

\seealso{
  \code{\link{runDiffusion}} \code{\link{runPagerank}}}

\examples{

runHypergeom(object = myObject)

runHypergeom(object = myObject, 
             data = "myData")
                  
}


\author{Sergi Picart,
 \email{sergi.picart@upc.edu}}
