\name{runPagerank}
\alias{runPagerank}
\title{Pathway enrichment using PageRank}
\description{
Function \code{runPagerank} performs the random walk based enrichment on a 
FELLA.USER object, making use of igraph function \code{page.rank}. If a custom 
background was specified, it will be used. This procedure gives statistical 
significance measures for each node and allows the extraction of a subgraph 
according to a fixed threshold.
}
\usage{

runPagerank(object = NULL, 
            data = "FELLA.DATA", 
            method = "simulation", 
            niter = 1000)

}

\arguments{
  \item{object}{
  FELLA.USER object containing the compounds and background data.
  }
  \item{data}{
  Character, name of FELLA.DATA object.
  }
  \item{method}{
  Character, approximation to be used for the p-values. 'normality' assumes that 
  the tails can be considered normal, 'simulation' will run random trials in the 
  input. In the latter case, the pagerank matrix will be used if available.
  Keep in mind that the output may slightly vary upon calling this function 
  several times.
  }
  \item{niter}{
  Number of random trials to perform, if \code{method} is set to \code{'simulation'}.
  }
}

\value{
A FELLA.USER object, whose p-values for PageRank are available.
}

\seealso{
  \code{\link{runHypergeom}} \code{\link{runDiffusion}}}

\examples{

runPagerank(object = myObject, 
              method = 'normality')

runPagerank(object = myObject, 
              method = 'simulation', 
              niter = 2e3)
                  
}


\author{Sergi Picart,
 \email{sergi.picart@upc.edu}}
