\name{runDiffusion}
\alias{runDiffusion}
\title{Pathway enrichment using heat diffusion}
\description{
Function \code{runDiffusion} performs the diffusion-based enrichment on a 
FELLA.USER object. If a custom background was specified, it will be used. 
This procedure gives statistical significance measures for each node and allows 
the extraction of a subgraph according to a fixed threshold.
}
\usage{

runDiffusion(object = NULL, 
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
  input. In the latter case, the diffusion matrix will be used if available.
  Keep in mind that the output may slightly vary upon calling this function 
  several times.
  }
  \item{niter}{
  Number of random trials to perform, if \code{method} is set to \code{'simulation'}.
  }
}

\value{
A FELLA.USER object, whose p-values for heat diffusion are available.
}

\seealso{
  \code{\link{runHypergeom}} \code{\link{runPagerank}}}

\examples{

runDiffusion(object = myObject, 
              method = 'normality')

runDiffusion(object = myObject, 
              method = 'simulation', 
              niter = 2e3)
                  
}


\author{Sergi Picart,
 \email{sergi.picart@upc.edu}}
