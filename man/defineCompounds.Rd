\name{defineCompounds}
\alias{defineCompounds}
\title{Define a set of compounds to enrich}
\description{
Function \code{defineCompounds} reads the specified compounds, typically coming 
from a case-control Metabolomics experiment. It also allows to specify a custom 
compound background for the statistical tests. 
}
\usage{

defineCompounds(compounds = NULL, 
                compoundsBackground = NULL, 
                data = NULL)

}

\arguments{
  \item{compounds}{
  Character vector containing the compounds which were proven to be affected, 
  typically through a previous statistical test.
  }
  \item{compoundsBackground}{
  Optional character vector. This serves as a background comparison for p-values 
  calculation and can be set to the laboratory platform detection range.
  }
  \item{data}{
  Character that specifies the FELLA.DATA object for the enrichment. If NULL, 
  the default name will be used.
  }
}

\value{
A FELLA.USER object, ready for the enrichment analysis.
}

\seealso{
  \code{\link{buildGraphFromKEGG}} \code{\link{defineCompounds}}}

\examples{

defineCompounds(compounds = c("C00001", "C00031"))

defineCompounds(compounds = "C00031", 
                compoundsBackground = c("C00001", "C00031"))
}


\author{Sergi Picart,
 \email{sergi.picart@upc.edu}}
