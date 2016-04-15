#' @importFrom igraph graph.empty

setClass("igraph")

setClass("D.keggdata",
         representation(
           graph = "igraph", 
           id2name = "list", 
           pvalues.size = "matrix", 
           id = "list", 
           status = "character"
         ),
         prototype(
           graph = igraph::graph.empty(), 
           id2name = list(), 
           pvalues.size = matrix(), 
           id = list(), 
           status = "empty"
         )
)

setClass("D.hypergeom",
         representation(
           matrix = "Matrix"
         ),
         prototype(
           matrix = Matrix::Matrix()
         )
)

setClass("D.diffusion",
         representation(
           matrix = "matrix", 
           rowSums = "vector", 
           squaredRowSums = "vector"
         ),
         prototype(
           matrix = matrix(), 
           rowSums = vector(), 
           squaredRowSums = vector()
         )
)

setClass("D.pagerank",
         representation(
           matrix = "matrix", 
           rowSums = "vector", 
           squaredRowSums = "vector"
         ),
         prototype(
           matrix = matrix(), 
           rowSums = vector(), 
           squaredRowSums = vector()
         )
)

setClass("FELLA.DATA",
         representation(
           keggdata = "D.keggdata",
           hypergeom = "D.hypergeom",
           diffusion = "D.diffusion",
           pagerank = "D.pagerank"
           ),
         prototype(
           keggdata = new("D.keggdata"),
           hypergeom = new("D.hypergeom"),
           diffusion = new("D.diffusion"),
           pagerank = new("D.pagerank")
           )
         )

setClass("U.userinput",
         representation(
           metabolites = "vector",
           metabolitesbackground = "vector", 
           excluded = "vector"
         ),
         prototype(
           metabolites = vector(),
           metabolitesbackground = vector(), 
           excluded = vector()
         )
)

setClass("U.hypergeom",
         representation(
           valid = "logical",
           pvalues = "vector",
           pathhits = "vector", 
           pathbackground = "vector", 
           nbackground = "numeric", 
           ninput = "numeric"
           ),
         prototype(
           valid = NA, 
           pvalues = vector(), 
           pathhits = vector(), 
           pathbackground = vector(), 
           nbackground = numeric(), 
           ninput = numeric()
           )
         )

setClass("U.diffusion",
         representation(
           valid = "logical",
           pvalues = "vector",
           approx = "character", 
           niter = "numeric", 
           used = "character"
           ),
         prototype(
           valid = NA, 
           pvalues = vector(),
           approx = character(), 
           niter = numeric(), 
           used = character()
           )
         )

setClass("U.pagerank",
         representation(
           valid = "logical", 
           pvalues = "vector",
           approx = "character", 
           niter = "numeric", 
           used = "character"
           ),
         prototype(
           valid = NA, 
           pvalues = vector(),
           approx = character(), 
           niter = numeric(), 
           used = character()
           )
         )

setClass("FELLA.USER",
         representation(
           userinput = "U.userinput",
           hypergeom = "U.hypergeom",
           diffusion = "U.diffusion",
           pagerank = "U.pagerank"
           ),
         prototype(
           userinput = new("U.userinput"),
           hypergeom = new("U.hypergeom"),
           diffusion = new("U.diffusion"),
           pagerank = new("U.pagerank")
           )
         )
