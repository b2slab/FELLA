#' @import methods
#' @import igraph
methods::setClass("igraph")

#' An internal S4 class to represent the KEGG graph and related files
#'
#' @slot graph KEGG graph
#' @slot id2name Mapping list: KEGG ID to KEGG name 
#' (can contain multiple hits)
#' @slot pvalues.size Numeric matrix for the 
#' evaluation of CC through their size
#' @slot id List with character vectors for KEGG categories
#' @slot status Character that specifies the 
#' current status of this S4 class
#' 
#' @rdname D.keggdata
setClass(
    "D.keggdata",
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

#' An internal S4 class for the binary matrix (hypergeometric test)
#'
#' @slot matrix Binary sparse matrix
#' 
#' @rdname D.hypergeom
setClass(
    "D.hypergeom",
    representation(
        matrix = "Matrix"
    ),
    prototype(
        matrix = Matrix::Matrix()
    )
)

#' An internal S4 class for the diffusion data
#'
#' @slot matrix Numeric (dense) matrix [optional]
#' @slot rowSums Numeric named vector with rowSums internal data
#' @slot squaredRowSums Numeric named vector with 
#' squaredRowSums internal data
#' 
#' @rdname D.diffusion
setClass(
    "D.diffusion",
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

#' An internal S4 class for the PageRank data
#'
#' @slot matrix Numeric (dense) matrix [optional]
#' @slot rowSums Numeric named vector with rowSums internal data
#' @slot squaredRowSums Numeric named vector with 
#' squaredRowSums internal data
#' 
#' @rdname D.pagerank
setClass(
    "D.pagerank",
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

#' An S4 class to represent all the necessary KEGG data
#'
#' @slot keggdata A \code{D.keggdata} S4 object
#' @slot hypergeom A \code{D.hypergeom} S4 object
#' @slot diffusion A \code{D.diffusion} S4 object
#' @slot pagerank A \code{D.pagerank} S4 object
#' 
#' @aliases FELLA.DATA
#' @rdname FELLA.DATA
#' @exportClass FELLA.DATA
setClass(
    "FELLA.DATA",
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

#' An internal S4 class for the user input data
#'
#' @slot metabolites Character vector containing 
#' the affected compounds
#' @slot metabolitesbackground Character vector containing 
#' the compounds for the personalised background. 
#' Optionally, can be \code{NULL} for default background
#' @slot excluded Character vector containing 
#' the compounds that have been 
#' excluded because they cannot be mapped to KEGG graph compounds
#' 
#' @rdname U.userinput
setClass(
    "U.userinput",
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

#' An internal S4 class for the user data 
#' of the hypergeometric over representation 
#' analysis
#'
#' @slot valid Logical value; is the analysis valid?
#' @slot pvalues Named numeric vector with p-values
#' @slot pathhits Numeric named vector with 
#' the quantities "sample_success" 
#' for the hypergeometric distribution (#affected in path)
#' @slot pathbackground Numeric named vector with 
#' the quantities "total_success" 
#' for the hypergeometric distribution (total in path)
#' @slot nbackground Numeric value, number of compoudns in the background. 
#' Equivalently, number of rows for the hypergeometric binary matrix
#' @slot ninput Numeric value, number of affected compounds matched to the 
#' rownames
#' 
#' @rdname U.hypergeom
setClass(
    "U.hypergeom",
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

#' An internal S4 class for the user data 
#' of the diffusion enrichment analysis
#'
#' @slot valid Logical value; is the analysis valid?
#' @slot pscores Named numeric vector with p-scores
#' @slot approx Character; which approximation was used? 
#' Can be "simulation" for Monte Carlo; 
#' "normality", "gamma" or "t" for parametric approaches
#' @slot niter Numeric value, number of iterations 
#' for the simulated approach
#' 
#' @rdname U.diffusion
setClass(
    "U.diffusion",
    representation(
        valid = "logical",
        pscores = "vector",
        approx = "character", 
        niter = "numeric"
    ),
    prototype(
        valid = NA, 
        pscores = vector(),
        approx = character(), 
        niter = numeric()
    )
)

#' An internal S4 class for the user data 
#' of the PageRank enrichment analysis
#'
#' @slot valid Logical value; is the analysis valid?
#' @slot pscores Named numeric vector with p-scores
#' @slot approx Character; which approximation was used? 
#' Can be "simulation" for Monte Carlo; 
#' "normality", "gamma" or "t" for parametric approaches
#' @slot niter Numeric value, number of iterations 
#' for the simulated approach
#' 
#' @rdname U.pagerank
setClass(
    "U.pagerank",
    representation(
        valid = "logical", 
        pscores = "vector",
        approx = "character", 
        niter = "numeric"
    ),
    prototype(
        valid = NA, 
        pscores = vector(),
        approx = character(), 
        niter = numeric()
    )
)

#' An S4 class to save all the user analysis data
#'
#' @slot userinput A \code{U.userinput} S4 object
#' @slot hypergeom A \code{U.hypergeom} S4 object
#' @slot diffusion A \code{U.diffusion} S4 object
#' @slot pagerank A \code{U.pagerank} S4 object
#' 
#' @aliases FELLA.USER
#' @rdname FELLA.USER
#' @exportClass FELLA.USER
setClass(
    "FELLA.USER",
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
