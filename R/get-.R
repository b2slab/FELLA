#' Get compounds in the defined background
#' 
#' Extractor function for the compounds defined as background
#'
#' @inheritParams .params
#'
#' @return Vector of compounds in the background. 
#' If this vector is empty, all 
#' the compounds are used as background by default.
#' 
#' @examples
#' data(FELLA.sample)
#' data(input.sample)
#' input <- head(input.sample, 12)
#' 
#' ## If the background is default, we see an empty vector 
#' ## Note that the number of iterations is really small in the example
#' obj <- enrich(
#' compounds = input, 
#' method = "diffusion", 
#' approx = "simulation", 
#' niter = 100, 
#' data = FELLA.sample)
#' 
#' getBackground(obj)
#' 
#' ## Otherwise we see the background compounds that mapped to the graph
#' obj <- enrich(
#' compounds = input, 
#' compoundsBackground = input.sample, 
#' method = "diffusion", 
#' approx = "simulation", 
#' niter = 100, 
#' data = FELLA.sample)
#' getBackground(obj)
#' 
#' @export
getBackground <- function(object) {
    return(object@userinput@metabolitesbackground)
}

#' Get community
#' 
#' Extractor function for all the nodes from a level/community of KEGG graph
#'
#' @inheritParams .params
#' @param format Format of the output, "name" returns KEGG IDs whereas 
#' "id" returns vertices IDs
#'
#' @return Vector of the names/ids of the desired KEGG graph community
#' 
#' @examples 
#' data(FELLA.sample)
#' ## Pathways
#' getCom(FELLA.sample, 1, format = "name")
#' getCom(FELLA.sample, 1, format = "id")
#' ## Modules
#' getCom(FELLA.sample, 2)
#' ## Enzymes
#' head(getCom(FELLA.sample, 3))
#' ## Reactions
#' head(getCom(FELLA.sample, 4))
#' ## Compounds
#' head(getCom(FELLA.sample, 5))
#' @export
getCom <- function(data, level, format = "name") {
    if (format == "name")
        return(names(data@keggdata@id[[level]]))
    
    if (format == "id")
        return(data@keggdata@id[[level]]) 
    
    stop("'format' must be 'name' or 'id'.")
}

#' Get excluded compounds
#' 
#' Extractor function for the compounds in the input 
#' that were not mapped to the KEGG graph
#'
#' @inheritParams .params
#'
#' @return Vector of the excluded compounds
#' 
#' @examples 
#' data(FELLA.sample)
#' data(input.sample)
#' 
#' ## No excluded compounds
#' obj <- defineCompounds(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' getExcluded(obj)
#' 
#' ## One compound does not map
#' ## The user gets a warning as well
#' obj <- defineCompounds(
#' compounds = c(input.sample, "intruder"), 
#' data = FELLA.sample)
#' getExcluded(obj)
#' @export
getExcluded <- function(object) {
    return(object@userinput@excluded)
}

#' Get KEGG graph
#' 
#' Extractor function for the KEGG graph from the FELLA.DATA object
#'
#' @inheritParams .params
#'
#' @return KEGG graph as an \pkg{igraph} object
#' @examples 
#' data(FELLA.sample)
#' g <- getGraph(FELLA.sample)
#' class(g)
#' @export
getGraph <- function(data) {
    return(data@keggdata@graph)
}

#' Get KEGG version info
#' 
#' Extractor function for the info about the KEGG version used to 
#' build the FELLA.DATA object
#'
#' @inheritParams .params
#'
#' @return Character containing the KEGG release details
#' @examples 
#' data(FELLA.sample)
#' getInfo(FELLA.sample)
#' 
#' @export
getInfo <- function(data) {
    return(comment(getGraph(data)))
}

#' Get metabolites in the input
#' 
#' Extractor function for the metabolites 
#' specified by the user in the input
#'
#' @inheritParams .params
#'
#' @return Vector of metabolites in the input
#' 
#' @examples
#' data(FELLA.sample)
#' data(input.sample)
#' 
#' ## No excluded compounds: the input is recovered as is
#' obj <- defineCompounds(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' i1 <- getInput(obj)
#' 
#' ## One compound does not map: the input contains only the mapped entities
#' obj <- defineCompounds(
#' compounds = c(input.sample, "intruder"), 
#' data = FELLA.sample)
#' i2 <- getInput(obj)
#' 
#' identical(sort(i1), sort(i2))
#' @export
getInput <- function(object) {
    return(object@userinput@metabolites)
}

#' Get matrix for the desired methodology
#' 
#' Extractor function for the matrices of 
#' hypergeometric, diffusion and PageRank methodologies
#'
#' @inheritParams .params
#'
#' @return Matrix for the desired methodology (internal usage)
#' @examples 
#' ## This function is internal
#' attach(environment(FELLA:::getMatrix))
#' data(FELLA.sample)
#' # When a matrix is loaded:
#' x <- getMatrix(FELLA.sample, "hypergeom")
#' dim(x)
#' # When it is not:
#' y <- getMatrix(FELLA.sample, "diffusion")
#' dim(y)
#' y
getMatrix <- function(data, method) {
    return(slot(data, method)@matrix)
}

#' Map KEGG identifiers to KEGG names
#' 
#' Map KEGG identifiers to KEGG names, multiple names 
#' for an ID are reported if annotated. 
#' The KEGG identifiers may have mixed levels.
#'
#' @inheritParams .params
#' @param id KEGG IDs whose name is desired
#'
#' @return List whose names are KEGG IDs and 
#' whose entries are the vectors of matches
#' @examples 
#' data(FELLA.sample)
#' getName(FELLA.sample, c("C00002", "C00040"))
#' 
#' @export
getName <- function(data, id) {
    ans <- id
    names(ans) <- id
    
    names.intersect <- id %in% names(data@keggdata@id2name)
    ans[names.intersect] <- data@keggdata@id2name[ans[names.intersect]]
    return(ans)
}

#' Get p-scores from the desired methodology
#' 
#' Extractor function for the p-scores using the desired methodology
#'
#' @inheritParams .params
#'
#' @return Named vector of p-scores
#' 
#' @examples
#' data(FELLA.sample)
#' data(input.sample)
#' obj <- enrich(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' p <- getPscores(obj, "diffusion")
#' sum(p < 0.1)
#' 
#' @export
getPscores <- function(object, method) {
    if (method == "hypergeom") return(slot(object, method)@pvalues)
    return(slot(object, method)@pscores)
}

#' Get matrix for the p-value regarding CC size
#' 
#' Extractor function for the matrix containing p-value by CC size
#' that compares to a random selection of nodes in the KEGG graph
#'
#' @inheritParams .params
#'
#' @return Matrix with p-values for CC size (internal usage)
#' 
#' @examples 
#' ## This function is internal
#' attach(environment(FELLA:::getPvaluesSize))
#' data(FELLA.sample)
#' M <- getPvaluesSize(FELLA.sample)
#' dim(M)
#' summary(as.vector(M))
#' @keywords internal
getPvaluesSize <- function(data) {
    return(data@keggdata@pvalues.size)
}

#' Get rowSums/squaredRowSums
#' 
#' Extractor function for rowSums/squaredRowSums
#'
#' @inheritParams .params
#' @param squared Logical, whether to return 
#' \code{rowSums} (\code{F}) or \code{squaredRowSums} (\code{T})
#'
#' @return Named vector with rowSums/squaredRowSums (internal usage)
#' @examples 
#' ## This function is internal
#' attach(environment(FELLA:::getSums))
#' data(FELLA.sample)
#' rowsums <- getSums(FELLA.sample, "diffusion", squared = FALSE)
#' hist(rowsums)
getSums <- function(data, method, squared) {
    if (squared) 
        return(slot(data, method)@squaredRowSums)
    
    if (!squared) 
        return(slot(data, method)@rowSums)
    
    invisible()
}

#' Get the slot "valid"
#' 
#' Extractor function for the slot "valid"
#'
#' @inheritParams .params
#'
#' @return Slot "valid" (internal usage)
#' 
#' @examples 
#' ## This function is internal
#' 
#' data(FELLA.sample)
#' data(input.sample)
#' 
#' obj <- enrich(
#' compounds = input.sample, 
#' method = "diffusion", 
#' data = FELLA.sample)
#' 
#' ## If the analysis is valid
#' FELLA:::getValid(obj, "diffusion")
#' 
#' ## Otherwise
#' FELLA:::getValid(new("FELLA.USER"), "diffusion")
#' FELLA:::getValid(obj, "pagerank")
#' 
getValid <- function(object, method) {
    return(slot(object, method)@valid)
}

#' Get the slot "status" 
#' 
#' Extractor function for the slot "status" for the KEGG data
#'
#' @inheritParams .params
#'
#' @return Slot "status" (internal usage)
#' 
#' @examples 
#' ## This function is internal
#' 
#' data(FELLA.sample)
#' 
#' ## Is the object loade?
#' FELLA:::getStatus(FELLA.sample)
#' FELLA:::getStatus(new("FELLA.DATA"))
getStatus <- function(data) {
    return(data@keggdata@status)
}