#' Pathway enrichment through heat diffusion
#' 
#' Function \code{runDiffusion} performs 
#' the diffusion-based enrichment on a 
#' \code{\link[FELLA]{FELLA.USER}} object. 
#' If a custom background was specified, 
#' it will be used. 
#' 
#' @template approxTemplate
#'
#' @inheritParams .params
#'
#' @return The \code{\link[FELLA]{FELLA.USER}} object 
#' with the diffusion enrichment results
#' 
#' @examples
#' data(FELLA.sample)
#' ## Load a list of compounds to enrich
#' data(input.sample)
#' obj.empty <- defineCompounds(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' obj.diff <- runDiffusion(
#' object = obj.empty, 
#' approx = "normality", 
#' data = FELLA.sample)
#' obj.diff
#' 
#' ## Note that the enrich wrapper can do this in a compact way
#' obj.diff <- enrich(
#' compounds = input.sample, 
#' method = "diffusion", 
#' data = FELLA.sample)
#' obj.diff
#' 
#' @importFrom stats ecdf pnorm pgamma pt
#' @import Matrix
#' @import igraph
#' @export
runDiffusion <- function(
    object = NULL, 
    data = NULL, 
    approx = "normality", 
    t.df = 10, 
    niter = 1000) {
    
    # Checking the input
    ###########################
    checkArgs <- checkArguments(
        approx = approx, 
        t.df = t.df, 
        niter = niter,
        object = object, 
        data = data)
    if (!checkArgs$valid)
        stop("Bad argument when calling function 'runDiffusion'.")
    
    if (getStatus(data) != "loaded"){
        message(
            "'data' points to an empty FELLA.DATA object! ", 
            "Returning original 'object'...")
        return(object)
    }
    #####################
    
    message("Running diffusion...")
    
    # The metabolites in the input
    comp.input <- getInput(object)
    n.input <- length(comp.input)
    
    if (n.input == 0) {
        message(
            "Diffusion failed because ", 
            "there are no compounds in the input.")
        return(object)
    }
    
    ###### First case: simulation
    if (approx == "simulation") {
        message("Estimating p-values by simulation.")
        
        # The background
        if (length(getBackground(object)) == 0) {
            comp.background <- getCom(data, "compound")
        } else {
            comp.background <- getBackground(object)
        }
        
        if (prod(dim(getMatrix(data, "diffusion"))) == 1) {
            message(
                "Diffusion matrix not loaded. ", 
                "Simulations will be slower...")
            
            # Load the graph as undirected and its Laplacian
            graph <- as.undirected(getGraph(data))
            KI <- graph.laplacian(
                graph = graph, 
                normalized = FALSE, 
                sparse = TRUE)
            # Connect pathways to boundary
            Matrix::diag(KI)[getCom(data, "pathway", "id")] <- 
                Matrix::diag(KI)[getCom(data, "pathway", "id")] + 1
            
            # Heat generation vector
            generation <- numeric(dim(KI)[1])
            names(generation) <- V(graph)$name
            
            # Current temperatures
            generation[comp.input] <- 1
            current.temp <- as.vector(solve(KI, generation))
            generation[comp.input] <- 0
            
            # Null model
            null.temp <- sapply(1:niter, function(dummy) {
                if (dummy %% round(.1*niter) == 0) 
                    message(round(dummy*100/niter),"%")
                
                generation[sample(comp.background, n.input)] <- 1
                as.vector(solve(KI, generation)) 
            })
            
            n.nodes <- length(current.temp)
            pscores <- sapply(1:n.nodes, function(row) {
                ((1 - stats::ecdf(null.temp[row, ])(current.temp[row]))*
                    n.nodes + 1)/(n.nodes + 1)
            })
            names(pscores) <- V(graph)$name
            
            
        } else {
            # Calculate current temperature
            diffusion.matrix <- getMatrix(data, "diffusion")
            
            current.temp <- rowSums(
                diffusion.matrix[, comp.input, drop = FALSE])
            
            null.temp <- sapply(1:niter, function(dummy) {
                if (dummy %% round(.1*niter) == 0) 
                    message(round(dummy*100/niter),"%")
                
                rowSums(
                    diffusion.matrix[, sample(comp.background, n.input)])
            })
            
            n.nodes <- length(current.temp)
            pscores <- sapply(1:n.nodes, function(row) {
                ((1 - stats::ecdf(null.temp[row, ])(current.temp[row]))*
                    n.nodes + 1)/(n.nodes + 1)
            })
            names(pscores) <- rownames(diffusion.matrix)
        }
        
    ###### Second case: moments
    } else if (approx %in% c("normality", "gamma", "t")) {
        message("Computing p-scores through the specified distribution.")
        if (length(getBackground(object)) > 0) {
            if (prod(dim(getMatrix(data, "diffusion"))) == 1) {
                # Custom background, no matrix...
                message(
                    "Diffusion matrix not loaded. ", 
                    "Normality is not available for custom background ", 
                    "without it.")
                return(object)
            } else {# Custom background, matrix available
                background.matrix <- 
                    getMatrix(data, "diffusion")[, getBackground(object)]
                RowSums <- rowSums(background.matrix)
                squaredRowSums <- apply(
                    X = background.matrix, 
                    MARGIN = 1, 
                    FUN = function(row) sum(row*row))
                
                n.comp <- ncol(background.matrix)
            }
        } else {# Default background
            n.comp <- length(getCom(data, "compound"))
            
            # RowSums
            if (length(getSums(data, "diffusion", squared = FALSE)) == 0) {
                message(
                    "RowSums not available. ", 
                    "The normal approximation cannot be done.")
                return(object)
            } else {
                RowSums <- getSums(data, "diffusion", squared = FALSE)
            }
            
            # Squared RowSums
            if (length(getSums(data, "diffusion", squared = TRUE)) == 0) {
                message(
                    "squaredRowSums not available. ", 
                    "The normal approximation cannot be done.")
                return(object)
            } else {
                squaredRowSums <- getSums(data, "diffusion", squared = TRUE)
            }
        }
        
        # Compute current temperature
        # Load the graph as undirected and its Laplacian
        graph <- as.undirected(getGraph(data))
        KI <- graph.laplacian(
            graph = graph, 
            normalized = FALSE, 
            sparse = TRUE)
        # Connect pathways to boundary
        Matrix::diag(KI)[getCom(data, "pathway", "id")] <- 
            Matrix::diag(KI)[getCom(data, "pathway", "id")] + 1
        
        # Heat generation vector
        generation <- numeric(nrow(KI))
        names(generation) <- V(graph)$name
        
        # Current temperatures
        generation[comp.input] <- 1
        current.temp <- as.vector(solve(KI, generation))
        
        # Statistical moments
        temp.means <- RowSums*n.input/n.comp
        temp.vars <- n.input*(n.comp - n.input)/(n.comp*(n.comp - 1))*
            (squaredRowSums - (RowSums^2)/n.comp)
        
        # Statistical approximations
        if (approx == "normality") {
            pscores <- stats::pnorm(
                q = current.temp, 
                mean = temp.means, 
                sd = sqrt(temp.vars), 
                lower.tail = FALSE)
        }
        if (approx == "gamma") {
            pscores <- stats::pgamma(
                q = current.temp, 
                shape = temp.means^2/temp.vars, 
                scale = temp.vars/temp.means, 
                lower.tail = FALSE)
        }
        if (approx == "t") {
            pscores <- stats::pt(
                q = (current.temp - temp.means)/sqrt(temp.vars), 
                df = t.df, 
                lower.tail = FALSE)
        }
        
        names(pscores) <- names(RowSums)
    } 
    
    object@diffusion@pscores <- pscores
    object@diffusion@approx <- approx
    object@diffusion@niter <- niter
    
    message("Done.")
    object@diffusion@valid <- TRUE
    return(object)
}