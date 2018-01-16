#' @include runHypergeom.R
#' 
#' @details
#' Function \code{runDiffusion} performs 
#' the diffusion-based enrichment on a 
#' \code{\link{FELLA.USER}} object with mapped metabolites 
#' and a \code{\link{FELLA.DATA}} object [Picart-Armada, 2017]. 
#' If a custom background was specified, it will be used. 
#' The idea behind the heat diffusion is the usage of the 
#' finite difference formulation of the heat equation to 
#' propagate labels from the metabolites to the rest of the graph.
#' 
#' Following the notation in [Picart-Armada, 2017], 
#' the temperatures (diffusion scores) 
#' are computed as:
#' 
#' \deqn{
#' T = -KI^{-1} \cdot G 
#' }{
#' T = -KI^(-1)*G 
#' }
#' 
#' \code{G} is an indicator vector of the input metabolites 
#' (\code{1} if input metabolite, \code{0} otherwise).
#' \code{KI} is the matrix \code{-KI = L + B}, being 
#' \code{L} the unnormalised graph Laplacian and 
#' \code{B} the diagonal matrix with \code{B[i,i] = 1} if 
#' node \code{i} is a pathway and \code{B[i,i] = 0} otherwise.
#' 
#' Equivalently, with the notation in the HotNet approach [Vandin, 2011], 
#' the stationary temperature is named \code{fs}:
#' 
#' \deqn{
#' f^s = L_{\gamma}^{-1} \cdot b^s 
#' }{
#' fs = Lgamma^(-1)*bs 
#' }
#'
#' \code{bs} is the indicator vector \code{G} from above. 
#' \code{Lgamma}, on the other hand, is found as 
#' \code{Lgamma = L + gamma*I}, where \code{L} is the unnormalised 
#' graph Laplacian, \code{gamma} is the first order leaking rate 
#' and \code{I} is the identity matrix. 
#' In our formulation, only the pathway nodes are allowed to leak, 
#' therefore \code{I} is switched to \code{B}. 
#' The parameter \code{gamma} is set to \code{gamma = 1}.
#'
#' The input metabolites are forced to stay warm, 
#' propagating flow to all the nodes in the network. 
#' However, only pathway nodes are allowed to evacuate 
#' this flow, so that its directionality is bottom-up. 
#' Further details on the setup of the diffusion process can be 
#' found in the supplementary file S2 from [Picart-Armada, 2017].
#' 
#' Finally, the warmest nodes in the graph are reported as 
#' the relevant sub-network. 
#' This will probably include some input metabolites and 
#' also reactions, enzymes, modules and pathways. 
#' Other metabolites can be suggested as well. 
#'
#' @inheritParams .params
#'
#' @return \code{runDiffusion} returns a 
#' \code{\link{FELLA.USER}} object 
#' updated with the diffusion enrichment results
#' 
#' @rdname enrich-funs 
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
            n.nodes <- vcount(graph)
            
            minusKI <- graph.laplacian(
                graph = graph, 
                normalized = FALSE, 
                sparse = TRUE)
            # Connect pathways to boundary
            Matrix::diag(minusKI)[getCom(data, "pathway", "id")] <- 
                Matrix::diag(minusKI)[getCom(data, "pathway", "id")] + 1.0
            
            # Heat generation vector
            generation <- numeric(dim(minusKI)[1])
            names(generation) <- V(graph)$name
            
            # Current temperatures
            generation[comp.input] <- 1
            current.temp <- as.vector(solve(minusKI, generation))
            generation[comp.input] <- 0
            
            # Null model
            null.temp <- vapply(seq_len(niter), function(dummy) {
                if (dummy %% round(.1*niter) == 0) 
                    message(round(dummy*100/niter),"%")
                
                generation[sample(comp.background, n.input)] <- 1
                as.vector(solve(minusKI, generation)) 
            }, FUN.VALUE = double(n.nodes))
            
            pscores <- vapply(seq_len(n.nodes), function(row) {
                ((1 - stats::ecdf(null.temp[row, ])(current.temp[row]))*
                    n.nodes + 1)/(n.nodes + 1)
            }, FUN.VALUE = double(1))
            names(pscores) <- V(graph)$name
        } else {
            # Calculate current temperature
            diffusion.matrix <- getMatrix(data, "diffusion")
            n.nodes <- nrow(diffusion.matrix)
            
            current.temp <- rowSums(
                diffusion.matrix[, comp.input, drop = FALSE])
            
            null.temp <- vapply(seq_len(niter), function(dummy) {
                if (dummy %% round(.1*niter) == 0) 
                    message(round(dummy*100/niter),"%")
                
                rowSums(
                    diffusion.matrix[, sample(comp.background, n.input)])
            }, FUN.VALUE = double(n.nodes))
            
            pscores <- vapply(seq_len(n.nodes), function(row) {
                ((1 - stats::ecdf(null.temp[row, ])(current.temp[row]))*
                    n.nodes + 1)/(n.nodes + 1)
            }, FUN.VALUE = double(1))
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
        minusKI <- graph.laplacian(
            graph = graph, 
            normalized = FALSE, 
            sparse = TRUE)
        # Connect pathways to boundary
        Matrix::diag(minusKI)[getCom(data, "pathway", "id")] <- 
            Matrix::diag(minusKI)[getCom(data, "pathway", "id")] + 1.0
        
        # Heat generation vector
        generation <- numeric(nrow(minusKI))
        names(generation) <- V(graph)$name
        
        # Current temperatures
        generation[comp.input] <- 1
        current.temp <- as.vector(solve(minusKI, generation))
        
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