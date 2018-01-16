#' @include runDiffusion.R
#' 
#' @details
#' Function \code{runPagerank} performs the random walk 
#' based enrichment on a 
#' \code{\link{FELLA.USER}} object with mapped metabolites 
#' and a \code{\link{FELLA.DATA}} object.
#' If a custom background was specified, it will be used. 
#' PageRank was originally conceived as a scoring system for websites 
#' [Page, 1999]. 
#' Intuitively, PageRank favours nodes that 
#' (1) have a large amount of nodes pointing 
#' at them, and (2) whose pointing nodes also have high scores. 
#' Classical PageRank is formulated in terms of a random walker -  
#' the PageRank of a given node is the stationary probability 
#' of the walker visiting it. 
#' 
#' The walker chooses, in each step, 
#' whether to continue the random walk with probability 
#' \code{dampingFactor} or to restart it with probability 
#' \code{1 - dampingFactor}. 
#' In the original publication, \code{dampingFactor = 0.85}, 
#' which is the value used in \code{FELLA} by default. 
#' If he or she continues, an edge is picked from the outgoing edges 
#' in the current node with a probability proportional to its weight. 
#' If he or she restarts it, a node is uniformly picked from the 
#' whole graph. 
#' The "personalised PageRank" variant allows a user-defined 
#' distribution as the source of new random walks. 
#' The R package \code{igraph} contains such variant in its 
#' \code{\link[igraph:page_rank]{page.rank}} function [Csardi, 2006].
#' 
#' As described in the supplement S3 from [Picart-Armada, 2017], 
#' the PageRank \code{PR} can be computed as 
#' a column vector by imposing a stationary 
#' state in the probability.
#' With a damping factor \code{d} and the user-defined 
#' distribution \code{p} as a column vector:
#' 
#' \deqn{
#' \textrm{PR} = d\cdot M\cdot \textrm{PR} + (1 - d)\cdot p
#' }{
#' PR = d*M*PR + (1 - d)*p
#' }
#' 
#' \code{M} is the matrix whose element \code{M[i,j]} is the 
#' probability of transitioning from \code{j} to \code{i}. 
#' If node \code{j} has outgoing edges, their probability is proportional 
#' to their weight - all weights must be positive. 
#' If node \code{j} has no outgoing edges, the probability is 
#' uniform over all the nodes, i.e. \code{M[i,j] = 1/nrow(M)} 
#' for every \code{i}. 
#' Note that all the columns from \code{M} sum up exactly \code{1}.
#' This leads to an expression to compute PageRank:
#' 
#' \deqn{
#' \textrm{PR} = (1 - d)p \cdot(I - dM)^{-1}
#' }{
#' PR = (1 - d)*p*(I - d*M)^(-1)
#' }
#' 
#' The idea behind the method \code{"pagerank"} is closely related 
#' to \code{"diffusion"}. 
#' Relevant metabolites are the sources of new random walks and 
#' nodes are scored through their PageRank. 
#' Specifically, \code{p} is set to a uniform probability on the 
#' input metabolites. 
#' More details on the setup can be found in 
#' the supplementary file S3 from [Picart-Armada, 2017].
#' 
# ' @template approxTemplate
#'
#' @inheritParams .params
#'
#' @return \code{runPagerank} returns a 
#' \code{\link[FELLA]{FELLA.USER}} object 
#' updated with the PageRank enrichment results
#' 
#' @rdname enrich-funs
#' 
#' @importFrom stats ecdf pnorm pgamma pt
#' @import Matrix
#' @import igraph
#' @export
runPagerank <- function(
    object = NULL, 
    data = NULL, 
    approx = "normality", 
    dampingFactor = 0.85, 
    t.df = 10, 
    niter = 1000) {
    
    # Checking the input
    ###########################
    checkArgs <- checkArguments(
        approx = approx, 
        dampingFactor = dampingFactor, 
        t.df = t.df, 
        niter = niter,
        object = object, 
        data = data)
    if (!checkArgs$valid)
        stop("Bad argument when calling function 'runPagerank'.")
    
    if (getStatus(data) != "loaded"){
        message(
            "'data' points to an empty FELLA.DATA object! ", 
            "Returning original 'object'...")
        return(object)
    }
    #####################
    
    message("Running PageRank...")
    
    d <- dampingFactor
    
    # The metabolites in the input
    comp.input <- getInput(object)
    n.input <- length(comp.input)
    
    if (n.input == 0) {
        message(
            "PageRank failed because ", 
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
        
        if (prod(dim(getMatrix(data, "pagerank"))) == 1) {
            message(
                "PageRank matrix not loaded. ", 
                "Simulations may be a bit slower...", 
                "Using provided damping factor")
            
            # Load the graph 
            graph <- getGraph(data)
            n.nodes <- vcount(graph)
            
            # Prior for personalized PageRank
            prior <- numeric(n.nodes)
            names(prior) <- V(graph)$name
            
            # Current scores
            prior[comp.input] <- 1
            current.score <- page.rank(
                graph = graph, 
                directed = TRUE, 
                damping = d, 
                personalized = prior)$vector
            prior[comp.input] <- 0
            
            # Null model
            null.score <- vapply(seq_len(niter), function(dummy) {
                if (dummy %% round(.1*niter) == 0) 
                    message(round(dummy*100/niter),"%")
                
                prior[sample(comp.background, n.input)] <- 1
                page.rank(
                    graph = graph, 
                    directed = TRUE, 
                    damping = d, 
                    personalized = prior)$vector
            }, FUN.VALUE = double(n.nodes))
            
            pscores <- vapply(seq_len(n.nodes), function(row) {
                ((1 - stats::ecdf(null.score[row, ])(current.score[row]))*
                    n.nodes + 1)/(n.nodes + 1)
            }, FUN.VALUE = double(1))
            names(pscores) <- V(graph)$name
            
        } else {
            # Calculate current scores.
            # Warning: each score vector should be divided by n.input 
            # to be consistent with the rest of this function.
            # This is not done, because it just rescales all the scores 
            # and becomes irrelevant when computing the empirical p-value.
            message(
                "Using pagerank matrix. ", 
                "Damping factor will be the one with which the database ",
                "was built")
            
            pagerank.matrix <- getMatrix(data, "pagerank")
            
            current.score <- rowSums(
                pagerank.matrix[, comp.input, drop = FALSE])
            
            null.score <- vapply(seq_len(niter), function(dummy) {
                if (dummy %% round(.1*niter) == 0) 
                    message(round(dummy*100/niter),"%")
                
                rowSums(pagerank.matrix[, sample(comp.background, n.input)])
            }, FUN.VALUE = double(n.nodes))
            
            n.nodes <- length(current.score)
            pscores <- vapply(seq_len(n.nodes), function(row) {
                ((1 - stats::ecdf(null.score[row, ])(current.score[row]))*
                    n.nodes + 1)/(n.nodes + 1)
            }, FUN.VALUE = double(1))
            names(pscores) <- rownames(pagerank.matrix)
        }
    ###### Second case: moments   
    } else {
        message("Computing p-scores through the specified distribution.")
        
        # The background
        if (length(getBackground(object)) > 0) {
            if (prod(dim(getMatrix(data, "pagerank"))) == 1) {
                # Custom background, no matrix...
                message(
                    "PageRank matrix not loaded. ", 
                    "Normality is not available yet for custom background.")
                return(object)
            } else {
                # Custom background, matrix available
                background.matrix <- 
                    getMatrix(data, "pagerank")[, getBackground(object)]
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
            if (length(getSums(data, "pagerank", squared = FALSE)) == 0) {
                message(
                    "RowSums not available. ", 
                    "The normal approximation cannot be done.")
                return(object)
            } else {
                RowSums <- getSums(data, "pagerank", squared = FALSE)
            }
            
            # Squared RowSums
            if (length(getSums(data, "pagerank", squared = TRUE)) == 0) {
                message(
                    "squaredRowSums not available. ", 
                    "The normal approximation cannot be done.")
                return(object)
            } else {
                squaredRowSums <- getSums(data, "pagerank", squared = TRUE)
            }
        }
        
        # Compute current score
        # Load the graph 
        graph <- getGraph(data)
        
        # Prior for personalized PageRank
        prior <- numeric(vcount(graph))
        names(prior) <- V(graph)$name
        
        message("Using provided damping factor...")
        
        # Current scores
        prior[comp.input] <- 1
        current.score <- page.rank(
            graph = graph, 
            directed = TRUE, 
            damping = d, 
            personalized = prior)$vector
        
        # p-scores
        # statistical moments
        # note that, compared to runDiffusion, these 
        # moments are divided by n.input (means) 
        # and by n.input**2 (vars)
        # This is becasue igraph takes as input the indicator
        # vector divided by its sum, which is indeed n.input
        score.means <- RowSums/n.comp
        score.vars <- (n.comp - n.input)/(n.input*n.comp*(n.comp - 1))*
            (squaredRowSums - (RowSums^2)/n.comp)
        
        if (approx == "normality") {
            pscores <- stats::pnorm(
                q = current.score, 
                mean = score.means, 
                sd = sqrt(score.vars), 
                lower.tail = FALSE)
        }
        if (approx == "gamma") {
            pscores <- stats::pgamma(
                q = current.score, 
                shape = score.means^2/score.vars, 
                scale = score.vars/score.means, 
                lower.tail = FALSE)
        }
        if (approx == "t") {
            pscores <- stats::pt(
                q = (current.score - score.means)/sqrt(score.vars), 
                df = t.df, 
                lower.tail = FALSE)
        }
        
        names(pscores) <- names(RowSums)
        
    } 
    
    object@pagerank@pscores <- pscores
    object@pagerank@approx <- approx
    object@pagerank@niter <- niter  
    
    message("Done.")
    object@pagerank@valid <- TRUE
    return(object)
}