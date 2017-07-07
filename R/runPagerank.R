#' Pathway enrichment through PageRank
#' 
#' Function \code{runPagerank} performs the random walk 
#' based enrichment on a 
#' \code{\link[FELLA]{FELLA.USER}} object, making use of igraph function 
#' \code{\link[igraph]{page.rank}}. 
#' If a custom background was specified, it will be used. 
#' This procedure gives statistical significance measures for each node 
#' and allows the extraction of a subgraph according to a fixed threshold.
#'
#' @inheritParams .object
#' @inheritParams .data
#' @inheritParams .approx
#' @param dampingFactor Numeric value between 0 and 1 (none inclusive), 
#' damping factor \code{d} for 
#' PageRank (\code{\link[igraph]{page_rank}})
#' @inheritParams .t.df
#' @inheritParams .niter
#' @inheritParams .p.adjust
#'
#' @return The \code{\link[FELLA]{FELLA.USER}} object 
#' with the PageRank enrichment results
#' 
#' @examples 
#' data(FELLA.sample)
#' ## Load a list of compounds to enrich
#' data(input.sample)
#' obj.empty <- defineCompounds(
#' compounds = input.sample, 
#' data = FELLA.sample)
#' obj.diff <- runPagerank(
#' object = obj.empty, 
#' approx = "normality", 
#' data = FELLA.sample)
#' obj.diff
#' 
#' ## Note that the enrich wrapper can do this in a compact way
#' obj.diff <- enrich(
#' compounds = input.sample, 
#' method = "pagerank", 
#' data = FELLA.sample)
#' obj.diff
#' 
#' @import Matrix
#' @import igraph
#' @export
runPagerank <- function(
    object = NULL, 
    data = NULL, 
    approx = "normality", 
    dampingFactor = 0.85, 
    t.df = 10, 
    niter = 1000, 
    p.adjust = "fdr") {
    
    # Checking the input
    #########################
    if (!is.FELLA.USER(object)) {
        message(
            "'object' is not a FELLA.USER object. ", 
            "Returning NULL...")
        return(invisible())
    } 
    if (!is.FELLA.DATA(data)) {
        message(
            "'data' is not a FELLA.DATA object. ", 
            "Returning NULL...")
        return(invisible())
    }
    
    if (data@keggdata@status != "loaded"){
        message(
            "'data' points to an empty FELLA.DATA object! ", 
            "Returning original 'object'...")
        return(object)
    }
    
    if (!is.character(approx)) {
        message(
            "'approx' must be a character: ", 
            "'simulation', 'normality', 'gamma' or 't'. ", 
            "Returning original 'object'...")
        return(object)
    }
    
    if (!(approx %in% c("simulation", "normality", "gamma", "t"))) {
        message(
            "'approx' must be a character: ", 
            "'simulation', 'normality', 'gamma' or 't'. ", 
            "Returning original 'object'...")
        return(object)
    }
    
    if (!is.numeric(t.df)) {
        message(
            "'t.df' must be a real value greater than 0. ", 
            "Returning original 'object'...")
        return(object)
    }
    
    if (t.df <= 0) {
        message(
            "'t.df' must be a real value greater than 0. ", 
            "Returning original 'object'...")
        return(object)
    }
    
    if (!is.numeric(niter)) {
        message(
            "'niter' must be an integer between 100 and 1e5. ", 
            "Returning original 'object'...")
        return(object)
    }
    
    if (niter < 100 | niter > 1e5) {
        message(
            "'niter' must be an integer between 100 and 1e5. ", 
            "Returning original 'object'...")
        return(object)
    }
    #######################
    
    message("Running PageRank...")
    
    # Damping factor
    if (!is.numeric(dampingFactor)) 
        stop(
            "'dampingFactor' must be a number ", 
            "between 0 and 1, both excluded.")
    
    if (dampingFactor <= 0 | dampingFactor >= 1) 
        stop(
            "'dampingFactor' must be a number ", 
            "between 0 and 1, both excluded.")
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
            
            # Prior for personalized PageRank
            prior <- numeric(vcount(graph))
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
            null.score <- sapply(1:niter, function(dummy) {
                if (dummy %% round(.1*niter) == 0) 
                    message(round(dummy*100/niter),"%")
                
                prior[sample(comp.background, n.input)] <- 1
                page.rank(
                    graph = graph, 
                    directed = TRUE, 
                    damping = d, 
                    personalized = prior)$vector
            })
            
            n.nodes <- length(current.score)
            pscores <- sapply(1:n.nodes, function(row) {
                ((1 - stats::ecdf(null.score[row, ])(current.score[row]))*
                    n.nodes + 1)/(n.nodes + 1)
            })
            names(pscores) <- V(graph)$name
            
        } else {
            # Calculate current scores.
            # Warning: each score vector should be divided by n.input. 
            # It won't, as it just rescales all the scores 
            # and becomes irrelevant for the test.
            message(
                "Using pagerank matrix. ", 
                "Damping factor will be the one with which the database ",
                "was built")
            
            pagerank.matrix <- getMatrix(data, "pagerank")
            
            if (n.input == 1) {
                current.score <- pagerank.matrix[, comp.input]
            }
            else current.score <- rowSums(pagerank.matrix[, comp.input])
            
            null.score <- sapply(1:niter, function(dummy) {
                if (dummy %% round(.1*niter) == 0) 
                    message(round(dummy*100/niter),"%")
                
                rowSums(pagerank.matrix[, sample(comp.background, n.input)])
            })
            
            n.nodes <- length(current.score)
            pscores <- sapply(1:n.nodes, function(row) {
                ((1 - stats::ecdf(null.score[row, ])(current.score[row]))*
                    n.nodes + 1)/(n.nodes + 1)
            })
            names(pscores) <- rownames(pagerank.matrix)
        }
        
    } else if (approx %in% c("normality", "gamma", "t")) {
        message("Estimating p-values through the specified distribution.")
        
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
                
                n.comp <- dim(background.matrix)[2]
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
    
    pscores <- stats::p.adjust(p = pscores, method = p.adjust)
    
    object@pagerank@pscores <- pscores
    object@pagerank@approx <- approx
    object@pagerank@niter <- niter  
    
    message("Done.")
    object@pagerank@valid <- TRUE
    return(object)
}