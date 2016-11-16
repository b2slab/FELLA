#' Pathway enrichment through heat diffusion
#' 
#' Function \code{runDiffusion} performs the diffusion-based enrichment on a 
#' \code{\link[FELLA]{FELLA.USER}} object. If a custom background was specified, 
#' it will be used. 
#' This procedure gives statistical significance measures for each node and allows 
#' the extraction of a subgraph according to a fixed threshold.
#'
#' @inheritParams .object
#' @inheritParams .data
#' @inheritParams .approx
#' @inheritParams .t.df
#' @inheritParams .niter
#' @inheritParams .p.adjust
#' @inheritParams .BIMODAL
#'
#' @return The \code{\link[FELLA]{FELLA.USER}} object with the diffusion enrichment results
#' 
#' @import Matrix
#' @import igraph
#' @export
runDiffusion <- function(
  object = NULL, 
  data = NULL, 
  approx = "simulation", 
  t.df = 10, 
  niter = 1000, 
  p.adjust = "fdr", 
  BIMODAL = FALSE) {
  
  # Checking the input
  ##############################################################################
  if (!is.FELLA.USER(object)) {
    message("'object' is not a FELLA.USER object. ", 
            "Returning NULL...")
    return(invisible())
  } 
  if (!is.FELLA.DATA(data)) {
    message("'data' is not a FELLA.DATA object. ", 
            "Returning NULL...")
    return(invisible())
  }
  
  if (data@keggdata@status != "loaded"){
    message("'data' points to an empty FELLA.DATA object! ", 
            "Returning original 'object'...")
    return(object)
  }
  
  if (!is.character(approx)) {
    message("'approx' must be a character: ", 
            "'simulation', 'normality', 'gamma' or 't'. ", 
            "Returning original 'object'...")
    return(object)
  }
  
  if (!(approx %in% c("simulation", "normality", "gamma", "t"))) {
    message("'approx' must be a character: ", 
            "'simulation', 'normality', 'gamma' or 't'. ", 
            "Returning original 'object'...")
    return(object)
  }
  
  if (!is.numeric(t.df)) {
    message("'t.df' must be a real value greater than 0. ", 
            "Returning original 'object'...")
    return(object)
  }
  
  if (t.df <= 0) {
    message("'t.df' must be a real value greater than 0. ", 
            "Returning original 'object'...")
    return(object)
  }
  
  if (!is.numeric(niter)) {
    message("'niter' must be an integer between 100 and 1e5. ", 
            "Returning original 'object'...")
    return(object)
  }
  
  if (niter < 100 | niter > 1e5) {
    message("'niter' must be an integer between 100 and 1e5. ", 
            "Returning original 'object'...")
    return(object)
  }
  ##############################################################################
  
  message("Running diffusion...")
  
  # The metabolites in the input
  comp.input <- getInput(object)
  n.input <- length(comp.input)
  
  if (n.input == 0) {
    message("Diffusion failed because there are no compounds in the input.")
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
    
    if (prod(dim(getMatrix(data, "diffusion"))) == 1) {
      message("Diffusion matrix not loaded. Simulations will be slower...")
      
      # Load the graph as undirected and its Laplacian
      graph <- as.undirected(getGraph(data))
      KI <- graph.laplacian(graph = graph, 
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
      pvalues <- sapply(1:n.nodes, function(row) {
        ((1 - ecdf(null.temp[row, ])(current.temp[row]))*
           n.nodes + 1)/(n.nodes + 1)
      })
      names(pvalues) <- V(graph)$name
      
      
    } else {
      # Calculate current temperature
      diffusion.matrix <- getMatrix(data, "diffusion")
      
      if (n.input == 1) {
        current.temp <- diffusion.matrix[, comp.input]
      }
      else current.temp <- rowSums(diffusion.matrix[, comp.input])
      

      null.temp <- sapply(1:niter, function(dummy) {
        if (dummy %% round(.1*niter) == 0) 
          message(round(dummy*100/niter),"%")
        
        rowSums(diffusion.matrix[, sample(comp.background, n.input)])
      })

      n.nodes <- length(current.temp)
      pvalues <- sapply(1:n.nodes, function(row) {
        ((1 - ecdf(null.temp[row, ])(current.temp[row]))*
           n.nodes + 1)/(n.nodes + 1)
      })
      names(pvalues) <- rownames(diffusion.matrix)
    }
    
  } else if (approx %in% c("normality", "gamma", "t")) {
    message("Estimating p-values through the specified distribution.")
    
    if (length(getBackground(object)) > 0 | BIMODAL) {
      if (prod(dim(getMatrix(data, "diffusion"))) == 1) {
        # Custom background, no matrix...
        message("Diffusion matrix not loaded. ", 
                "Normality is not available yet for custom background.")
        return(object)
      } else if (BIMODAL) {
        # THIS ELSE IS THE ONLY BIMODAL THING
        # you can safely delete it
        background.matrix <- getMatrix(data, "diffusion")
        idComp <- getCom(data, "compound")
        
        inputInfluence <- diag(background.matrix[idComp, idComp])
#         browser()
        
        diag(background.matrix[idComp, idComp]) <- 0
        
        RowSums <- rowSums(background.matrix) 
        squaredRowSums <- apply(X = background.matrix, 
                                MARGIN = 1, 
                                FUN = function(row) sum(row*row))
        
        n.comp <- dim(background.matrix)[2]
      } else {
        # Custom background, matrix available
        background.matrix <- 
          getMatrix(data, "diffusion")[, getBackground(object)]
        RowSums <- rowSums(background.matrix)
        squaredRowSums <- apply(X = background.matrix, 
                                MARGIN = 1, 
                                FUN = function(row) sum(row*row))
        
        n.comp <- dim(background.matrix)[2]
      }
    } else {# Default background
      n.comp <- length(getCom(data, "compound"))
      
      # RowSums
      if (length(getSums(data, "diffusion", squared = FALSE)) == 0) {
        message("RowSums not available. ", 
                "The normal approximation cannot be done.")
        return(object)
      } else {
        RowSums <- getSums(data, "diffusion", squared = FALSE)
      }
      
      # Squared RowSums
      if (length(getSums(data, "diffusion", squared = TRUE)) == 0) {
        message("squaredRowSums not available. ", 
                "The normal approximation cannot be done.")
        return(object)
      } else {
        squaredRowSums <- getSums(data, "diffusion", squared = TRUE)
      }
    }
    
    # Compute current temperature
    # Load the graph as undirected and its Laplacian
    graph <- as.undirected(getGraph(data))
    KI <- graph.laplacian(graph = graph, 
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
    
    # p-values
    temp.means <- RowSums*n.input/n.comp
    temp.vars <- n.input*(n.comp - n.input)/(n.comp*(n.comp - 1))*
      (squaredRowSums - (RowSums^2)/n.comp)
    # ONLY THIS IS BIMODAL
    if (BIMODAL){
      # need to change the compounds
      temp.means[idComp] <- RowSums[idComp]*(n.input)/(n.comp - 1) 
      temp.vars[idComp] <- 
        n.input*(n.comp - 1 - n.input)/((n.comp - 1)*(n.comp - 2))*
        (squaredRowSums[idComp] - (RowSums[idComp]^2)/(n.comp - 1))
#       browser()
      # input:
      theInput <- getInput(object)
      temp.means[theInput] <- 
        RowSums[theInput]*(n.input - 1)/(n.comp - 1) + inputInfluence[theInput]
      temp.vars[theInput] <- 
        (n.input - 1)*(n.comp - n.input)/((n.comp - 1)*(n.comp - 2))*
        (squaredRowSums[theInput] - (RowSums[theInput]^2)/(n.comp - 1))

    }
    
    if (approx == "normality") {
      pvalues <- pnorm(
        q = current.temp, 
        mean = temp.means, 
        sd = sqrt(temp.vars), 
        lower.tail = FALSE)
    }
    if (approx == "gamma") {
      pvalues <- pgamma(
        q = current.temp, 
        shape = temp.means^2/temp.vars, 
        scale = temp.vars/temp.means, 
        lower.tail = FALSE)
    }
    if (approx == "t") {
      pvalues <- pt(
        q = (current.temp - temp.means)/sqrt(temp.vars), 
        df = t.df, 
        lower.tail = FALSE)
    }
    
    names(pvalues) <- names(RowSums)
#     browser()
  } else {
    stop("Invalid 'type' argument for heat diffusion. ", 
         "Please choose between 'simulation' and 'normality'")
  }

  pvalues <- p.adjust(p = pvalues, method = p.adjust)

  object@diffusion@pvalues <- pvalues
  object@diffusion@approx <- approx
  object@diffusion@niter <- niter

  message("Done.")
  object@diffusion@valid <- TRUE
  return(object)
}