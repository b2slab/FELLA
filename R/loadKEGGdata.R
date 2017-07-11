#' Load KEGG data 
#' 
#' This function loads all necessary contextual data from KEGG as a 
#' \code{\link[FELLA]{FELLA.DATA}} object. This object is necessary 
#' to perform any kind of enrichment using \code{\link[FELLA]{FELLA}}.
#'
#' @inheritParams .params
#' @param databaseDir Path for the KEGG RData files
#' @param internalDir Logical, is the directory located 
#' in the package directory?
#'
#' @return The \code{\link[FELLA]{FELLA.DATA}} object 
#' that contains the KEGG representation
#' 
#' @examples 
#' \dontrun{
#' ## Toy example
#' data("FELLA.sample")
#' ## Graph to build the database
#' g.sample <- FELLA:::getGraph(FELLA.sample)
#' dir.tmp <- tempdir()
#' ## Save it in a temporary directory
#' buildDataFromGraph(
#' keggdata.graph = g.sample, 
#' databaseDir = dir.tmp, 
#' internalDir = FALSE, 
#' matrices = c("hypergeom", "diffusion", "pagerank"), 
#' normality = c("diffusion", "pagerank"), 
#' dampingFactor = 0.7,
#' niter = 10)
#' ## Load database
#' myFELLA.DATA <- loadKEGGdata(
#' dir.tmp, 
#' internalDir = FALSE)
#' myFELLA.DATA
#' }
#' 
#' @import igraph
#' @export
loadKEGGdata <- function(
    databaseDir = "myDatabase", 
    internalDir = TRUE, 
    loadMatrix = NULL) { 
    
    message("Loading KEGG graph data...")
    
    # Checking the input
    ########################
    if (!is.null(loadMatrix) & length(loadMatrix) > 1)
        stop(
            "'loadMatrix' can only be a length 1 character ", 
            " ('diffusion', 'pagerank', 'all') or NULL.")
    
    if (!is.null(loadMatrix) & !is.character(loadMatrix))
        stop(
            "'loadMatrix' can only be a length 1 character ", 
            "('diffusion', 'pagerank', 'all') or NULL.")
    
    if (!is.character(databaseDir) | length(databaseDir) > 1) 
        stop(
            "'databaseDir' must be a length 1 character ", 
            " of an existing directory.")
    
    if (!is.logical(internalDir) | is.na(internalDir))
        stop("'internalDir' must be a non-NA logical value")
    
    ##########################
    assign("F.DATA", new("FELLA.DATA"))
    
    # Make sure there is a slash
    path <- ifelse(
        internalDir, 
        paste0(
            system.file("database", package = "FELLA"), 
            "/", databaseDir, "/"), 
        paste0(databaseDir, "/"))
    
    # Does the dir exist?
    if (!dir.exists(path)) {
        stop(
            "Directory ", path, " does not exist. ", 
            "Database '", databaseDir, "' cannot be found. ",
            "Aborting...")
    }
    
    # Load the graph and the identifiers (required)
    if (file.exists(paste0(path, "keggdata.graph.RData"))) {
        load(paste0(path, "keggdata.graph.RData"))
        keggdata.graph <- get("keggdata.graph")
        keggdata.pvalues.size <- get("keggdata.pvalues.size")
        
        F.DATA@keggdata@graph <- keggdata.graph
        F.DATA@keggdata@pvalues.size <- keggdata.pvalues.size
        F.DATA@keggdata@id2name <- V(keggdata.graph)$NAME
        names(F.DATA@keggdata@id2name) <- V(keggdata.graph)$name
        
        F.DATA@keggdata@id$pathway <- which(V(keggdata.graph)$com == 1)
        names(F.DATA@keggdata@id$pathway) <- 
            (V(keggdata.graph)$name)[F.DATA@keggdata@id$pathway]
        F.DATA@keggdata@id$module <- which(V(keggdata.graph)$com == 2)
        names(F.DATA@keggdata@id$module) <- 
            (V(keggdata.graph)$name)[F.DATA@keggdata@id$module]
        F.DATA@keggdata@id$enzyme <- which(V(keggdata.graph)$com == 3)
        names(F.DATA@keggdata@id$enzyme) <- 
            (V(keggdata.graph)$name)[F.DATA@keggdata@id$enzyme]
        F.DATA@keggdata@id$reaction <- which(V(keggdata.graph)$com == 4)
        names(F.DATA@keggdata@id$reaction) <- 
            (V(keggdata.graph)$name)[F.DATA@keggdata@id$reaction]
        F.DATA@keggdata@id$compound <- which(V(keggdata.graph)$com == 5)
        names(F.DATA@keggdata@id$compound) <- 
            (V(keggdata.graph)$name)[F.DATA@keggdata@id$compound]
    } else {
        stop(
            "'keggdata.graph.RData' not present in:", 
            paste0(path, "keggdata.graph.RData"), 
            ". Please check that KEGG data is available.")
    }
    message("Done.")
    
    # Load matrix for hypergeometric test
    message("Loading hypergeom data...")
    message("Loading matrix...")
    if (file.exists(paste0(path, "hypergeom.matrix.RData"))) {
        load(paste0(path, "hypergeom.matrix.RData"))
        hypergeom.matrix <- get("hypergeom.matrix")
        
        F.DATA@hypergeom@matrix <- hypergeom.matrix
    } else {
        message(
            "'hypergeom.matrix.RData' not present in:", 
            paste0(path, "hypergeom.matrix.RData"), 
            ". Hypergeometric test won't execute.")
    }
    message("Done.")
    
    # Load matrix for diffusion 
    message("Loading diffusion data...")
    message("Loading matrix...")
    if (identical(loadMatrix, "diffusion") || 
        identical(loadMatrix, "all")) {
        if (!file.exists(paste0(path, "diffusion.matrix.RData"))) {
            message(
                "'diffusion.matrix.RData' not present in:", 
                paste0(path, "diffusion.matrix.RData"), 
                ". Simulated p-values may execute slower for diffusion.")
        } else {
            load(paste0(path, "diffusion.matrix.RData"))
            diffusion.matrix <- get("diffusion.matrix")
            
            F.DATA@diffusion@matrix <- diffusion.matrix
        }
        
    } else {
        message(
            "'diffusion.matrix.RData' not loaded. ", 
            "Simulated p-values may execute slower for diffusion.")
    }
    message("Done.")
    
    # Load diffusion rowsums for z-score calculation 
    message("Loading rowSums...")
    if (file.exists(paste0(path, "diffusion.rowSums.RData")) ) {
        load(paste0(path, "diffusion.rowSums.RData"))
        diffusion.rowSums <- get("diffusion.rowSums")
        diffusion.squaredRowSums <- get("diffusion.squaredRowSums")
        
        F.DATA@diffusion@rowSums <- diffusion.rowSums
        F.DATA@diffusion@squaredRowSums <- diffusion.squaredRowSums
    } else {
        message(
            "'diffusion.rowSums.RData' not present in:", 
            paste0(path, "diffusion.rowSums.RData"), 
            ". Z-scores won't be available for diffusion.")
    }
    message("Done.")
    
    # Load matrix for pagerank
    message("Loading pagerank data...")
    message("Loading matrix...")
    if (identical(loadMatrix, "pagerank") || 
        identical(loadMatrix, "all")) {
        if (!file.exists(paste0(path, "pagerank.matrix.RData"))) {
            message(
                "'pagerank.matrix.RData' not present in:", 
                paste0(path, "pagerank.matrix.RData"), 
                ". Simulated p-values may execute slower for pagerank.")
        } else {
            load(paste0(path, "pagerank.matrix.RData"))
            pagerank.matrix <- get("pagerank.matrix")
            
            F.DATA@pagerank@matrix <- pagerank.matrix
        }
        
    } else {
        message(
            "'pagerank.matrix.RData' not loaded. ", 
            "Simulated p-values may execute slower for pagerank.")  
    }
    message("Done.")
    
    # Load pagerank rowsums for z-score calculation
    message("Loading rowSums...")
    if (file.exists(paste0(path, "pagerank.rowSums.RData")) ) {
        load(paste0(path, "pagerank.rowSums.RData"))
        pagerank.rowSums <- get("pagerank.rowSums")
        pagerank.squaredRowSums <- get("pagerank.squaredRowSums")
        
        F.DATA@pagerank@rowSums <- pagerank.rowSums
        F.DATA@pagerank@squaredRowSums <- pagerank.squaredRowSums
    } else {
        message(
            "'pagerank.rowSums.RData' not present in:", 
            paste0(path, "pagerank.rowSums.RData"), 
            ". Z-scores won't be available for pagerank.") 
    }
    message("Done.")
    
    F.DATA@keggdata@status <- "loaded"
    
    message("Data successfully loaded.")
    return(F.DATA)
}