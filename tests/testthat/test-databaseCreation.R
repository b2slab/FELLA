context("FELLA.DATA database creation from graph")

test.env <- environment()
data("FELLA.sample", package = "FELLA", envir = test.env)

graph.db <- try(FELLA:::getGraph(FELLA.sample))
dir.tmp <- tempdir()
dir.append <- paste(sample(letters, 40, replace = T), collapse = "")

# make sure KEGGREST tests are not run in a windows i386 R installation 
# (memory usage exceeds the 3GB limit)
check_win32 <- function() {
    sys <- Sys.info()
    if (grepl("i386", sys["machine"]) & grepl("Windows", sys["sysname"])) {
        msg <- paste0(
            "32-bit Windows installations may hit ", 
            "the allocation limit when using buildGraphFromKEGGREST"
        )
        skip(msg)
    }
}

# Takes some time
test_that("The graph can be parsed from KEGGREST", {
    check_win32()
    
    expect_error({
        g.full <- buildGraphFromKEGGREST(organism = "gla")
    }, NA)

    expect_is(g.full, "igraph")
})

# Use the small graph here
test_that("The database can be built (external)", {
    db_dir <- paste0(tempdir(), "/", dir.append)
  
    # First time: success (dir does not exist)
    expect_error({
        FELLA.test.external <- buildDataFromGraph(
            keggdata.graph = graph.db, 
            databaseDir = db_dir, 
            internalDir = FALSE, 
            matrices = c("hypergeom", "diffusion", "pagerank"), 
            normality = c("diffusion", "pagerank"), 
            dampingFactor = 0.7,
            niter = 10
        )
    }, NA)
    
    # Second time: fails (not to overwrite db)
    expect_error({
        FELLA.test.external.bis <- buildDataFromGraph(
            keggdata.graph = graph.db, 
            databaseDir = db_dir, 
            internalDir = FALSE, 
            matrices = c("hypergeom", "diffusion", "pagerank"), 
            normality = c("diffusion", "pagerank"), 
            dampingFactor = 0.7,
            niter = 10
        )
    }, "exists")
    
    expect_error({
        read.test.external <- 
            loadKEGGdata(
                databaseDir = db_dir, 
                internalDir = FALSE, 
                loadMatrix = c("diffusion", "pagerank")
            )
    }, NA)
  
    expect_true(FELLA.test.external)
    expect_true(is.FELLA.DATA(read.test.external))
})

test_that("The database can be built and listed (internal)", {
    expect_error({
        FELLA.test.internal <- buildDataFromGraph(
            keggdata.graph = graph.db, 
            databaseDir = dir.append, 
            internalDir = TRUE, 
            matrices = c("hypergeom", "diffusion", "pagerank"), 
            normality = c("diffusion", "pagerank"), 
            dampingFactor = 0.7,
            niter = 10
        )
    }, NA)
    
    expect_error({
        read.test.internal <- loadKEGGdata(
            databaseDir = dir.append, 
            internalDir = TRUE, 
            loadMatrix = c("diffusion", "pagerank")
        )
    }, NA)

    expect_error({
        db.name <- listInternalDatabases(full.names = FALSE)
        db.path <- listInternalDatabases(full.names = TRUE)
    }, NA)
    
    expect_true(FELLA.test.internal)
    expect_true(is.FELLA.DATA(read.test.internal))
    
    expect_true(dir.append %in% db.name)
    unlink(db.path[grepl(dir.append, db.path)], recursive = TRUE)
})

