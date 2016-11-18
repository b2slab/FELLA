context("FELLA.DATA database creation from graph")

test.env <- environment()
data("FELLA.sample", package = "FELLA", envir = test.env)

graph.db <- try(FELLA:::getGraph(FELLA.sample))
dir.tmp <- tempdir()

# Takes some time
test_that("The graph can be parsed from KEGGREST", {
  g.full <- try(
    buildGraphFromKEGGREST()
  )
  
  expect_is(g.full, "igraph")
})

# Use the small graph here
test_that("The database can be built (external)", {
  FELLA.test.external <- try(
    buildDataFromGraph(
      keggdata.graph = graph.db, 
      databaseDir = dir.tmp, 
      internalDir = FALSE, 
      matrices = c("hypergeom", "diffusion", "pagerank"), 
      normality = c("diffusion", "pagerank"), 
      dampingFactor = 0.7,
      niter = 10
    )
  )
  read.test.external <- try(
    loadKEGGdata(
      databaseDir = dir.tmp, 
      internalDir = FALSE, 
      loadMatrix = "all"
    )
  )
  
  expect_true(FELLA.test.external)
  expect_true(is.FELLA.DATA(read.test.external))
})

test_that("The database can be built and listed (internal)", {
  FELLA.test.internal <- try(
    buildDataFromGraph(
      keggdata.graph = graph.db, 
      databaseDir = "eraseme", 
      internalDir = TRUE, 
      matrices = c("hypergeom", "diffusion", "pagerank"), 
      normality = c("diffusion", "pagerank"), 
      dampingFactor = 0.7,
      niter = 10
    )
  )
  read.test.internal <- try(
    loadKEGGdata(
      databaseDir = "eraseme", 
      internalDir = TRUE, 
      loadMatrix = "all"
    )
  )
  db.name <- listInternalDatabases(full.names = FALSE)
  db.path <- listInternalDatabases(full.names = TRUE)
  
  expect_true(FELLA.test.internal)
  expect_true(is.FELLA.DATA(read.test.internal))
  
  expect_true("eraseme" %in% db.name)
  unlink(db.path, recursive = TRUE)
})

