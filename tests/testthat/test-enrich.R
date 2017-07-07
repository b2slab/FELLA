context("Enrichment analysis (wrapper enrich)")

# Load data to test
# test.env <- new.env(parent = environment())
test.env <- environment()
data("FELLA.sample", package = "FELLA", envir = test.env)
data("input.sample", package = "FELLA", envir = test.env)


test_that("Enrichment wrapper", {
  nnodes <- try(igraph::vcount(getGraph(FELLA.sample)))
  npaths <- length(getCom(FELLA.sample, "pathway"))
  
  for (method in c("hypergeom", "diffusion", "pagerank")) {
    for (approx in c("normality", "simulation", "t", "gamma")) {
      FELLA.test <- enrich(
        compounds = input.sample, 
        method = method, 
        approx = approx, 
        niter = 100, 
        data = FELLA.sample)
      
      expect_s4_class(FELLA.test, "FELLA.USER")
      
      pval <- getPscores(FELLA.test, method)
      
      expect_named(pval)
      expect_is(pval, "numeric")
      if (method == "hypergeom") {
        expect_length(pval, npaths)
      } else {
        expect_length(pval, nnodes)
      }
      expect_true(all(pval >= 0 & pval <= 1))
    }
  }
})