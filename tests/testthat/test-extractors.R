context("Extractor functions")

# Load data to test
# test.env <- new.env(parent = environment())
test.env <- environment()
data("FELLA.sample", package = "FELLA", envir = test.env)
data("input.sample", package = "FELLA", envir = test.env)

test_that("getCom extractor", {
  categories <- c(as.list(1:5), 
                  list("pathway", 
                       "module", 
                       "enzyme", 
                       "reaction", 
                       "compound"))
  lapply(categories, function(categ) {
    
    temporary <- try(FELLA:::getCom(data = FELLA.sample, 
                                    level = categ, 
                                    format = "name"))
  
    expect_is(temporary, "character")
    expect_gt(length(temporary), 0)
    
    temporary <- try(FELLA:::getCom(data = FELLA.sample, 
                                    level = categ, 
                                    format = "id"))
    
    expect_is(temporary, "integer")
    expect_named(temporary)
    expect_gt(length(temporary), 0)
    
    invisible()
  })
})

test_that("getGraph extractor", {
  expect_is(FELLA:::getGraph(FELLA.sample), "igraph")
})

test_that("getMatrix extractor", {
  expect_is(FELLA:::getMatrix(FELLA.sample, "hypergeom"), "lgCMatrix")
  expect_is(FELLA:::getMatrix(FELLA.sample, "diffusion"), "matrix")
  expect_is(FELLA:::getMatrix(FELLA.sample, "pagerank"), "matrix")
})

test_that("getName extractor", {
  temp <- FELLA:::getCom(FELLA.sample, level = 3)
  ans_temp <- FELLA::getName(FELLA.sample, id = temp)
  
  expect_is(ans_temp, "list")
  expect_gt(length(ans_temp), 0)
  expect_is(ans_temp[[1]], "character")
  expect_gt(length(ans_temp[[1]]), 0)
})

test_that("getSums extractor", {
  for (method in c("diffusion", "pagerank")) {
    for (squared in c(TRUE, FALSE)) {
      sums <- FELLA:::getSums(data = FELLA.sample, 
                             type = "diffusion", 
                             squared = FALSE)
  
      expect_named(sums)
      expect_is(sums, "numeric")
      expect_gt(length(sums), 0)
      expect_true(all(sums > 0))
    }
  }
})

