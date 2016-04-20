context("FELLA.USER creation from compounds in lists")

# Load data to test
# test.env <- new.env(parent = environment())
test.env <- environment()
data("FELLA.sample", package = "FELLA", envir = test.env)
data("input.sample", package = "FELLA", envir = test.env)


background.sample <- try(FELLA:::getCom(FELLA.sample, 5))


test_that("All compounds map, personalised background", {
  FELLA.test <- try(defineCompounds(compounds = input.sample, 
                                    compoundsBackground = background.sample, 
                                    data = FELLA.sample))
  
  expect_s4_class(FELLA.test, "FELLA.USER")
  expect_equivalent(getInput(object = FELLA.test), 
                    input.sample)
  expect_equivalent(getBackground(object = FELLA.test), 
                    background.sample)
  expect_length(getExcluded(object = FELLA.test), 
                0)
})

test_that("All compounds map, no background", {
  expect_message(FELLA.test <- try(defineCompounds(compounds = input.sample, 
                                    compoundsBackground = NULL, 
                                    data = FELLA.sample)), 
                 "No background compounds specified")
  
  expect_s4_class(FELLA.test, "FELLA.USER")
  expect_equivalent(getInput(object = FELLA.test), 
                    input.sample)
  expect_length(getBackground(object = FELLA.test), 
                0)
  expect_length(getExcluded(object = FELLA.test), 
                0)
})

test_that("Not all compounds map, no background", {
  intruder.sample <- paste0("intruder", 1:10)
  expect_warning(FELLA.test <- try(defineCompounds(compounds = c(input.sample, intruder.sample), 
                                                   compoundsBackground = NULL, 
                                                   data = FELLA.sample)), 
                "excluded")
  
  expect_s4_class(FELLA.test, "FELLA.USER")
  expect_equivalent(sort(getInput(object = FELLA.test)), 
                    sort(input.sample))
  expect_length(getBackground(object = FELLA.test), 
                0)
  expect_equivalent(sort(getExcluded(object = FELLA.test)), 
                    sort(intruder.sample))
})

test_that("Not all compounds map, background with mapping issues too", {
  intruder1.sample <- paste0("intruder", 1:5)
  intruder2.sample <- paste0("intruder", 1:10)
  expect_warning(FELLA.test <- try(defineCompounds(compounds = c(input.sample, intruder1.sample), 
                                                   compoundsBackground = c(background.sample, intruder2.sample), 
                                                   data = FELLA.sample)), 
                 "excluded")
  
  expect_s4_class(FELLA.test, "FELLA.USER")
  expect_equivalent(sort(getInput(object = FELLA.test)), 
                    sort(input.sample))
  expect_equivalent(sort(getBackground(object = FELLA.test)), 
                    sort(background.sample))
  expect_equivalent(sort(getExcluded(object = FELLA.test)), 
                    sort(intruder1.sample))
})

# method <- "diffusion"
# approx <- "normality"
# FELLA.object <- FELLA::enrich(compounds = input.sample,
#                               method = method, 
#                               approx = approx, 
#                               data = FELLA.sample)
# 
# 
# 
# getCom(data = FELLA.sample, level = "compound")
# summary(getPvalues(object = FELLA.object, type = "diffusion"))
