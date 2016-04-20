library(FELLA)
library(devtools)
# document()

data("FELLA.sample")
data("input.sample")

# FELLA.DATA <- loadKEGGdata(path = "~/Datasets/FELLAdata/", 
#                            loadMatrix = "none")

# load("~/Rstuffbro/DiffusionArticle/data/compounds.metabolon.RData")

method <- "diffusion"
approx <- "normality"
FELLA.object <- FELLA::enrich(compounds = input.sample,
                              method = method, 
                              approx = approx, 
                              data = FELLA.sample)



getCom(data = FELLA.sample, level = "compound")
summary(getPvalues(object = FELLA.object, type = "diffusion"))
