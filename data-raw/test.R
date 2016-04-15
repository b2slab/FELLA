# library(FELLA)
library(devtools)
document()

FELLA.DATA <- loadKEGGdata(path = "~/Datasets/FELLAdata/", 
                           loadMatrix = "none")

load("~/Rstuffbro/DiffusionArticle/data/compounds.metabolon.RData")

method <- "diffusion"
approx <- "normality"
FELLA.USER <- enrich(compounds = compounds.metabolon, 
                       method = method, 
                       approx = approx, 
                       data = FELLA.DATA)

getCom(data = FELLA.DATA, level = "compound")
summary(getPvalues(object = FELLA.USER, type = "diffusion"))
