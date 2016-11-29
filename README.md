# Enriching metabolomics data through diffusion


This repository contains the `FELLA` package. `FELLA` is a metabolomics data enrichment tool that contextualises the experimental results using KEGG reactions, enzymes, modules and pathways. 

- The input for our package is a __list of affected metabolites__ between experimental conditions. 
- The layout of the analysis is in a __comprehensive human-readable layout__, exportable to several formats, containing a biological interpetation of the experiment. 

The subnetwork displayed to the user is found using diffusive processes on a graph that represents the known biological annotations at several molecular levels. To use this package type in your terminal: 

```bash
R CMD build FELLA
R CMD INSTALL FELLA
```

Alternatively, you can use `devtools` if you experience some trouble building the vignette. Working in the package directory, this should do the trick:

```r
devtools::install(build_vignettes = T)
```

Once `FELLA` is installed, you may load it by typing in your R terminal

```r
library("FELLA")
```

To get the global picture about `FELLA` usage, you may browse its __vignette__

```r
browseVignettes("FELLA")
```

All of the functions in `FELLA` have a (very basic) documentation, inclusive 
the package and the sample data `FELLA.sample` and `input.sample`.

In addition, there is a shiny app to facilitate the 
usage of the package. Before launching it, a 
database should be built. For example, for Homo sapiens 
excluding the hsa01100 pathway:

```r
g <- buildGraphFromKEGGREST(
  organism = "hsa", 
  filter.path = "hsa01100", 
  GOterms_hsa = TRUE
)
buildDataFromGraph(
  g, 
  niter = 10)
```

Then we can launch the shiny app 

```r
FELLA:::launchApp(
  host = "127.0.0.1", 
  port = 8888
)
```

and leave the command active. Going to the direction 
[127.0.0.1:8888](127.0.0.1:8888) will start the analysis.