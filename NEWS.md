# News log

---

### Version 0.2.6

* Fixed indentation
* Fixed long lines
* Added NEWS file
* Fixed examples

To do:

* **Vignette breaks**
* Quantitative input?
* Improve function documentation (use templates) to supress warnings
* Change `rcytoscapejs` dependency to `visNetwork` (in CRAN)
* What to do about the p-values?

### Version 0.2.5

Some minor changes to accomodate Bioconductor's standards:

* Update `R` version
* Add `R` file to the `testthat` folder
* Add GO biocView

To do from Bioconductor:

* **Vignette breaks**
* Add NEWS file
* Fix indentation
* Fix long lines (<80)
* Add runnable examples: 
  - buildDataFromGraph
  - buildGraphFromKEGGREST
  - loadKEGGdata

To do:

* Quantitative input?
* Improve function documentation (use templates) to supress warnings
* Change `rcytoscapejs` dependency to `visNetwork` (in CRAN)


### Version 0.2.4

Minor tweaks in the plotting function. Has more flexibility and can use 
tkplot as well (for image generation purposes), or force layout. This 
function should be really improved, it's not that hard...

### Version 0.2.3

* Fixed CREB website link
* Added the option to specify the damping factor in pagerank
* Changed default damping factor to 0.85


### Version 0.2.2

* Fixed table export (```stringsAsFactors``` was not disabled when creating the 
data.frame).
* Added the function ```generateEnzymesTable``` to build a table with the 
EC numbers, along with the genes (entrez) within them and the associated 
[GO labels](http://www.geneontology.org/).
* The shiny app exports tables with the full enzyme data and 
simply as a text gene list as well. 

### Version 0.2.1

The shiny app has been updated. 

* Now the rcytoscape part has clickable links. 
This is due to a change in the 
[r-cytoscape.js](https://github.com/cytoscape/r-cytoscape.js/) package that 
changes the **href** node attribute to **tooltip**.
* The colors and shapes have been updated as well, to be consistent with 
our notation. 

Also, the labels of the nodes are not sorted by length. 
Short labels can be really weird...

---

## Version 0.2.0

Added the shiny app as a developer option. It requires the following packages:

* shiny
* DT
* xtable
* knitr
* rcytoscapejs

---

## First pre-launch version - 0.0.999

The first version of this package contains: 

* Basic enrichment functions: hypergeom, diffusion, pagerank
* Null models: normal or Monte Carlo
* Export options: to tables or graphs
* Plotting capabilities
* Possibility of custom background

Further versions should include interactive plots and applications for an easier usage