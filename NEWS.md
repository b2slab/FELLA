# News log

---

### Version 0.4.9

* Small changes in plotting functions

### Version 0.4.8

* Fixed the `FELLA.sample` file so that it contains the metadata
* Added the `getInfo` function for getting the metabata, with corresponding
unit testing
* Fixed minor writing mistakes
* `loadMatrix` can no longer be `"all"`
* `AllArguments.R` now has `loadMatrix`, `databaseDir`, `internalDir`
* Shiny app:
  - Fixed the database error when no database is present
  - Fixed table error when the graph was empty (e.g. hypergeom)
  - Adding a GO term is now functional again. Added several biomaRt/GO options.
  - GO term changes plot legend
  - Changes in the UI (text, new widgets for GO term)
  - GO example autofills all the new widgets

### Version 0.4.7

* Improved vignette text and added Bioconductor style
* Added `BiocStyle` as a dependency

To do:

* General check of arguments and documentation
* Quantitative input?
* Elaborate on this vignette or build another one for publication

### Version 0.4.6

* Removed the infamous `splitByCC` part (from `checkArguments`, 
the conditional on the `plot` method and from the vignette)
* Now the option is to filter out small CCs but still work with a single graph
(`thresholdConnectedComponent` still exists). Therefore, function
`generateResultsGraph` can only return a graph, being more intuitive.

### Version 0.4.5

* Modified size p-value computation. Now it is more efficient and consistent

### Version 0.4.4

* Fused `checkArguments` into `AllArguments.R`
* Deleted the generic `summary` as it is not necessary. 
Now the same can be achieved using `show` only, and this avoids 
method dispatch issues. 

### Version 0.4.3

* Added the `summary` method for `FELLA.DATA` objects
* Fixed some dependencies in methods
* Separated `mytriangle` in function `plotGraph`
* Renamed functions (`is-.R`, `plotLegend.R`)
* Updated sample data documentation

### Version 0.4.2

* Added doc template for the statistical normalisation
* Removed p-values from table
* Improved `generateResultsTable` function
* Improved `runDiffusion` and `runPagerank` at the code level and 
avoided checking arguments

### Version 0.4.1

* Added accessor to `status` and substituted all the calls
* Addition of GO terms works

### Version 0.4.0

* Removed some superfluous arguments: askplots, filename, BIMODAL
* Aggregated `get-.R` and `list-.R` functions in single files
* Cleaned part of: `AllMethods`, `exportResults`, `generate-`
* Fixed unit testing
* Fixed runnable examples
* Fixed vignette
* Rebuilt example data
* Improved argument handling. `method` can no longer be `"all"` and 
handles a unique method now, but `methods`
can pass a vector of methods instead
* Passes `check()` and `BiocCheck(".")` 

### Version 0.3.3

* Improved argument documentation (still work to do)
* Fused individual dummy functions that contained the commented parameters
* Added `list-()` functions
* Removed `p.adjust` from diffusion and pagerank
* Improved unit testing

To do:

* Fix documentation of changed functions
* Try to better isolate functions (e.g. `plot` and `generateResultsGraph`
do too many things at once...)
* Quantitative input?
* Delete repetitive code
* Use `checkArguments` in main functions
* Improve `AllMethods`: use accessors, clean code...

### Version 0.3.2

* Switched from `pValues` to `pScores` (except in `hypergeom`)

### Version 0.3.1

* Removed `rcytoscapejs` dependency

### Version 0.3.0

* Fixed vignette 
* Modifications to several functions, addressing:
  - Proper handling of GO labels - now can be used in many species
  - `addCellularComponentToGraph` is now `addGOToGraph`
  - New dependency: biomaRt
  - Plotting functions have been simplified, but still need work
  - Graph building has been improved
* Shiny app:
  - Removed tabs, has only 4 now
  - Left only interactive plot
  - Table is now from `DT`

To do:

* Fix documentation of changed functions
* Try to better isolate functions (e.g. `plot` and `generateResultsGraph`
do too many things at once...)
* Quantitative input?
* Improve function documentation (use templates) to supress warnings
* Change `rcytoscapejs` dependency to `visNetwork` (in CRAN)
* What to do about the p-values?

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