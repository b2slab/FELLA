# News log

---

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