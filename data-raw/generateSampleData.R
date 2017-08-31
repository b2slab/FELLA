library(igraph)

# path <- "~/Datasets/FELLAdataslim/"
matrices <- c("hypergeom")

# Only the graph is needed, then using FELLA's functions we will
# generate the data from a toy graph
db <- loadKEGGdata()
keggdata.graph <- getGraph(db)
metadata <- comment(keggdata.graph)
# keggdata.graph <- buildGraphFromKEGGREST()
# load(paste0(path, "keggdata.graph.RData"))

#############################################
# Code from Sam Steingold at list.nongnu.org
graph.reverse <- function (graph) {
  if (!is.directed(graph))
    return(graph)
  e <- get.data.frame(graph, what="edges")
  ## swap "from" & "to"
  neworder <- 1:length(e)
  neworder[1:2] <- c(2,1)
  e <- e[neworder]
  names(e) <- names(e)[neworder]
  graph.data.frame(e, vertices = get.data.frame(graph, what="vertices"))
}
#############################################

keggdata.graph <- graph.reverse(keggdata.graph)

# First, let's build a toy graph with two close pathways
pathway <- c("hsa00640", "hsa00010")

# all the nodes under the umbrella of those two guys
d <- distances(keggdata.graph, mode = "out", v = pathway)
any <- apply(d, 2, function(col) any(col < 5))
newnodes <- colnames(d)[any]

# sample input
id.compounds <- V(keggdata.graph)[V(keggdata.graph)$com == 5]$name
any.firstpath <- intersect(colnames(d)[d[1, ] < 5], id.compounds)

set.seed(1)
input.sample <- sample(any.firstpath, 30)

# Leave original weights
graph.sample <- graph.reverse(induced.subgraph(keggdata.graph, newnodes))
comment(graph.sample) <- metadata

# Use a temporary directory to save the data and reload it later
tmpdir <- paste0(tempdir(), "/databuild/")
# FELLA::buildDataFromGraph(graph.toy, outputDir = tmpdir, niter = 11)

# Generate the data without diffusion/pagerank matrices
FELLA::buildDataFromGraph(
    graph.sample, 
    databaseDir = tmpdir, 
    internalDir = FALSE, 
    matrices = matrices, 
    niter = 100)

list.files(tmpdir)
e.temp <- new.env(parent = globalenv())
sapply(
    list.files(tmpdir, full.names = T), 
    function(link) load(link, envir = e.temp))
# save(list = ls(e), file = "data-raw/test.RData", envir = e.temp)

FELLA.sample <- FELLA::loadKEGGdata(tmpdir, internalDir = FALSE)

# test it...
k <- enrich(
    compounds = input.sample, 
    method = "diffusion", 
    approx = "normality", 
    data = FELLA.sample)
plot(k, data = FELLA.sample, 
     method = "diffusion", 
     threshold = .1)


# it seems to work! 
# save them as loadable data examples
save(FELLA.sample, file = "data/FELLA.sample.RData")
save(input.sample, file = "data/input.sample.RData")
