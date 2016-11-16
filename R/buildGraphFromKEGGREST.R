#' Generate the KEGG graph 
#' 
#' Function \code{buildGraphFromKEGG} returns the curated KEGG graph, necessary 
#' to build the other KEGG RData files and, ultimately, build the 
#' \code{\link[FELLA]{FELLA.USER}} object. This procedure should only be used once, and 
#' might take some amount of time.
#' 
#' @param organism Character, KEGG code for the organism of interest
#' @param filter.path Character vector, pathways to filter. This is a pattern 
#' matched using regexp. E.g: '01100' to filter metabolic pathways in any species
#' @param GOterms_hsa Logical value, should GO terms be added to the KEGG enzyme 
#' category? This is performed by default, and allows the GO 
#' similarity analysis for the enrichment output
#' 
#' @return Curated KEGG graph (class \code{\link[igraph]{igraph}})
#' 
#' @import igraph
#' @import Matrix
#' @import KEGGREST
#' @import plyr
#' @export
# by now out - why do I need it? # @import dplyr
# @importFrom org.Hs.eg.db org.Hs.egGO ## maybe gives trouble because we use annotationdbi too?
# @importFrom org.Hs.eg.db org.Hs.egGO
# @importFrom AnnotationDbi as.list Term
buildGraphFromKEGGREST <- function(
  organism = "hsa",  
  filter.path = NULL, 
  GOterms_hsa = TRUE) {

# library(KEGGREST)
# library(plyr)
# library(dplyr)
# library(igraph)

  # organism <- "hsa"
  
  categories <- c("pathway", "module", "enzyme", "reaction", "compound")
  
  # Get rid of hsa:, cpd:....
  sanitise <- function(x, category, organism) {
    old.attr <- attributes(x)
    if (category == "pathway") {
      ans <- gsub("(path:)(.+)(\\d{5})", paste0(organism, "\\3"), x)
    }
    if (category == "module") {
      ans <- gsub("(md:)(.*)(M\\d{5})", "\\3", x)
    }
    if (category == "enzyme") {
      ans <- ifelse(grepl(pattern = "-", x = x),  
                    NA, 
                    gsub("(ec:)(\\d+\\.\\d+\\.\\d+\\.\\d+)", "\\2", x))
    }
    if (category == "gene") {
      ans <- gsub(paste0("(", organism, ":)(.*\\d+)"), "\\2", x)
    }
    if (category == "reaction") {
      ans <- gsub("(rn:)(R\\d{5})", "\\2", x)
    }
    if (category == "compound") {
      ans <- gsub("(cpd:)(C\\d{5})", "\\2", x)
    }
    
    attributes(ans) <- old.attr
    ans
  }
  
  # Connections through genes
  infere.con2ec <- function(ids, ent, ent2gene, gene2enzyme) {
    ans <- plyr::ldply(
      ids, 
      function(x) {
        aux <- ent2gene[names(ent2gene) == x]
        ans <- unique(gene2enzyme[names(gene2enzyme) %in% aux])
        # names(ans) <- rep(x, length(ans))
        data.frame(from = ans, to = rep(x, length(ans)))
      }) 
    
    attr(ans, "from") <- "enzyme"
    attr(ans, "to") <- ent
    ans
  }  
  
  
  # Data from KEGGREST
  # 
  # List of id-name
  message("Building through KEGGREST...")
  
  list.list <- plyr::llply(
    setNames(categories, categories), 
    function(category) {
      if (category %in% c("pathway", "module")) {
        ans <- KEGGREST::keggList(
          database = category, 
          organism = organism)
      } else {
        ans <- KEGGREST::keggList(database = category)
      }
      names(ans) <- sanitise(names(ans), category, organism)
      
      ans
    }, 
    .progress = "text"
  )
  
  # Map identifiers to category 
  map.category <- plyr::ldply(
    list.list, 
    function(categ) 
      data.frame(id = names(categ), 
                 stringsAsFactors = FALSE), 
    .id = "category") 
  map.category <- setNames(as.character(map.category$category), 
                           map.category$id)
  
  # List of kegg links - essentially our edges
  list.link <- plyr::alply(
    expand.grid(categories, 
                categories, 
                KEEP.OUT.ATTRS = FALSE, 
                stringsAsFactors = FALSE)[lower.tri(matrix(1:25, nrow = 5)), ], 
    1, 
    function(row) {
      # browser()
      original <- KEGGREST::keggLink(row[1], row[2])
      df <- data.frame(from = sanitise(original, row[1], organism), 
                       to = sanitise(names(original), row[2], organism))
      attr(df, "from") <- as.character(row[1])
      attr(df, "to") <- as.character(row[2])
      
      df
    }, 
    .progress = "text"
  )
  attributes(list.link) <- NULL
  
  # To mine mapping through enzymes
  m.path_gene <- KEGGREST::keggLink(organism, "pathway") %>% 
    setNames(., sanitise(names(.), "pathway", organism))
  m.mod_gene <- KEGGREST::keggLink(organism, "module") %>% 
    setNames(., sanitise(names(.), "module", organism))
  
  m.gene_enzyme <- KEGGREST::keggLink("enzyme", organism) %>% 
    sanitise(., "enzyme", organism)
  m.enzyme_gene <- KEGGREST::keggLink(organism, "enzyme") %>% 
    setNames(., sanitise(names(.), "enzyme", organism)) %>%
    sanitise(., "gene", organism) %>% 
    split(., names(.), drop = TRUE) %>%
    plyr::llply(., function(r) sort(as.character(r)))
  
  
  # Inferred connections (through genes)
  con.infere <- list(
    infere.con2ec(names(list.list$pathway), 
                            "pathway", 
                            m.path_gene, 
                            m.gene_enzyme),
    
    infere.con2ec(names(list.list$module), 
                           "module", 
                           m.mod_gene, 
                           m.gene_enzyme)
  )
  
  # Direct connections
  df.noinfere <- plyr::ldply(
    list.link, 
    function(df.piece) {
      a.from <- attr(df.piece, "from")
      a.to <- attr(df.piece, "to")
      
      if (a.from == "enzyme" & (a.to %in% c("module", "pathway"))) 
        return(NULL)
      # browser()
      return(df.piece)
    }, 
    .id = NULL
  )
  
  df.infere <- plyr::ldply(
    con.infere, 
    function(df.piece) {
      a.from <- attr(df.piece, "from")
      a.to <- attr(df.piece, "to")
      
      
      return(df.piece)
    } 
  )
  
  matrix.adjacency <- as.matrix(rbind(
    df.noinfere, 
    df.infere
  ))
  
  message("Done.")
  
  message("Building graph...")
  g.raw <- igraph::simplify(
    graph.edgelist(matrix.adjacency, 
                   directed = TRUE)) 
     
  V(g.raw)$com <- match(map.category[V(g.raw)$name], categories) 
  
  # Nodes without a kegg name are either obsolete or inexistent
  g.raw <- delete.vertices(
    g.raw, 
    which(is.na(V(g.raw)$com)))
  
  # Enzymes that cannot be inferred should be deleted 
  # (not found in desired species!)
  g.raw <- delete.vertices(
    g.raw, 
    which((V(g.raw)$com == 3) & !(V(g.raw)$name %in% df.infere$from)))
  
  # Order by category and id
  g.raw <- permute.vertices(g.raw, 
                            order(order(V(g.raw)$com, V(g.raw)$name)))
  
  # Weighting the edges
  tmp <- get.edges(g.raw, E(g.raw))
  E(g.raw)$weight <- abs(V(g.raw)$com[tmp[, 1]] - V(g.raw)$com[tmp[, 2]])
  
  # Keep only reactions in a pathway
  # i.e. delete reactions that don't have any 3-weight edge
  g.raw <- (setdiff(
    which(V(g.raw)$com == 4), 
    get.edges(g.raw, E(g.raw)[E(g.raw)$weight == 3])[, 1]) %>%
      delete.vertices(graph = g.raw, .))
  
  # Keep only compounds that are reactants/products in these reactions
  # i.e. delete compounds that don't have any 1-weight edge
  g.raw <- (setdiff(
    which(V(g.raw)$com == 5), 
    get.edges(g.raw, E(g.raw)[E(g.raw)$weight == 1])[, 1]) %>%
      delete.vertices(graph = g.raw, .)) 
  
  # Other filtering (remove nodes?)
  if (!is.null(filter.path)) {
    names.path <- V(g.raw)[V(g.raw)$com == 1]$name
    filter.out <- lapply(filter.path, 
                         function(p) {
                            which(grepl(p, names.path))
                         })
    names.out <- names.path[unique(unlist(filter.out))]
    message(paste0("Filtering ", length(names.out), " pathways."))
    g.raw <- delete.vertices(g.raw, names.out)
  }
  
  largestcc <- function(graph) {
    cl <- clusters(graph)
    x <- which.max(cl$csize)
    induced.subgraph(graph, which(cl$membership == x))
  }
  g.raw <- largestcc(g.raw)
  
  message("Done.")
  
  message("Pruning graph...")
  # CURATE GRAPH
  # We start with the graph curation
  edges.split <- split(1:ecount(g.raw), E(g.raw)$weight)
  
  message(paste0("Current weight: 1 out of 4..."))
  g.curated <- subgraph.edges(
    graph = g.raw, 
    eids = edges.split[[1]], 
    delete.vertices = FALSE)
  
  for (w in names(edges.split)[-1]) {
    current.w <- as.numeric(w)
    message(paste0("Current weight: ", w, " out of 4..."))
    # browser()
    dist.matrix <- distances(g.curated, mode = "out")
    list.edges <- edges.split[[w]]
    
    list.ends <- ends(g.raw, list.edges)
    new.edges <- dist.matrix[list.ends] > E(g.raw)$weight[list.edges]
    
    g.curated <- add.edges(
      graph = g.curated, 
      edges = t(list.ends[new.edges, ]), 
      attr = list(weight = E(g.raw)[list.edges[new.edges]]$weight))
  }
  rm(current.w, dist.matrix, list.edges, list.ends, new.edges)
  # gc()
  
  # Final edge weights have to be inverted
  E(g.curated)$weight <- 1/E(g.curated)$weight
  
  tmp <- list.list
  names(tmp) <- NULL
  tmp <- unlist(tmp)
  V(g.curated)$NAME <- lapply(
    strsplit(tmp[V(g.curated)$name], split = "; "), 
    function(names) {
      names[order(nchar(names))]
  })
  V(g.curated)$GENE <- m.enzyme_gene[V(g.curated)$name]
  
  comment(g.curated) <- KEGGREST::keggInfo(organism)
  
  message("Done.")
  
  if (GOterms_hsa) {
    # GO terms
    message("Adding GO terms to enzymes...")
    
    if (!requireNamespace("org.Hs.eg.db", "AnnotationDbi", 
                          quietly = TRUE)) {
      stop(
        "Packages org.Hs.eg.db, AnnotationDbi ", 
        "must be installed to add GO terms", 
        call. = FALSE)
    }
    
    # library(org.Hs.eg.db)
    gene.all <- V(g.curated)$GENE
    gene2GO <- AnnotationDbi::as.list(org.Hs.eg.db::org.Hs.egGO)
    
    V(g.curated)$GO <- plyr::llply(
      V(g.curated)$GENE, 
      function(gene) {
        if (!is.null(gene)) {
          # browser()
          tmp <- gene2GO[gene]
          names(tmp) <- NULL
          go_terms <- names(unlist(tmp, recursive = FALSE))
          if (!is.null(go_terms)) {
            return(AnnotationDbi::Term(go_terms))
          }
          return(NA)
        }
        return(NA)
      })
    message("Done.")
  }

  keggdata.graph <- g.curated
  
  return(keggdata.graph)
}