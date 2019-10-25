#' ID sanitiser function
#' 
#' Sanitise KEGG identifiers
#'
#' @param x Character vector, IDs to sanitise
#' @param category Character, one of: 
#' \code{"pathway"}, \code{"module"}, \code{"enzyme"}, 
#' \code{"ncbi"}, \code{"reaction"}, \code{"compound"}
#'
#' @return Character vector, sanitised \code{x}
#' 
#' @examples 
#' FELLA:::sanitise(c("path:hsa00010", "path:hsa00020"), "pathway", "hsa")
#' 
#' @keywords internal
sanitise <- function(x, category, organism) {
    old.attr <- attributes(x)
    if (category == "pathway") {
        ans <- gsub("(path:)(.+)(\\d{5})", paste0(organism, "\\3"), x)
    }
    if (category == "module") {
        ans <- gsub("(md:)(.*)(M\\d{5})", "\\3", x)
    }
    if (category == "enzyme") {
        ans <- ifelse(
            grepl(pattern = "-", x = x),  
            NA, 
            gsub("(ec:)(\\d+\\.\\d+\\.\\d+\\.\\d+)", "\\2", x))
    }
    if (category == "ncbi") {
        ans <- gsub("(ncbi.+:)(.*\\d+)", "\\2", x)
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

#' Infer connections to EC 
#' 
#' Function \code{infere.con2ec} infers network connections to KEGG EC 
#' families by passing through genes. This assumes that the category being 
#' mapped to enzymes is above them.
#'
#' @param ids Character vector of identifiers to map. For example, 
#' all the KEGG pathways
#' @param ent Character, entity that we are mapping 
#' (one of \code{"pathway"} and one of \code{"module"})
#' @param ent2gene Named character vector, names are the entity \code{ent} 
#' and values are genes
#' @param gene2enyzme Named character vector, names are genes and 
#' values are EC enzyme families
#' category Character, one of: 
#'
#' @return Two-column data frame. Column \code{"from"} contains the 
#' KEGG enzyme families whereas \code{"to"} contains the entity \code{ent}.
#' 
#' @examples 
#' ids <- "hsa00010"
#' ent <- "pathway"
#' ent2gene <- c("hsa00010" = "hsa:10", "hsa00010" = "hsa:120")
#' gene2enzyme <- c("hsa:10" = "1.1.1.1", "hsa:120" = "1.2.3.4")
#' FELLA:::infere.con2ec(ids, ent, ent2gene, gene2enzyme)
#' 
#' @keywords internal
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

#' Extract largest CC
#' 
#' Function \code{largestcc} extracts the 
#' largest connected component of an igraph object
#'
#' @param graph Igraph object
#' 
#' @return Connected igraph object
#' 
#' @examples 
#' library(igraph)
#' g <- barabasi.game(10) + graph.empty(10)
#' FELLA:::largestcc(g)
#' 
#' @keywords internal
largestcc <- function(graph) {
    cl <- clusters(graph)
    x <- which.max(cl$csize)
    induced.subgraph(graph, which(cl$membership == x))
}


#' @title Parse, build and load the KEGG knowledge model
#' 
#' @description 
#' Function \code{buildGraphFromKEGGREST} makes use of the KEGG 
#' REST API (requires internet connection) 
#' to build and return the curated KEGG graph. 
#' 
#' @details
#' In function \code{buildGraphFromKEGGREST}, 
#' The user specifies (i) an organism, and (ii) patterns matching 
#' pathways that should not be included as nodes. 
#' A graph object, as described in [Picart-Armada, 2017], 
#' is built from the comprehensive 
#' KEGG database [Kanehisa, 2017]. 
#' As described in the main vignette, accessible through 
#' \code{browseVignettes("FELLA")}, this graph has five levels that 
#' represent categories of KEGG nodes. 
#' From top to bottom: pathways, modules, enzymes, reactions and compounds.
#' This knowledge representation is resemblant to the one formerly 
#' used by MetScape [Karnovsky, 2011], in which enzymes connect 
#' to genes instead of modules and pathways.
#' The necessary KEGG annotations 
#' are retrieved through KEGGREST R package [Tenenbaum, 2013]. 
#' Connections between pathways/modules and enzymes are inferred through 
#' organism-specific genes, i.e. an edge is added if a gene 
#' connects both entries. 
#' However, in order to enrich metabolomics data, the user has to 
#' pass the graph object to \code{buildDataFromGraph}  
#' to obtain the \code{\link{FELLA.USER}} object. 
#' All the networks are handled with the igraph R package [Csardi, 2006].
#' 
#' 
#' @param organism Character, KEGG code for the organism of interest
#' @param filter.path Character vector, pathways to filter. 
#' This is a pattern matched using regexp. 
#' E.g: \code{"01100"} to filter 
#' the overview metabolic pathway in any species
#' 
#' @return \code{buildGraphFromKEGGREST} returns the 
#' curated KEGG graph (class \pkg{igraph})
#' 
#' @name data-funs
#' @rdname data-funs
#' 
#' @import igraph
#' @import Matrix
#' @import KEGGREST
#' @import plyr
#' @export
buildGraphFromKEGGREST <- function(
    organism = "hsa",  
    filter.path = NULL) {
    
    categories <- listCategories()
    
    # Data from KEGGREST
    # 
    # List of id-name
    message("Building through KEGGREST...")
    
    info.org <- KEGGREST::keggInfo(organism)
    info.geneannot <- grep(
        "ncbi-[[:lower:]]+", 
        capture.output(cat(info.org)), 
        value = TRUE)
    info.geneannot <- gsub("[[:space:]]", "", info.geneannot)
    cat.geneannot <- head(
        intersect(c("ncbi-geneid", "ncbi-proteinid"), info.geneannot), 
        1
    )
    if (length(cat.geneannot) == 0) 
        stop(
            "Organism ", organism, " does not appear to have either ", 
            "ncbi-geneid or ncbi-proteinid. Please contact ", 
            "FELLA's maintainer.")
    message(
        "Available gene annotations: ", 
        paste(info.geneannot, collapse = ", "), 
        ". Using ", cat.geneannot)
    
    list.list <- plyr::llply(
        stats::setNames(categories, categories), 
        function(category) {
            # only pathways are organism-specific now
            # modules are filtered through genes
            if (category %in% c("pathway")) {
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
            data.frame(id = names(categ), stringsAsFactors = FALSE), 
        .id = "category") 
    map.category <- stats::setNames(
        as.character(map.category$category), 
        map.category$id)
    
    # List of kegg links - essentially our edges
    list.link <- plyr::alply(
        expand.grid(
            categories, 
            categories, 
            KEEP.OUT.ATTRS = FALSE, 
            stringsAsFactors = FALSE)[lower.tri(
                matrix(seq_len(25), nrow = 5)), ], 
        1, 
        function(row) {
            original <- KEGGREST::keggLink(row[1], row[2])
            df <- data.frame(
                from = sanitise(original, row[1], organism), 
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
        stats::setNames(., sanitise(names(.), "pathway", organism))
    m.mod_gene <- KEGGREST::keggLink(organism, "module") %>% 
        stats::setNames(., sanitise(names(.), "module", organism))
    
    # Gene to enzyme
    m.gene_enzyme <- KEGGREST::keggLink("enzyme", organism) %>% 
        sanitise(., "enzyme", organism)
    
    # Enzyme to gene, but giving the entrez id rather than KEGG's
    
    # Map kegg to entrez
    keggGene2entrez <- KEGGREST::keggConv(cat.geneannot, organism) %>% 
        sanitise(., category = "ncbi") %>%  
        split(., names(.))
    # Map kegg enzymes to entrez
    m.enzyme_gene <- KEGGREST::keggLink(organism, "enzyme") %>% 
        stats::setNames(., sanitise(names(.), "enzyme", organism)) %>%
        # sanitise(., "gene", organism) %>% 
        split(., names(.), drop = TRUE) %>%
        plyr::llply(
            ., function(r) sort(as.character(unique(keggGene2entrez[r]))))
    
    # Inferred connections (through genes)
    con.infere <- list(
        infere.con2ec(
            names(list.list$pathway), 
            "pathway", 
            m.path_gene, 
            m.gene_enzyme),
        
        infere.con2ec(
            names(list.list$module), 
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
        graph.edgelist(matrix.adjacency, directed = TRUE)
    ) 
    
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
    
    # Same for the modules that do no belong to the species
    # Keep only those that have at least one gene associated 
    # 
    # Alternative: keggLink("genome", "compound") and pick only 
    # those of the organism code
    # The downside is that it takes around 90s, but might be 
    # a safer option
    # Checked in 19/10/2019 and both approaches are equivalent
    org.modules <- unique(names(m.mod_gene))
    g.raw <- delete.vertices(
        g.raw, 
        which((V(g.raw)$com == 2) & !(V(g.raw)$name %in% org.modules)))
    
    # Order by category and id
    g.raw <- permute.vertices(
        g.raw, 
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
        filter.out <- lapply(
            filter.path, 
            function(p) {
                which(grepl(p, names.path))
            })
        names.out <- names.path[unique(unlist(filter.out))]
        message(paste0("Filtering ", length(names.out), " pathways."))
        g.raw <- delete.vertices(g.raw, names.out)
    }
    
    g.raw <- largestcc(g.raw)
    
    message("Done.")
    
    message("Pruning graph...")
    # CURATE GRAPH
    # We start with the graph curation
    edges.split <- split(seq_len(ecount(g.raw)), E(g.raw)$weight)
    
    message(paste0("Current weight: 1 out of 4..."))
    g.curated <- subgraph.edges(
        graph = g.raw, 
        eids = edges.split[[1]], 
        delete.vertices = FALSE)
    
    for (w in names(edges.split)[-1]) {
        current.w <- as.numeric(w)
        message(paste0("Current weight: ", w, " out of 4..."))
        
        dist.matrix <- distances(g.curated, mode = "out")
        list.edges <- edges.split[[w]]
        
        list.ends <- ends(g.raw, list.edges)
        new.edges <- dist.matrix[list.ends] > E(g.raw)$weight[list.edges]
        
        g.curated <- add.edges(
            graph = g.curated, 
            edges = t(list.ends[new.edges, ]), 
            attr = list(weight = E(g.raw)[list.edges[new.edges]]$weight))
    }
    
    # Final edge weights have to be inverted
    E(g.curated)$weight <- 1/E(g.curated)$weight
    
    tmp <- list.list
    names(tmp) <- NULL
    tmp <- unlist(tmp)
    
    # Tried sorting according to number of characters 
    # (take shortest names). This is weird, as some names 
    # are not very known. I will leave the original order
    V(g.curated)$NAME <- strsplit(tmp[V(g.curated)$name], split = "; ")
    V(g.curated)$entrez <- m.enzyme_gene[V(g.curated)$name]
    
    comment(g.curated) <- info.org
    g.curated$organism <- organism
    
    message("Done.")
    
    keggdata.graph <- g.curated
    
    return(keggdata.graph)
}
