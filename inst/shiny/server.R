# server.R

shinyServer(function(input, output, session) {
  # Database of choice
  FELLA.DATA <- reactive({
    if (input$database != "") {
      loadKEGGdata(
        databaseDir = input$database, 
        internalDir = FALSE, 
        loadMatrix = c("diffusion", "pagerank")
      )
    }
  })
  # database summary
  output$databaseInfo <- renderText({
    data <- FELLA.DATA()
    if (is.null(data)) {
      c(
        "Database directory is empty...", 
        "\nMake sure you executed the following", 
        "\nat least once after installing FELLA:", 
        "\nbuildGraphFromKEGGREST", 
        "\nbuildDataFromGraph")
    } else {
      comment(FELLA:::getGraph(data))
    }
  })
  
  # First step: create the USER variable
  createUser <- reactive({
    if (input$radioInput == 1) {
      read.comp <- readLines("examples/input_1_compounds.list")
    } else if (input$radioInput == 2) {
      read.comp <- readLines("examples/input_2_compounds.list")
    } else if (input$radioInput == 3) {
      read.comp <- readLines("examples/input_3_metaboanalyst.list")
    } else {
      if (!is.null(input$file)) {
        read.file <- read.table(
          input$file$datapath, 
          header = FALSE, 
          stringsAsFactors = FALSE)
        read.comp <- as.character(read.file[, 1])
        read.comp <- read.comp[complete.cases(read.comp)]
        if (is.null(read.comp)) {
          return(NULL)
        }
      }
      else {
        message("No compounds uploaded yet.")
        return(NULL)
      }
    }
    data <- FELLA.DATA()
    if (is.null(data)) return(data)
    
    result <- enrich(
      compounds = read.comp, 
      method = input$method, 
      approx =input$approx, 
      niter = input$niter, 
      data = data)
    
    return(result)
  })
  
  # Show the user how the file looks like
  output$exampleInput <- renderText(
    paste(
      readLines("examples/input_2_compounds.list"), 
      collapse = "\n"
    )
  )
  
  # Second: the summary of the compounds in the input: 
  # read names, mapped and unmapped metabolites
  inputSummary <- reactive({
    data <- FELLA.DATA()
    if (!is.null(createUser()) & !is.null(data)) {
      input <- getInput(createUser())
      inputNames <- getName(data, input)
      inputNames <- sapply(inputNames, function(x) {
        if (length(x)) {
          return(x[[1]])}
        return(NULL)
      }) 
      excluded <- getExcluded(createUser())
      
      return(list("input" = input, 
                  "inputNames" = inputNames, 
                  "excluded" = excluded))
    }
  })
  
  # Downloadable example file 
  output$downloadExample <- downloadHandler(
    filename = function() {
      "example2.txt"
    }, 
    content = function(file) {
      con <- readLines("examples/input_2_compounds.list")
      write(con, file)
    }, 
    contentType = "text"
  )
  
  # Table showing current compounds
  output$tableCompounds <- renderTable({
    if (!is.null(inputSummary())) {
      outTable <- cbind(inputSummary()$inputNames)
      colnames(outTable) <- "KEGG name"
      return(outTable)
    }
  })
  
  # Table showing excluded compounds
  output$tableExcluded <- renderTable({
    #     browser()
    if (!is.null(inputSummary())) {
      if (length(inputSummary()$excluded) > 0) {
        outTable <- data.frame(inputSummary()$excluded)
        names(outTable) <- "KEGG id"
        return(outTable)
      }
    }
  })
  
  # Generate the graph / graphlist...
  # BUT the current graph is extracted using another function
  # That's because the current graph can vary with the user-selected 
  # GO term, if any
  generateGraph <- reactive({
    usr <- createUser()
    data <- FELLA.DATA()
    if (!is.null(usr) & !is.null(data)) {
      return(
        generateResultsGraph(
          object = usr,
          method = input$method, 
          threshold = input$threshold, 
          plimit = 15, 
          nlimit = input$nlimit, 
          thresholdConnectedComponent = input$thresholdConnectedComponent, 
          LabelLengthAtPlot = input$LabelLengthAtPlot, 
          data = data))
    }
  })
 
  # This function returns the currently chosen graph connected component
  currentGraph <- reactive({
    if (!is.null(createUser())) {
      g <- generateGraph()
      # GO tag?
      orgDb <- input$GOorgDb
      ont <- input$GOOntology
      biomart <- input$GObiomart
      dataset <- input$GOdataset
      if (input$GOTermInput != "" & 
          input$method != "hypergeom" &
          !is.null(orgDb) & 
          !is.null(ont) & 
          !is.null(biomart) & 
          !is.null(dataset)) {
        return(
          addGOToGraph(
            graph = g, 
            GOterm = input$GOTermInput, 
            godata.options = list(
              OrgDb = orgDb, 
              ont = ont
            ), 
            mart.options = list(
              biomart = biomart, 
              dataset = dataset
            )
          )
        )
      }
      return(g)
    }
    return(NULL)
  })
  
  # Tooltip about the current graph size
  output$graphSize <- renderText({
    if (!is.null(createUser())) {
      g <- currentGraph()
      if (!is.null(g))  {
        return(paste0("Number of nodes: ", vcount(g)))
      } else {
        return("Graph is NULL! No significant pathways have been found.")
      }
    }
  })
  # ---------------------------------------------------
  # Table of results
  output$tableSolution <- DT::renderDataTable({
    data <- FELLA.DATA()
    user <- createUser()
    g <- currentGraph()
    if (!is.null(data) & !is.null(g) & is.igraph(g)) {
      if (vcount(g) == 0) return(NULL)
      wholeTable <- generateResultsTable(
        object = user, 
        method = input$method, 
        threshold = input$threshold,
        plimit = 15, 
        nlimit = input$nlimit, 
        LabelLengthAtPlot = 100, 
        data = data)
      
      # Only nodes in graph
      plottedRows <- wholeTable$"KEGG.id" %in% V(g)$name
      outTable <- wholeTable[plottedRows, ]
      rownames(outTable) <- NULL
      
      # Add hyperlinks to KEGG
      outTable$"KEGG.id" <- paste0(
        "<a href=\"http://www.genome.jp/dbget-bin/www_bget?",
        outTable$"KEGG.id", "\"", "\ target=\"_blank", 
        "\">", outTable$"KEGG.id", "</a>")
      escape <- which(colnames(outTable) == "KEGG.id")
      
      DT::datatable(outTable, 
                    escape = escape, 
                    options = list(pageLength = 100)) %>%
        DT::formatSignif(columns = "p.score", digits = 2)
    }
  })
  
  # ---------------------------------------------------
  #  CC example: change default value for the updateTextInput!
  observe({
    if (input$exampleGOCC > 0) {
      updateTextInput(session, "GOTermInput", value = "GO:0005739")
      updateTextInput(session, "GOorgDb", value = "org.Hs.eg.db")
      updateSelectInput(session, "GOOntology", selected = "CC")
      updateTextInput(session, "GObiomart", value = "ensembl")
      updateTextInput(session, "GOdataset", value = "hsapiens_gene_ensembl")
    }
  })
  
  ###########################################################
  # Cytoscape plugin!
  network <- reactive({
    g <- currentGraph()
    if (!is.null(g)) {
      id <- V(g)$name
      label <- V(g)$label
      nodes <- data.frame(id, label, stringsAsFactors = FALSE)
      
      # GO labels?
      if ("GO.simil" %in% list.vertex.attributes(g)) {
        GO.simil <- unlist(V(g)$GO.simil)
        GO.annot <- TRUE
      } else {
        GO.annot <- FALSE
      }
      
      map.com <- c("pathway", "module", "enzyme", "reaction", "compound")
      map.color <- c("#E6A3A3", "#E2D3E2", "#DFC1A3", "#D0E5F2", "#A4D4A4")
      map.labelcolor <- c("#CD0000", "#CD96CD", "#CE6700", "#8DB6CD", "#548B54")
      map.nodeWidth <- c(40, 30, 25, 22, 22)

      nodeShape <- ifelse(
        V(g)$name %in% getInput(createUser()),
        "box",
        "ellipse"
      )
      nodes$group <- map.com[V(g)$com]
      nodes$color <- map.color[V(g)$com]

      # width
      nodes$value <- map.nodeWidth[V(g)$com]
      nodes$shape <- nodeShape
      
      # Change color and label if GO annotations are present
      if (GO.annot) {
        ids <- !is.na(GO.simil)
        GO.semsim <- GO.simil[ids]
        GO.hits <- names(GO.semsim)
        if (!is.null(GO.hits)) {
          newColor <- sapply(
            GO.semsim, 
            function(x) {
              if (x < 0.5) return("#FFD500")
              else if (x < 0.7) return("#FF5500")
              else if (x < 0.9) return("#FF0000")
              return("#B300FF")
            }
          )
          newName <- paste0(nodes$label[ids], "[", GO.hits, "]")
          newShape <- "triangle"
          
          # modify name and color
          nodes$label[ids] <- newName
          nodes$color[ids] <- newColor
          nodes$shape[ids] <- newShape
        }
      }
      
      # tooltip
      nodeLink <- paste0(
        "<a href=\"http://www.genome.jp/dbget-bin/www_bget?",
        V(g)$name, "\"", "\ target=\"_blank", "\">", V(g)$name, "</a>")
      if(vcount(g) == 0) nodeLink <- character(0)

      nodes$title <- nodeLink
      
      source <- V(g)[get.edgelist(g)[, 1]]$name
      target <- V(g)[get.edgelist(g)[, 2]]$name
      edges <- data.frame(source, target, stringsAsFactors = FALSE)
      
      names(edges) <- c("from", "to")

      network <- list(
        nodes = nodes,
        edges = edges)
      
      return(network)
    }
    return(NULL)
  })
  
  output$cytoscapePlot <- renderVisNetwork({
      net <- network()
      if (!is.null(net)) {
          visNetwork(nodes = net$nodes, edges = net$edges) %>%
              visIgraphLayout() %>%
              visEdges(smooth = FALSE) %>% 
              visOptions(
                  selectedBy = "group", 
                  nodesIdSelection = TRUE,
                  highlightNearest = TRUE)
      }
  })
  ###########################################################
  
  output$exportResults_csv <- downloadHandler(
    filename = function() {
      "resultsTable.csv"
    }, 
    content = function(file) {
      data <- FELLA.DATA()
      if (!is.null(data)) {
        exportResults(
          format = "csv", 
          file = file, 
          object = createUser(), 
          method = input$method, 
          threshold = input$threshold,
          nlimit = input$nlimit, 
          plimit = 15, 
          data = data)
      }
      
    }, 
    contentType = "text/csv"
  )
  
  tableEnzymes <- reactive({
    user <- createUser()
    data <- FELLA.DATA()
    if (!is.null(user) & !is.null(data)) {
      generateEnzymesTable(
        object = user, 
        method = input$method, 
        threshold = input$threshold,
        nlimit = input$nlimit, 
        data = data)
    }
  })
  output$exportEnzymes_csv <- downloadHandler(
    filename = function() {
      "enzymesTable.csv"
    }, 
    content = function(file) {
      tab <- tableEnzymes()
      if (!is.null(tab)) {
        write.csv(
          x = tab, 
          file = file, 
          row.names = FALSE
        )
      }
    }, 
    contentType = "text/csv"
  )
  
  output$exportEnzymes_genelist <- downloadHandler(
    filename = function() {
      "genesFromEnzymes.txt"
    }, 
    content = function(file) {
      # browser()
      tab <- tableEnzymes()
      if (!is.null(tab)) {
        writeLines(
          text = unique(unlist(strsplit(tab$Genes, split = ";"))), 
          con = file
        )
      }
    }, 
    contentType = "text"
  )
  
  output$exportigraph <- downloadHandler(
    filename = function() {
      "resultsSubgraph.RData"
    }, 
    content = function(file) {
      data <- FELLA.DATA()
      if (!is.null(data)) {
        exportResults(
          format = "igraph", 
          file = file, 
          object = createUser(), 
          method = input$method, 
          threshold = input$threshold,
          thresholdConnectedComponent = input$thresholdConnectedComponent, 
          nlimit = input$nlimit, 
          plimit = 15, 
          data = data)
      }
    } 
  )
  
  #   output$exportcsv = downloadHandler(
  #     filename = "report.pdf",
  #     
  #     content = function(file) {
  #       out = knit2pdf("report/sample.Rnw")
  #       file.rename(out, file) # move pdf to file for downloading
  #     },
  #     
  #     contentType = "application/pdf"
  #   )  
  
  output$report = downloadHandler(
    filename = "report.pdf",
    
    content = function(file) {
      out = knit2pdf("report/sample.Rnw")
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = "application/pdf"
  )
  
  output$reportPR = downloadHandler(
    filename = "reportPR.pdf",
    
    content = function(file) {
      out = knit2pdf("report/samplePR.Rnw")
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = "application/pdf"
  )
})