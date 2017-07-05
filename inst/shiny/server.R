# server.R

shinyServer(function(input, output, session) {
  # Database of choice
  FELLA.DATA <- reactive({
    loadKEGGdata(
      databaseDir = input$database, 
      internalDir = FALSE, 
      loadMatrix = "all")
  })
  # database summary
  output$databaseInfo <- renderText({
    data <- FELLA.DATA()
    comment(FELLA:::getGraph(data))
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
          splitByConnectedComponent = input$splitByConnectedComponent, 
          thresholdConnectedComponent = input$thresholdConnectedComponent, 
          LabelLengthAtPlot = input$LabelLengthAtPlot, 
          data = data))
    }
  })
  
  # This function tracks the number of CCs and 
  # lets the user choose with an input text
  #  inputText 'updateSelectInput' changes as the 
  #  size of the graph list does.
  observe({
    if (input$method == "hypergeom") {
      # Hypergeometric test has been chosen
      updateSelectInput(
        session = session, 
        inputId = "selectGraphCC", 
        choices = list("Whole graph (hypergeometric test)" =  1), 
        selected = 1)
    } else {
      if (as.logical(input$splitByConnectedComponent)) {
        #  If the result is split by cc... 
        #  make a new selectInput! With all the cc's.
        sizes <- sapply(generateGraph(), vcount)
        pvalues <- names(generateGraph())
        outputNames <- paste0(sizes, " nodes (p = ", pvalues, ")")
        outputChoice <- as.list(1:length(outputNames))
        names(outputChoice) <- outputNames
        
        updateSelectInput(
            session = session, 
            inputId = "selectGraphCC", 
            choices = outputChoice, 
            selected = 1)
      } else {
        # If it is not split.. there is only one cc...
        updateSelectInput(
            session = session, 
            inputId = "selectGraphCC", 
            choices = list("Whole graph (not split by CC)" =  1), 
            selected = 1)
      }
    }
  })
  
  # This function returns the currently chosen graph connected component
  currentGraph <- reactive({
    if (!is.null(createUser())) {
      if (input$method == "hypergeom") {
        return(generateGraph())
      } else {
        if (!input$splitByConnectedComponent) {
          return(generateGraph())
        } else {
          return(generateGraph()[[as.numeric(input$selectGraphCC)]])
        }
      }
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
  
  # Plot the CURRENT graph! 
  # Reactive function
  plotSolution <- reactive({
    if (!is.null(generateGraph())) {
      if (input$method == "hypergeom") {
        if (is.null(generateGraph()))  {
          return(NULL)
        } else {
          return(FELLA:::plotBipartite(
            graph = currentGraph(), 
            layout = TRUE))
        }
      } else {
        return(FELLA:::plotGraph(
          graph = currentGraph(), 
          input = getInput(createUser()), 
          layout = TRUE, 
          NamesAsLabels = TRUE))
      }
    }
    else return(NULL)
  })
  
  # Table of results
  output$tableSolution <- DT::renderDataTable({
    data <- FELLA.DATA()
    if (!is.null(data)) {
      wholeTable <- generateResultsTable(
        object = createUser(), 
        method = input$method, 
        threshold = input$threshold,
        plimit = 15, 
        nlimit = input$nlimit, 
        LabelLengthAtPlot = 100, 
        data = data)
      
      plottedRows <- wholeTable$"KEGG id" %in% V(currentGraph())$name
      outTable <- wholeTable[plottedRows, ]
      rownames(outTable) <- NULL
      
      DT::datatable(outTable)
    }
  })
  
  # ---------------------------------------------------
  #  CC example: change default value for the updateTextInput!
  observe({
    if (input$exampleGOCC > 0)
      updateTextInput(session, "GOTermInput", value = "GO:0005739")
  })
  
  # SAVE YOUR RESULTS
  output$saveText <- reactive({
    if (input$saveButton) {
      directory <- paste0(getwd(), "/", input$saveName)
      
      dir.create(directory)
      
      pdf(paste0(directory, "/", "test.pdf"), width = 16, height = 16)
      plotPagerank()
      dev.off()
      
      if (!is.null(createUser())) 
        write.csv(
          createUser()@pagerank@pvalues,
          file = paste0(directory, "/table.csv"))
      
      return(paste0("Results saved in directory ", directory))
      
    }
    return("Introduce the directory to save your outputs")
  })
  
  ###########################################################
  # Cytoscape plugin!
  network <- reactive({
    g <- currentGraph()
    if (!is.null(g)) {
      id <- V(g)$name
      name <- V(g)$label
      nodeData <- data.frame(id, name, stringsAsFactors = FALSE)
      
      map.solidColor <- setNames(
        c("#E6A3A3", "#E2D3E2", "#DFC1A3", "#D0E5F2", "#A4D4A4"), 
        1:5
      )
      map.solidLabelColor <- setNames(
        c("#CD0000", "#CD96CD", "#CE6700", "#8DB6CD", "#548B54"), 
        1:5
      )
      map.nodeWidth <- setNames(
        c(40, 30, 25, 22, 22), 
        1:5
      )
      
      nodeShape <- ifelse(
        V(g)$name %in% getInput(createUser()), 
        "rectangle", 
        "ellipse"
      )
      
      nodeData$color <- map.solidColor[V(g)$com]
      nodeData$nodeLabelColor <- map.solidLabelColor[V(g)$com]
      nodeData$width <- map.nodeWidth[V(g)$com]
      nodeData$height <- map.nodeWidth[V(g)$com]
      nodeData$shape <- nodeShape
      
      nodeLink <- paste0(
        "<a href=\"http://www.genome.jp/dbget-bin/www_bget?",
        V(g)$name, "\"", "\ target=\"_blank", "\">", V(g)$name, "</a>")
      
      nodeData$href <- nodeLink
      nodeData$tooltip <- nodeLink
      nodeData$x <- (plotSolution()$x)*600
      nodeData$y <- -(plotSolution()$y)*800
      
      source <- V(g)[get.edgelist(g)[, 1]]$name
      target <- V(g)[get.edgelist(g)[, 2]]$name
      edgeData <- data.frame(source, target, stringsAsFactors = FALSE)
      
      names(edgeData) <- c("source", "target")

      network <- createCytoscapeJsNetwork(
        nodeData = nodeData,
        edgeData = edgeData, 
        edgeSourceShape = "none",
        edgeTargetShape = "none")
      
      return(network)
    }
    return(NULL)
  })
  
  output$cytoscapePlot <- renderRcytoscapejs({
    net <- network()
    if (!is.null(net)) {
        rcytoscapejs(
            nodeEntries = net$nodes, 
            edgeEntries = net$edges, 
            showPanzoom = TRUE, 
            layout = "preset")
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
          nlimit = input$nlimit, 
          plimit = 15, 
          data = data)
      }
      
    } 
    #     , contentType = "text/csv"
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