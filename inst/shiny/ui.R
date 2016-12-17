# library(shiny)

shinyUI(fluidPage(
  titlePanel(
    title = h1(
      a(href = "http://b2slab.upc.edu/", 
        img(src='b2slab_small.png', align = "center")), 
      "Pathway enrichment from KEGG compounds", 
      a(href = "http://www.creb.upc.edu/", 
        img(src='creb_upc.png', align = "center"), 
        align = "center")),  
    windowTitle = "SHELLA: enrichment tool for Metabolomics"
  ),
  
  
  
  fluidRow(
    tabsetPanel(
      tabPanel(
        "1. Upload your compounds", 
        column(
          12, 
          column(
            3, 
            h2("Overview and instructions"), 
            h5("SHELLA online tool allows you to perform 
               a pathway enrichment for Metabolomics using KEGG database."), 
            h3("1. Upload your compounds"), 
            h5("You can upload your data in terms of affected 
               compounds, alternatively you can use our sample data. 
               Once your compounds are uplodaded, you will be shown 
               the matches and the mismatches (excluded compounds)."), 
            h3("2. Advanced options"), 
            h5("This tab allows you to adjust methodological and 
               graphical parameters. You can specify your algorithm 
               of preference, the thresholds and the maximum nodes 
               allowed in the solution graph. You can also apply 
               a visual filter using GO cellular component 
               for the enzymes in the solution."), 
            h3("3. Examine your results"), 
            h5("This tab depicts your solution and lets you jump 
               though the connected components of the solution graph. 
               In addition, a table gathers the represented nodes 
               along with additional data."), 
            h3("4. BiMS viewer"), 
            h5("This page contains the BiMS viewer, our self-made tool 
               for iteractively visualising your results."), 
            h3("5. R-cytoscape viewer"), 
            h5("This tab integrates the r-cytoscapejs visual tool 
               for network analysis."), 
            h3("6. Export your results"), 
            h5("A variety of output formats is available 
               for downloading your results.")
            ), 
          column(
            3,
            h2("Upload your compounds"), 
            radioButtons(
              "radioInput", 
              label = h4("Define your significant compounds:"),
              choices = list("Example 1" = 1, 
                             "Example 2" = 2, 
                             "Example MetaboAnalyst" = 3, 
                             "I will upload my compounds" = 4),
              selected = 1),
            
            fileInput("file", label = ""),
            
            br(), 
            h2("Example file"), 
            h5("If you are willing to upload your results, here is the format 
               that you should use in your file."), 
            h4("This is the content of the data for 'Example 2'. 
               KEGG identifiers can be quoted as well, but this is not 
               necessary. The file extension is irrelevant (.txt, .csv) 
               as long as the format is correct. 
               Make sure your compounds are parsed as expected."),  
            verbatimTextOutput("exampleInput"), 
            downloadButton("downloadExample", "Download Example 2")
            ),
          
          column(
            5,
            h2("Check your uploaded compounds"), 
            h5("Remember to place your compounds as a column in the csv file."), 
            h6("The mapped compounds should appear below:"), 
            tableOutput("tableCompounds"), 
            h6("Excluded compounds:"),
            tableOutput("tableExcluded") 
          )
        )
      ), 
      tabPanel(
        "2. Advanced options", 
        column(
          12, 
          h2("Adjust arguments (optional)"), 
          column(
            3,
            h3("The database"), 
            selectInput(
              "database", 
              label = h4("Local databases choice"),
              choices = pathData), 
            verbatimTextOutput("databaseInfo"), 
            h3("The method"), 
            #################################
            selectInput(
              "method", 
              label = h4("Method choice"),
              choices = list("Hypergeometric test" = "hypergeom", 
                             "Heat diffusion" = "diffusion", 
                             "PageRank" = "pagerank"),
              selected = "diffusion"), 
            
            #################################
            selectInput(
              "approx", 
              label = h4("P-values approximation"),
              choices = list("Normality" = "normality", 
                             "Simulation (slower)" = "simulation"),
              selected = "normality"), 
            sliderInput(
              "niter", 
              label = h4(
                "Number of iterations (if simulation)"), 
              min = 5000, 
              max = 15000, 
              step = 1000,
              value = 10000)
          ), 
          column(
            4,
            h3("Graphical parameters"), 
            #################################
            sliderInput(
              "threshold", 
              label = h4(
                "Threshold (fdr) for the nodes in the solution graph"), 
              min = 0.005, 
              max = 0.2, 
              step = 0.005,
              value = 0.05), 
            #################################
            sliderInput(
              "nlimit", 
              label = h4(
                "Limit for the number of nodes in the solution graph"), 
              min = 40, 
              max = 300, 
              step = 5, 
              value = 250), 
            #################################
            selectInput(
              "splitByConnectedComponent", 
              label = h4(
                "Split solution graph by connected components?"),
              choices = list("Yes" = TRUE, 
                             "No" = FALSE),
              selected = FALSE), 
            #################################
            sliderInput(
              "thresholdConnectedComponent", 
              label = h4(
                "Threshold for the size of a connected component to be shown"), 
              min = 0.001, 
              max = 0.1, 
              step = 0.001,
              value = 0.05), 
            #################################
            sliderInput(
              "LabelLengthAtPlot", 
              label = h4("Limit for the label length in the plot"), 
              min = 10, 
              max = 100, 
              step = 1, 
              value = 50)
            #################################
          ), 
          column(
            4,
            h3("GO Cellullar Component for enzymes"), 
            h5("You can add a cellular component filter for the 
               enzymes in your output. For example, if you decide 
               to filter by 'mitochondrion', use the example GO label. 
               For each enzyme family, its GO term will be determined by 
               the best semantic similarity using the genes in that family. 
               The GO cellular component of the best hit will be appended 
               to the node label and the node shape will become triangular. 
               If GO annotations are unavailable for an enzyme, it will 
               be left with a circular shape."), 
            #################################
            textInput(
              "GO.CellularComponent", 
              label = h4(
                "Specify a GO term for Cellullar component 
                (optional and slower)"), 
              value = NULL), 
            actionButton("exampleGOCC", label = "GO CC example"), 
            #################################
            selectInput(
              "GONamesAsLabels", 
              label = h4(
                "Should GO names used as labels, instead of GO id's?"),
              choices = list("Yes" = TRUE, 
                             "No" = FALSE),
              selected = TRUE)
            
            )
          )
        
      ), 
      #################################
      tabPanel(
        "3. Examine your results", 
        textOutput("graphSize", container = h2), 
        textOutput("hoverNode", container = h3), 
        selectInput(
          "selectGraphCC", 
          label = "Select connected component to visualise:", 
          choices = list("No graph available" =  1), 
          selected = 1), 
        plotOutput(
          "plotSolution", 
          click = "clickSolution",
          hover = "hoverSolution", 
          width = "1200px", 
          height = "1200px"), 
        tableOutput("tableSolution")
        
      ), 
      #################################
      tabPanel(
        "4. BIMS Viewer", 
        h4("Under construction")
        # includeHTML("BiMS/index.html")
        # includeHTML(pathBIMS)
        
      ), 
      #################################
      tabPanel(
        "5. R-cytoscape Viewer", 
        rcytoscapejsOutput(
          "cytoscapePlot", 
          height = "1200px", 
          width = "1600px")
        
      ), 
      #################################
      tabPanel(
        "6. Export your results", 
        h2("Export tables"),
        h4("Export the whole results table as csv"),
        downloadButton(
          "exportResults_csv", "Download table with results"), 
        h4("Export the enzymes in the solution with related genes and GO terms as csv"), 
        downloadButton(
          "exportEnzymes_csv", "Download table with enzymes"),
        h4("Export the genes (entrez) that belong to the enzyme EC numbers as a text file"), 
        downloadButton(
          "exportEnzymes_genelist", "Download text file with genes"),
        h2("Export graph solution to R igraph"), 
        downloadButton("exportigraph", "Download R igraph solution")
        # plotOutput(
        # "plot.hypergeom", 
        # clickId="ht.click", 
        # width = "1200px", 
        # height = "1200px"),
        # column(12,
        #        column(6, tableOutput("link.hypergeom")),
        #        column(6, tableOutput("table.hypergeom"))
        # )
        
      )
    )
  )
))