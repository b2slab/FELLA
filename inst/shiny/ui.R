# library(shiny)

shinyUI(fluidPage(
  titlePanel(
    title = h1(
      a(href = "http://b2slab.upc.edu/", 
        img(src='b2slab_small.png', align = "center", height = "50px")), 
      "SHELLA: pathway analysis for metabolomics data", 
      a(href = "http://www.creb.upc.edu/", 
        img(src='creb_upc.png', align = "center", height = "50px"), 
        align = "center")),  
    windowTitle = "SHELLA: enriching metabolomics data"
  ),
  
  fluidRow(
    tabsetPanel(
      tabPanel(
        "1. Upload compounds", 
        column(
          12, 
          column(
            3, 
            h2("Overview"), 
            h4("SHELLA is an interface to the FELLA R package. 
               SHELLA allows the user to perform 
               a pathway enrichment on metabolomics data 
               using the KEGG database."), 
            h3("1. Upload compounds"), 
            h5("The user can upload a list of metabolites as a text file 
               containing KEGG ids.  
               Three sample lists show the app functionalities. 
               Once the metabolites are defined,  
               the matches and the mismatches are displayed."), 
            h3("2. Advanced options"), 
            h5("This tab lets the user adjust methodological and 
               graphical parameters. For instance, 
               the algorithm to prioritise the nodes, 
               the thresholds and the maximum amount of nodes 
               to display. Furthermore, the enzymes in the solution can 
               be overlaid with their similarity to a user-defined 
               GO label."), 
            h3("3. Interactive results"), 
            h5("This tab draws the solution and lets the user zoom, move, 
               search and highlight nodes in the solution graph. 
               Below, a table describes the depicted nodes 
               and links to their KEGG entries in their website."), 
            h3("4. Export results"), 
            h5("A variety of output formats are available 
               for downloading the results.")
            ), 
          column(
            3,
            h2("Upload compounds"), 
            radioButtons(
              "radioInput", 
              label = h4("List of metabolites to enrich:"),
              choices = list("Example 1" = 1, 
                             "Example 2" = 2, 
                             "Example MetaboAnalyst" = 3, 
                             "I will upload my compounds" = 4),
              selected = 1),
            
            fileInput("file", label = ""),
            
            br(), 
            h2("Example file"), 
            h5("Here is a sample of the format understood by SHELLA.
               This is the content of the data for 'Example 2'. 
               KEGG identifiers can be quoted as well, but this is not 
               necessary. The file extension is irrelevant (.txt, .csv) 
               as long as the format is correct."), 
            h6("Make sure the KEGG compounds are parsed as expected 
               in the right column. For instance, seek mismatches due to 
               whitespaces."), 
            downloadButton("downloadExample", "Download Example 2"), 
            h5("Contents of the file:"), 
            verbatimTextOutput("exampleInput")
            ),
          
          column(
            5,
            h2("Check the uploaded compounds"), 
            h5("Successfully mapped KEGG ids:"), 
            tableOutput("tableCompounds"), 
            h5("Mismatching compounds:"),
            h6("Note: due to the graph curation, 
               not every KEGG compound is within the SHELLA database,
               Even if the KEGG id exists, 
               a mismatch can take place."), 
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
            4,
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
                "Threshold (p-score) for the nodes in the solution graph"), 
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
            sliderInput(
              "thresholdConnectedComponent", 
              label = h4(
                "Threshold for the size of a connected component to be shown"), 
              min = 0.01, 
              max = 1, 
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
            h3("GO labels for enzymes"), 
            h4("Adding a GO term takes some time, please be patient"),
            h5("The user can add a GO label visual filter for the 
               enzymes in the output. For example, we can decide 
               to filter by 'mitochondrion' by clicking the following button:"),
            actionButton("exampleGOCC", label = "GO: mitochondrion example"), 
            h6("The GO term for each enzyme family is determined by 
               the best semantic similarity using the genes in it.  
               The GO term of the best hit is appended 
               to the node label and the node shape becomes triangular. 
               If GO annotations are unavailable for an enzyme, it is 
               left with a circular shape."),
            #################################
            textInput(
              "GOTermInput", 
              label = h4(
                "Specify a GO term"), 
              value = NULL), 
            h3("GO data options"), 
            textInput(
              "GOorgDb", 
              label = h4("GO orgDb"), 
              value = NULL
            ),
            selectInput(
              "GOOntology", 
              label = h4("GO ontology"), 
              choices = c("CC", "MF", "BP")
            ),
            h3("biomaRt options"), 
            textInput(
              "GObiomart", 
              label = h4(
                "Specify a biomaRt"), 
              value = NULL), 
            textInput(
              "GOdataset", 
              label = h4(
                "Specify a dataset"), 
              value = NULL), 
            
            #################################
            selectInput(
              "GONamesAsLabels", 
              label = h4(
                  "Should GO names used as labels, instead of GO id's?"),
              choices = list("Yes" = TRUE, "No" = FALSE),
              selected = TRUE)
            )
          )
        
      ), 
      #################################
      tabPanel(
        "3. Interactive results", 
        column(
          12,
          textOutput("graphSize", container = h2), 
          textOutput("hoverNode", container = h3), 
          visNetworkOutput(
              "cytoscapePlot", 
              height = "800px"
              # width = "1200px"
              ), 
          DT::dataTableOutput("tableSolution")
        )
        
      ), 
      #################################
      tabPanel(
        "4. Export results", 
        column(
          12, 
          h2("Export tables"),
          h4("Export the whole results table as csv"),
          downloadButton("exportResults_csv", "Download table with results"), 
          h4("Export the enzymes in the solution 
             with related genes and GO terms as csv"), 
          downloadButton(
            "exportEnzymes_csv", "Download table with enzymes"),
          h4("Export the genes (entrez) that belong to 
             the enzyme EC numbers as a text file"), 
          downloadButton(
            "exportEnzymes_genelist", "Download text file with genes"),
          h2("Export graph solution to R igraph"), 
          downloadButton("exportigraph", "Download R igraph solution")
        )
      )
    )
  )
))