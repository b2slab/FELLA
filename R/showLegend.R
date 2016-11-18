#' Internal function to add a legend to a graph plot
#' 
#' This function adds a legend to a solution plot. 
#' It can include the CC similarity.
#'
#' @inheritParams .GO.CellularComponent
#' @param cex Numeric value, \code{cex} parameter for the function 
#' \code{\link[graphics]{legend}}
#' 
#' @return This function is only used for its effect, 
#' so it returns \code{invisible()}
#' 
#' @examples 
#' ## This function is internal
#' attach(environment(FELLA:::showLegend))
#' 
#' library(igraph)
#' g <- barabasi.game(20)
#' plot(g)
#' showLegend()
#' plot(g)
#' showLegend(GO.CellularComponent = TRUE)
showLegend <- function(
  GO.CellularComponent = FALSE, 
  cex = 0.75) {
  # In the left side, legend for node categories
  graphics::legend(
#       x = -1.5, y = -1, 
    text.width = .2, 
    "bottomleft", 
    title = "Categories for each node", 
    title.col = "black", 
    legend = c("Pathway", 
               "Module", 
               "Enzyme", 
               "Reaction", 
               "Compound", 
               "Input compound"), 
    pch = c(21, 
            21, 
            21, 
            21, 
            21, 
            22), 
    col = "black", 
    cex = cex, 
    pt.cex = c(1.3,
               1.3, 
               1.3, 
               1.3, 
               1.3, 
               1.3), 
    pt.lwd = 1, 
    x.intersp = .3, 
    box.col = "grey", 
    pt.bg = c("#CD0000", 
              "#CD96CD", 
              "#FFD500", 
              "#8DB6CD", 
              "#548B54", 
              "#548B54"), 
    text.col = c("#CD0000", 
                 "#CD96CD", 
                 "#FFD500", 
                 "#8DB6CD", 
                 "#548B54", 
                 "#548B54"), 
    # horiz = T, 
    ncol = 3
  )
  
  # In the right side, enzyme for cellular component
  if (GO.CellularComponent) {
    graphics::legend(
#         x = 0.5, y = -1,
        text.width = .2, 
      "bottomright", 
      title = "Homo sapiens enzymes with CC similarity", 
      title.col = "black", 
      legend = c("Similarity < 0.5", 
                 "Similarity < 0.7", 
                 "Similarity < 0.9", 
                 "Similarity = 1"), 
      pch = c(24, 
              24, 
              24, 
              24), 
      col = "black", 
      cex = cex, 
      pt.cex = c(1.3, 
                 1.3, 
                 1.3, 
                 1.3), 
      pt.lwd = 1, 
      x.intersp = .3, 
      box.col = "grey", 
      pt.bg = c("#FFD500", 
                "#FF5500", 
                "#FF0000", 
                "#B300FF"), 
      text.col = c("#FFD500", 
                   "#FF5500", 
                   "#FF0000", 
                   "#B300FF"), 
      # horiz = T, 
      ncol = 2
    )
  }
  
  return(invisible())
}