#' Internal function to add a legend to a graph plot
#' 
#' This function adds a legend to a solution plot. 
#' It can include the CC similarity.
#'
#' @param GO.annot Logical, should GO annotations be included?
#' @param cex Numeric value, \code{cex} parameter for the function 
#' \code{\link[graphics]{legend}}
#' 
#' @return This function is only used for its effect, 
#' so it returns \code{invisible()}
#' 
#' @examples 
#' ## This function is internal
#' 
#' library(igraph)
#' g <- barabasi.game(20)
#' plot(g)
#' FELLA:::plotLegend()
#' plot(g)
#' FELLA:::plotLegend(GO.annot = TRUE)
plotLegend <- function(
    GO.annot = FALSE, 
    cex = 0.75) {
    # In the left side, legend for node categories
    graphics::legend(
        text.width = .25, 
        "bottomleft", 
        title = "Categories for each node", 
        title.col = "black", 
        legend = c(
            "Pathway", 
            "Module", 
            "Enzyme", 
            "Reaction", 
            "Compound", 
            "Input compound"), 
        pch = c(21, 21, 21, 21, 21, 22), 
        col = "black", 
        cex = cex, 
        pt.cex = c(1.1, 1.1, 1.1, 1.1, 1.1, 1.1), 
        pt.lwd = 1, 
        x.intersp = .3, 
        box.col = "grey", 
        pt.bg = c(
            "#CD0000", 
            "#CD96CD", 
            "#FFD500", 
            "#8DB6CD", 
            "#548B54", 
            "#548B54"), 
        text.col = c(
            "#CD0000", 
            "#CD96CD", 
            "#FFD500", 
            "#8DB6CD", 
            "#548B54", 
            "#548B54"), 
        ncol = 3
    )
    
    # In the right side, enzyme for cellular component
    if (GO.annot) {
        graphics::legend(
            text.width = .2, 
            "bottomright", 
            title = "Enzymes with CC similarity", 
            title.col = "black", 
            legend = c(
                "Simil < 0.5", 
                "Simil < 0.7", 
                "Simil < 0.9", 
                "Simil <= 1"), 
            pch = c(24, 24, 24, 24), 
            col = "black", 
            cex = cex, 
            pt.cex = c(1.1, 1.1, 1.1, 1.1), 
            pt.lwd = 1, 
            x.intersp = .3, 
            box.col = "grey", 
            pt.bg = c(
                "#FFD500", 
                "#FF5500", 
                "#FF0000", 
                "#B300FF"), 
            text.col = c(
                "#FFD500", 
                "#FF5500", 
                "#FF0000", 
                "#B300FF"), 
            ncol = 2
        )
    }
    
    return(invisible())
}