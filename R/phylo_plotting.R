#' @title Phylogeny Plotting
#' @description Plot a phylogeny based on either the original or simplified network.
#'
#' @param links The links from the original or simplified network.
#' @param nodes The nodes from the original or simplified network.
#' @param captions The captions that will appear when you hover over each node in the phylogeny.
#' @param iterations The number of iterations used to optimize the sankey plot (default is 25).
#' @param iterations The vertical padding between nodes in the sankey plot (default is 3).
#'
#' @return A D3.js sankey plot that can be saved as an interactive HTML file using \emph{htmlwidgets} or a static PNG file using \emph{r2d3}.
#' @export
#'
#' @examples plot <- phylo_plotting(links = simplified_network$links, nodes = simplified_network$nodes)
phylo_plotting <- function(links, nodes, captions = NULL, iterations = 25, nodePadding = 3){
  #change first colname to group so that function is compatible with unsimplified and simplified network
  colnames(nodes) <- c("group", "individuals")

  #get plot years from nodes
  plot_years <- as.numeric(sort(unique(gsub(".*_", "", nodes$group))))

  #assign colors
  colors <- data.frame(group = as.numeric(unique(gsub("_.*", "", nodes$group))), color = randomcoloR::distinctColorPalette(k = length(as.numeric(unique(gsub("_.*", "", nodes$group))))))
  nodes$color <- as.character(colors$color[match(gsub("_.*", "", nodes$group), colors$group)])
  rm(colors)

  #add x coordinates to node dataframe
  nodes$x <- rep(NA, nrow(nodes))
  for(i in 1:length(plot_years)){
    nodes$x[grep(paste0("_", plot_years[i]), nodes$group)] <- i
  }
  nodes$x <- nodes$x-1 #has to start at zero...

  #add node column and reorder nodes
  nodes$node <- 0:(nrow(nodes)-1)
  nodes <- nodes[,c(5, 1, 2, 3, 4)]
  rownames(nodes) <- NULL

  #replace population with index from node column
  links$from <- nodes$node[match(links$from, nodes$group)]
  links$to <- nodes$node[match(links$to, nodes$group)]

  #remove year from population
  nodes[,1] <- gsub("_.*", "", nodes$group)
  colnames(nodes[,1]) <- "group"

  #plot
  plot <- sankeyD3::sankeyNetwork(Links = links, Nodes = nodes, NodePosX = "x",
                                  Source = "from", Target = "to", Value = "value", NodeID = "group", NodeColor = "color",
                                  fontSize = 10, nodeWidth = 8, iterations = iterations, showNodeValues = FALSE, fontColor = "transparent",
                                  dragX = FALSE, dragY = FALSE, zoom = TRUE, nodeCornerRadius = 2, nodeStrokeWidth = 0.5, nodePadding = nodePadding, curvature = 0.5,
                                  linkType = "path1", xAxisDomain = c(plot_years), align = "none")

  #add captions to nodes
  if(!is.null(captions)){
    plot$x$nodes$caption <- captions
    plot <- htmlwidgets::onRender(plot, 'function(el, x){d3.selectAll(".node").select("title").text(function(d){return d.caption;});}')
  }

  return(plot)
}
