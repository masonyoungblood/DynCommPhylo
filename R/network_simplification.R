#' @title Network Simplification
#' @description Extract the largest connected component of the original network and simplify it using the fast greedy modularity optimization algorithm into a network where each node is a population and each link is the number of individuals moving between populations.
#'
#' @param links The links from the original network.
#' @param nodes The nodes from the original network.
#' @param method The clustering method used for network simplification (defaults to `cluster_fast_greedy`, but also accepts `cluster_louvain`, `cluster_edge_betweenness`, `cluster_walktrap`, `cluster_label_prop`, `cluster_spinglass`, `cluster_leading_eigen`, `cluster_infomap`). See igraph documentation for details.
#'
#' @return A list with links and nodes, as well as the proportion of communities included in the largest connected component and the modularity of the simplified network. The links are directed and weighted. The nodes are in the format populationID_year, where a single population can persist across multiple years (e.g. 24_1975 -> 24_1976 -> 24_1977).
#' @export
#'
network_simplification <- function(links, nodes, method = "cluster_fast_greedy"){
  cat("Extracting largest component --- ")

  #convert links to igraph object
  graph <- as.matrix(links[,1:2])
  graph <- igraph::graph_from_edgelist(graph, directed = FALSE)
  graph <- igraph::set_edge_attr(graph, "value", value = links$value)

  #get single largest component
  component <- igraph::decompose(graph, max.comps = NA)
  component <- component[[which.max(sapply(1:length(component), function(x){igraph::gsize(component[[x]])}))]]

  #store the proportion of network in largest component
  prop_in_component <- length(unique(gsub("_.*", "", names(igraph::V(component)))))/length(unique(gsub("_.*", "", names(igraph::V(graph)))))

  #run chosen clustering algorithm
  if(method == "cluster_fast_greedy"){
    cluster_output <- igraph::cluster_fast_greedy(component, weights = igraph::E(component)$value)
  }
  if(method == "cluster_louvain"){
    cluster_output <- igraph::cluster_louvain(component, weights = igraph::E(component)$value)
  }
  if(method == "cluster_edge_betweenness"){
    cluster_output <- igraph::cluster_edge_betweenness(component, weights = igraph::E(component)$value)
  }
  if(method == "cluster_walktrap"){
    cluster_output <- igraph::cluster_walktrap(component, weights = igraph::E(component)$value)
  }
  if(method == "cluster_label_prop"){
    cluster_output <- igraph::cluster_label_prop(component, weights = igraph::E(component)$value)
  }
  if(method == "cluster_spinglass"){
    cluster_output <- igraph::cluster_spinglass(component, weights = igraph::E(component)$value)
  }
  if(method == "cluster_leading_eigen"){
    cluster_output <- igraph::cluster_leading_eigen(component, weights = igraph::E(component)$value)
  }
  if(method == "cluster_infomap"){
    cluster_output <- igraph::cluster_infomap(component, e.weights = igraph::E(component)$value)
  }

  #store modularity
  modularity <- igraph::modularity(cluster_output)

  #create populations object
  populations <- data.table::data.table(community = character(), population = character(), individuals = list())

  cat("Reassigning memberships --- ")

  #assign each year represented within each cluster as a unique membership
  for(i in 1:length(cluster_output)){
    community <- cluster_output[[i]]
    community_years <- sort(unique(as.numeric(gsub(".*_", "", community))))

    #collect data from all communities from each year
    for(j in 1:length(community_years)){
      sub_communities <- community[which(as.numeric(gsub(".*_", "", community)) == community_years[j])]
      populations <- data.table::rbindlist(list(populations, data.table::data.table(community = sub_communities, population = rep(paste0(i, "_", community_years[j]), length(sub_communities)), individuals = rep(list(unique(unlist(nodes$individuals[which(nodes$community %in% sub_communities)]))), length(sub_communities)))))
    }
    rm(list = c("community", "community_years", "sub_communities"))
  }

  populations <- populations[match(igraph::V(component)$name, populations$community),] #reorder based on component
  igraph::V(component)$name <- populations$population #replace community with population in component
  populations$id <- match(populations$population, unique(unlist(populations$population))) #create dummy variable for each iteration of each population

  cat("Simplifying network --- ")

  #contract vertices and simplify
  simplified <- igraph::contract.vertices(component, populations$id)
  simplified <- igraph::simplify(simplified, remove.loops = TRUE, remove.multiple = TRUE, edge.attr.comb = list(value = "sum"))
  igraph::V(simplified)$name <- sapply(1:length(igraph::V(simplified)), function(x){unique(igraph::V(simplified)$name[[x]])})
  links <- as.data.frame(igraph::as_edgelist(simplified), stringsAsFactors = FALSE)
  links[,1] <- unlist(links[,1])
  links[,2] <- unlist(links[,2])

  #make sure nodes are in directed order
  for(i in 1:nrow(links)){
    if(gsub(".*_", "", links[i,1]) > gsub(".*_", "", links[i,2])){
      links[i, ] <- links[i, 2:1]
    }
  }

  #add value to links
  links$value <- as.numeric(igraph::get.edge.attribute(simplified, "value"))
  rownames(links) <- NULL
  colnames(links) <- c("from", "to", "value")

  #restore nodes and eliminate duplicates
  nodes <- populations[-which(duplicated(populations$population)), 2:3]

  cat("Done!")

  return(list(links = links, nodes = nodes, prop_in_component = prop_in_component, modularity = modularity))
}
