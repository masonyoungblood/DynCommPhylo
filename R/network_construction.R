#' @title Network Construction
#' @description Convert the output of the TILES dynamic community detection algorithm to a network where each node is a community and each link is the number of individuals moving between communities. The R workspace needs to include both the input file and a subdirectory called "output" that has the unzipped graph and strong-communities output files
#'
#' @param file The input file used for TILES, in the format specified by \url{https://github.com/GiulioRossetti/TILES}.
#' @param year The years from which TILES snapshots were collected.
#' @param min_group_size The minimum group size to be included in the network (default is 1).
#' @param min_link_size The minimum link size to be included in the network (default is 1).
#' @param unix_time_origin The origin for the Unix times used in the input file for TILES (defaults to 1970-01-01).
#'
#' @return A list with links and nodes. The links are directed and weighted. The nodes are in the format communityID_year, where a single community can persist across multiple years (e.g. 362_1975 -> 362_1976 -> 362_1977).
#' @export
#'
#' @examples original_network <- network_construction(file = "edges.tsv", years = c(1970:1999))
network_construction <- function(file, years, min_group_size = 1, min_link_size = 1, unix_time_origin = "1970-01-01"){
  cat("Loading TILES input file --- ")

  #import the input file used for TILES, to ensure that there are no communities that persist past their timepoints (documented TILES issue)
  edges <- read.table(file, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  edges$V3 <- substr(as.Date(as.POSIXct(edges$V3, origin = unix_time_origin)), start = 1, stop = 4)
  edges <- data.table::data.table(node1 = edges[,1], node2 = edges[,2], year = edges[,3])

  cat("Starting processing loop --- ")

  #begin processing loop
  for(i in 1:(length(years))){
    #store previous year as l_i_1 (as in l_i-1)
    if(i > 1){
      l_i_1 <- l_i
    }

    #import and process TILES output files
    strong_comm_i <- read.table(file = paste0("output/strong-communities-", years[i]-years[1]), sep = "\t", stringsAsFactors = FALSE)
    graph_i <- read.table(file = paste0("output/graph-", years[i]-years[1]), sep = "\t", stringsAsFactors = FALSE)[,1:2]
    l_i <- data.table::data.table(community = paste0(strong_comm_i[,1], "_", years[i]), individuals = regmatches(strong_comm_i[,2], gregexpr("[[:digit:]]+", strong_comm_i[,2])))

    #expand community membership to peripheral members
    for(j in 1:nrow(l_i)){
      temp <- as.character(unique(unlist(graph_i[which(graph_i[,1] %in% l_i$individuals[[j]] | graph_i[,2] %in% l_i$individuals[[j]]),])))
      if(!identical(sort(l_i$individuals[[j]]), sort(temp))){
        l_i$individuals[[j]] <- temp
      }
      rm(temp)
    }

    #store individuals
    l_i_individuals <- unique(unlist(l_i$individuals))

    #identify individuals that appear outside their timepoints (documented TILES issue) so they can be removed
    l_i_stragglers <- setdiff(l_i_individuals, unique(c(edges[which(edges$year == years[i]),]$node1, edges[which(edges$year == years[i]),]$node2)))

    #get network density for every group in l_i_
    l_i_nd <- c()
    for(j in 1:nrow(l_i)){
      l_i_nd <- c(l_i_nd, nrow(graph_i[which(graph_i[,1] %in% l_i$individuals[[j]] & graph_i[,2] %in% l_i$individuals[[j]]),])/((length(l_i$individuals[[j]])*(length(l_i$individuals[[j]])-1))/2))
    }

    #assign individuals in multiple communities to the community with the highest network density, with ties broken by group size
    sizes <- lengths(l_i$individuals)
    for(j in 1:length(l_i_individuals)){
      matches <- which(sapply(1:nrow(l_i), function(x) l_i_individuals[j] %in% l_i$individuals[[x]])) #which groups contain node
      if(l_i_individuals[j] %in% l_i_stragglers){
        l_i$individuals[matches] <- lapply(1:length(matches), function(x){l_i$individuals[matches][[x]][-which(l_i$individuals[matches][[x]] == l_i_individuals[j])]})
      }
      if(!(l_i_individuals[j] %in% l_i_stragglers)){
        if(length(matches) > 1){
          match <- matches[which(l_i_nd[matches] == max(l_i_nd[matches]))][which.max(sizes[matches[which(l_i_nd[matches] == max(l_i_nd[matches]))]])] #which matching group has highest network density (ties broken by group size)
          to_rm <- matches[which(matches != match)] #store matches to be removed
          l_i$individuals[to_rm] <- lapply(1:length(to_rm), function(x){l_i$individuals[to_rm][[x]][-which(l_i$individuals[to_rm][[x]] == l_i_individuals[j])]}) #remove those matches
          rm(list = c("match", "to_rm"))
        }
      }
      rm("matches")
    }

    #remove groups smaller than the minimum group size
    if(length(which(lengths(l_i$individuals) < min_group_size)) > 0){
      l_i <- l_i[-which(lengths(l_i$individuals) < min_group_size),]
    }

    #clear variables
    rm(list = c("strong_comm_i", "graph_i", "l_i_nd", "l_i_individuals", "l_i_stragglers", "sizes"))

    #store output
    if(i > 1){
      #generate links
      mat <- matrix(0, nrow = nrow(l_i_1), ncol = nrow(l_i))
      rownames(mat) <- l_i_1$community
      colnames(mat) <- l_i$community
      for(j in 1:nrow(l_i_1)){
        for(k in 1:nrow(l_i)){
          mat[j, k] <- length(intersect(l_i_1$individuals[[j]], l_i$individuals[[k]]))
        }
      }

      #reshape matrix to links
      temp_links <- reshape2::melt(mat)
      temp_links[,1] <- as.character(temp_links[,1])
      temp_links[,2] <- as.character(temp_links[,2])

      #remove links smaller than the minimum link size
      temp_links <- temp_links[-which(temp_links$value < min_link_size),]

      #append links and nodes
      if(i == 2){
        links <- temp_links
        nodes <- data.table::rbindlist(list(l_i_1, l_i))
      }
      if(i > 2){
        links <- data.table::rbindlist(list(links, temp_links))
        nodes <- data.table::rbindlist(list(nodes, l_i))
      }

      #clear variables
      rm(list = c("mat", "temp_links"))
    }

    #print year
    cat(paste0(years[i], "..."))
  }

  colnames(links) <- c("from", "to", "value")

  cat(" --- Done!")

  #return links and nodes
  return(list(links = links, nodes = nodes))
}
